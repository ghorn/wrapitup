{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module ParseOgre ( main
                 , CGWriterOut(..)
                 , Binding(..)
                 , writeHaskell'
                 , writeC'
                 , writeBinding'
                 ) where

--import Control.Monad ( unless )
import Control.Monad.State.Lazy
--import Control.Monad.IO.Class ( liftIO )
import Clang ( ClangApp )
--import Clang.Cursor ( Cursor, CursorKind(..) )
import Clang.Cursor ( CursorKind(..) )
import qualified Clang as C
import qualified Clang.Type as CT
import Clang.TranslationUnit ( TranslationUnit, TranslationUnitFlags(..),
                               withCreateIndex, withParse )
--import Data.Char ( toLower )
import Data.Either ( partitionEithers )
import Data.Maybe ( mapMaybe )
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Language.Haskell.Syntax
import Language.Haskell.Pretty
--import Language.Haskell.Parser

import ParseHeaders

main :: IO ()
main = do
--  let filepath = "/usr/include/OGRE/OgreBillboardChain.h"
--  let filepath = "/usr/include/OGRE/Ogre.h"
  let filepath = "/usr/include/OGRE/OgreAnimationState.h"
      --args = ["-x","c++"]
      args = ["-x","c++","-I/usr/local/llvm/lib/clang/3.4/include"] -- ++ ["-D__STDC_CONSTANT_MACROS", "-D__STDC_LIMIT_MACROS","-c"]
      --args = ["-x","c++","-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include","-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed"]
  cgOut <- myParseHeaders filepath args
  mapM_ putStrLn (M.keys $ cgUnhandledCe cgOut)
  mapM_ putStrLn (M.keys $ cgUnhandledTle cgOut)
  return ()

callWriteCode :: TranslationUnit -> ClangApp s CGWriterOut
callWriteCode tu = do
  (uh, tl) <- parseTopLevel tu
  unless (null uh) $ error "top level unhandled dammit"
  execStateT (writeCode tl) cgEmpty

myParseHeaders :: String -> [String] -> IO CGWriterOut
myParseHeaders filepath args = do
  withCreateIndex False False $ \index ->
    withParse index (Just filepath) args [] [TranslationUnit_None] callWriteCode (error "No TXUnit!")

-------------------------------------------------------------------
data CGWriterOut = CGWriterOut { cgUnhandledCe :: Map String [Cursor']
                               , cgUnhandledTle :: Map String [Cursor']
                               , cgC :: [String]
                               , cgHaskell :: [[String]]
                               , cgBinding :: [Binding]
                               }
cgEmpty :: CGWriterOut
cgEmpty = CGWriterOut M.empty M.empty [] [] []

data Binding = Binding { bCWrapper :: String
                       , bHSWrapper :: [String]
                       , bLoc :: String
                       }
type CGWriter s a = StateT CGWriterOut (ClangApp s) a

unhandledCe :: MonadState CGWriterOut m => CursorKind -> Cursor' -> m ()
unhandledCe ck c = do
  cg <- get
  put $ cg {cgUnhandledCe = M.insertWith (++) (show ck) [c] (cgUnhandledCe cg)}

unhandledTle :: MonadState CGWriterOut m => CursorKind -> Cursor' -> m ()
unhandledTle ck c = do
  cg <- get
  put $ cg {cgUnhandledTle = M.insertWith (++) (show ck) [c] (cgUnhandledTle cg)}

writeHaskell :: MonadState CGWriterOut m => [String] -> m ()
writeHaskell x = do
  cg <- get
  put $ cg {cgHaskell = cgHaskell cg ++ [x]}

writeHaskell' :: (MonadIO m, MonadState CGWriterOut m) => [String] -> m ()
writeHaskell' x = do
  liftIO $ mapM_ putStrLn x
  writeHaskell x

writeC :: MonadState CGWriterOut m => String -> m ()
writeC x = do
  cg <- get
  put $ cg {cgC = cgC cg ++ [x]}

writeC' :: (MonadIO m, MonadState CGWriterOut m) => String -> m ()
writeC' x = do
  liftIO $ putStrLn x
  writeC x

writeBinding :: MonadState CGWriterOut m => String -> [String] -> String -> m ()
writeBinding x y loc = do
  cg <- get
  put $ cg {cgBinding = cgBinding cg ++ [Binding x y loc]}

writeBinding' :: (MonadIO m, MonadState CGWriterOut m) => String -> [String] -> String -> m ()
writeBinding' x y loc = do
  liftIO $ putStrLn $ "----------- " ++ loc ++ " -----------"
  liftIO $ putStrLn x
  liftIO $ putStrLn "-------------------------------------"
  liftIO $ mapM_ putStrLn y
  liftIO $ putStrLn "-------------------------------------"
  writeBinding x y loc

toNiceEnum :: EnumDecl -> [(String, Integer)]
toNiceEnum (EnumDecl name loc _ fields) = case partitionEithers fields of
  ([], xyzs) -> map (\(x,_,z) -> (x,z)) xyzs
  _ -> error $ "toNiceEnum: got not-nice enum: " ++ name ++ ", " ++ loc

strip :: String -> String
strip = T.unpack . T.strip . T.pack

src0 :: SrcLoc
src0 = SrcLoc {srcFilename = "<unknown>", srcLine = 1, srcColumn = 1}

makeEnumDecl :: String -> [String] -> String
makeEnumDecl name fields =
  strip $ prettyPrint $
  HsDataDecl src0 [] (HsIdent name) [] (map (\f -> HsConDecl src0 (HsIdent f) []) fields) $
  [UnQual (HsIdent "Show"),UnQual (HsIdent "Eq")]

makeEnumInstance :: String -> [(String, Integer)] -> String
makeEnumInstance name elems =
  strip $ prettyPrint $
  HsInstDecl src0 [] (UnQual (HsIdent "Enum")) [HsTyCon (UnQual (HsIdent name))]
  [HsFunBind (map f elems)
  ,HsFunBind $ map g elems ++ [err]
  ]
  where
    f (fld,k) = HsMatch src0 (HsIdent "fromEnum") [HsPParen (HsPApp (UnQual (HsIdent fld)) [])] (HsUnGuardedRhs (HsLit (HsInt k))) []
    g (fld,k) = HsMatch src0 (HsIdent "toEnum") [HsPLit (HsInt k)] (HsUnGuardedRhs (HsCon (UnQual (HsIdent fld)))) []
    err = HsMatch src0 (HsIdent "toEnum") [HsPVar (HsIdent "k")] (HsUnGuardedRhs (HsInfixApp (HsInfixApp (HsInfixApp (HsVar (UnQual (HsIdent "error"))) (HsQVarOp (UnQual (HsSymbol "$"))) (HsLit (HsString "toEnum got unhandled number "))) (HsQVarOp (UnQual (HsSymbol "++"))) (HsApp (HsVar (UnQual (HsIdent "show"))) (HsVar (UnQual (HsIdent "k"))))) (HsQVarOp (UnQual (HsSymbol "++"))) (HsLit (HsString ("for type "++name))))) []

writeEnumDecl :: (MonadIO m, MonadState CGWriterOut m) => EnumDecl -> m ()
writeEnumDecl ed@(EnumDecl name loc _ _) =
  writeHaskell [ "-- EnumDecl: " ++ name ++ " " ++ loc
               , hsEnum
               , hsEnumInstance
               , ""
               ]
  where
    elems = toNiceEnum ed
    hsEnum = makeEnumDecl name (map fst elems)
    hsEnumInstance = makeEnumInstance name elems



insertCommas :: [String] -> String
insertCommas [] = ""
insertCommas [x] = x
insertCommas (x:xs) = x ++ ", " ++ insertCommas xs

insertCommasAndArgs' :: Int -> [String] -> String
insertCommasAndArgs' _ [] = ""
insertCommasAndArgs' k [x] = x ++ " x" ++ show k
insertCommasAndArgs' k (x:xs) = x ++ " x" ++ show k ++ ", " ++ insertCommasAndArgs' (k+1) xs

insertCommasAndNames :: [String] -> String
insertCommasAndNames = insertCommasAndArgs' 0

writeNonStaticClassMethod
  :: MonadState CGWriterOut m =>
     String -> [String] -> String -> String -> [String] -> m ()
writeNonStaticClassMethod  = do
  error "writeNonStaticClassMethod unimplemented"

writeStaticClassMethod
  :: MonadState CGWriterOut m =>
     String -> [String] -> String -> String -> [String] -> m ()
writeStaticClassMethod retTypeSp' context name loc ats = do
  let args = insertCommas ats
      argsWithNames = insertCommasAndNames ats
      proto args' = retTypeSp' ++ " " ++ cContext context ++ name ++ "(" ++ args' ++ ")"
      ret = init $ unlines $
            [ "// static class method"
            , "// " ++ loc
            , "extern \"C\" " ++ proto args ++ ";"
            , proto argsWithNames ++ " {"
            , "    " ++ "return " ++ cppContext context ++ name ++ "(" ++ argsWithNames ++ ");"
            , "}"
            ]
  writeC ret

cppContext :: [String] -> String
cppContext = concatMap (++ "::")
cContext :: [String] -> String
cContext =  concatMap (++ "_")
--hsContext :: [String] -> String
--hsContext = lowerCase . concatMap (++ "_")
--  where
--    lowerCase [] = []
--    lowerCase (x:xs) = toLower x:xs


writeClassMethod
  :: (MonadTrans t, MonadState CGWriterOut (t (ClangApp s))) =>
     Method -> t (ClangApp s) ()
writeClassMethod m = do
  when (mVirtual m) $ error "how to write virtual method???"
  if (mStatic m)
    then writeStaticClassMethod (mRetType m) (mContext m) (mName m) (mLoc m) (mArgTypes m)
    else writeNonStaticClassMethod (mRetType m) (mContext m) (mName m) (mLoc m) (mArgTypes m)

writeClassVarDecl :: (MonadIO (t (ClangApp s)), MonadTrans t,
                      MonadState CGWriterOut (t (ClangApp s))) =>
                     Cursor' -> t (ClangApp s) ()
writeClassVarDecl cursor' = do
  -- this is assumed to always be static if it comes from a class
  liftIO $ putStrLn $ "----------- writing class var decl --------------"
  let cursor = cCursor cursor'
  cursorType <- lift $ C.getType cursor

  context <- lift $ getContext cursor

  retTypeSp <- lift $ CT.getTypeSpelling cursorType >>= C.unpack

--  cq <- lift $ CT.isConstQualifiedType    cursorType
--  vq <- lift $ CT.isVolatileQualifiedType cursorType
--  rq <- lift $ CT.isRestrictQualifiedType cursorType
--  liftIO $ print (cq,vq,rq)

  name <- lift $ C.getSpelling cursor >>= C.unpack
  let cppname = cppContext context ++ name
      cname   = cContext   context ++ name
      refCname = "ref__" ++ cname
      cSrc spaces = init $ unlines $ map (spaces ++)
             [ "extern \"C\" " ++ retTypeSp ++ " " ++ cname ++ "(void);"
             , retTypeSp ++ " " ++ cname ++ "(void) {"
             , "    return " ++ cppname ++ ";"
             , "}"
             ]
      cRefSrc spaces = init $ unlines $ map (spaces ++)
                [ "extern \"C\" " ++ retTypeSp ++ " * " ++ refCname ++ "(void);"
                , retTypeSp ++ " " ++ refCname ++ "(void) {"
                , "    return &(" ++ cppname ++ ");"
                , "}"
                ]
      c = init $ unlines $
          [ "// " ++ cSpellingLoc cursor'
          , cSrc ""
          , ""
          , cRefSrc ""
          ]

  let hsSrc = HsForeignImport (SrcLoc "<unknown>" 1 1) "ccall" HsUnsafe cname (HsIdent ("c_"++cname)) (HsTyApp (HsTyCon (UnQual (HsIdent "IO"))) (HsTyCon (UnQual (HsIdent "BadType"))))
      hsRefSrc = HsForeignImport (SrcLoc "<unknown>" 1 1) "ccall" HsUnsafe refCname (HsIdent ("c_ref_"++cname)) (HsTyApp (HsTyCon (UnQual (HsIdent "IO"))) (HsTyCon (UnQual (HsIdent "BadName"))))
      hs = [ "-- | " ++ cSpellingLoc cursor'
           , cSrc "-- >  "
           , strip $ prettyPrint hsSrc
           , ""
           , "-- | " ++ cSpellingLoc cursor'
           , cRefSrc "-- >  "
           , strip $ prettyPrint hsRefSrc
           ]
  writeBinding' c hs (cSpellingLoc cursor')


writeClassElem :: ClassElem -> CGWriter s ()
writeClassElem (Class_Method x) = writeClassMethod x
writeClassElem (Class_TypedefDecl c) = unhandledCe Cursor_TypedefDecl c
writeClassElem (Class_VarDecl c) = writeClassVarDecl c
writeClassElem c = error $ "writeClassElem: unhandled " ++ show c

writeTle :: TopLevelElem -> CGWriter s ()
writeTle (TopLevel_TypedefDecl {}) = return ()
writeTle (TopLevel_ClassDecl (ClassDecl _ elems)) = mapM_ writeClassElem elems
writeTle (TopLevel_StructDecl (ClassDecl _ elems)) = mapM_ writeClassElem elems
writeTle (TopLevel_EnumDecl ed) = writeEnumDecl ed
writeTle (TopLevel_ClassTemplate c) = unhandledTle Cursor_ClassTemplate c
writeTle (TopLevel_ClassTemplatePartialSpecialization c) =
  unhandledTle Cursor_ClassTemplatePartialSpecialization c
writeTle (TopLevel_FunctionTemplate c) = unhandledTle Cursor_FunctionTemplate c
writeTle x = error $ "unhandled tle: " ++ show x

writeNamespace :: Namespace -> CGWriter s ()
writeNamespace (Namespace _ tles []) = mapM_ writeTle tles
writeNamespace (Namespace c _ xs) = error $ "writeNamespace: " ++ show c ++ "\ngot unhandled top level elements: " ++ show (map fromUtl xs)


writeCode :: [TopLevelElem] -> CGWriter s ()
writeCode tls = do
  -- filter out everything but Ogre
  let f (TopLevel_Namespace ns@(Namespace c _ _))
        | cDisplayName c == "Ogre" = Just ns
        | otherwise = Nothing
      f _ = Nothing
      ogreNamespaces = mapMaybe f tls

  mapM_ writeNamespace ogreNamespaces
