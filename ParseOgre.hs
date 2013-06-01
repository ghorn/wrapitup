{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}

module ParseOgre ( main
                 , CGWriterOut(..)
                 ) where


--import Control.Monad ( unless )
import Control.Monad.State.Lazy -- ( WriterT )
--import Control.Monad.IO.Class ( liftIO )
import Clang ( ClangApp )
--import Clang.Cursor ( Cursor, CursorKind(..) )
import Clang.Cursor ( CursorKind(..) )
--import qualified Clang.Type as CT
import Clang.TranslationUnit ( TranslationUnit, TranslationUnitFlags(..),
                               withCreateIndex, withParse )
--import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Char ( toLower )
import Data.Either ( partitionEithers )
--oimport Data.List ( partition )
import Data.Maybe ( mapMaybe )
import Data.Map ( Map )
import qualified Data.Map as M
import Language.Haskell.TH

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
                               , cgHaskell :: [String]
                               }
cgEmpty :: CGWriterOut
cgEmpty = CGWriterOut M.empty M.empty [] []

type CGWriter s a = StateT CGWriterOut (ClangApp s) a

unhandledCe :: MonadState CGWriterOut m => CursorKind -> Cursor' -> m ()
unhandledCe ck c = do
  cg <- get
  put $ cg {cgUnhandledCe = M.insertWith (++) (show ck) [c] (cgUnhandledCe cg)}

unhandledTle :: MonadState CGWriterOut m => CursorKind -> Cursor' -> m ()
unhandledTle ck c = do
  cg <- get
  put $ cg {cgUnhandledTle = M.insertWith (++) (show ck) [c] (cgUnhandledTle cg)}

writeHaskell :: MonadState CGWriterOut m => String -> m ()
writeHaskell x = do
  cg <- get
  put $ cg {cgHaskell = cgHaskell cg ++ [x]}

toNiceEnum :: EnumDecl -> [(String, Integer)]
toNiceEnum (EnumDecl name loc _ fields) = case partitionEithers fields of
  ([], xyzs) -> map (\(x,_,z) -> (x,z)) xyzs
  _ -> error $ "toNiceEnum: got not-nice enum: " ++ name ++ ", " ++ loc

writeEnumDecl :: (MonadIO m, MonadState CGWriterOut m) => EnumDecl -> m ()
writeEnumDecl ed@(EnumDecl name loc _ _) = do
  let elems = toNiceEnum ed
      cons = map (flip normalC [] . mkName . fst) elems
  hsEnum <- liftIO $ runQ $ dataD (cxt []) (mkName name) [] cons []
  hsEnumInstance <- liftIO $ runQ $ mkEnumInstance name elems

  writeHaskell $ "-- EnumDecl: " ++ name ++ " " ++ loc
  writeHaskell (pprint $ hsEnum)
  writeHaskell (pprint $ hsEnumInstance)
  writeHaskell ""

lowerCase :: String -> String
lowerCase [] = []
lowerCase (x:xs) = toLower x:xs

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

writeNonStaticClassMethod :: MonadIO m => String -> String -> String -> String -> [String] -> m b
writeNonStaticClassMethod  = do
  error "writeNonStaticClassMethod unimplemented"

writeStaticClassMethod
  :: MonadState CGWriterOut m =>
     String -> String -> String -> String -> String -> [String] -> m ()
writeStaticClassMethod retTypeSp' cContext cppContext name loc ats = do
  let args = insertCommas ats
      argsWithNames = insertCommasAndNames ats
      proto args' = retTypeSp' ++ " " ++ cContext ++ name ++ "(" ++ args' ++ ")"
      ret = init $ unlines $
            [ "// static class method"
            , "// " ++ loc
            , "extern \"C\" " ++ proto args ++ ";"
            , proto argsWithNames ++ " {"
            , "    " ++ "return " ++ cppContext ++ name ++ "(" ++ argsWithNames ++ ");"
            , "}"
            ]
  writeHaskell ret

writeClassMethod
  :: (MonadTrans t, MonadState CGWriterOut (t (ClangApp s))) =>
     Method -> t (ClangApp s) ()
writeClassMethod m = do

  let cppContext = concatMap (++ "::") (mContext m)
      cContext =  concatMap (++ "_") (mContext m)
      --hsContext = lowerCase $ concatMap (++ "_") (mContext m)

  when (mVirtual m) $ error "how to write virtual method???"
  if (mStatic m)
    then writeStaticClassMethod (mRetType m) cContext cppContext (mName m) (mLoc m) (mArgTypes m)
    else undefined

writeVarDecl c = do
  liftIO $ putStrLn $ "----------- writing var decl --------------"
  liftIO $ print c
  undefined

writeClassElem :: ClassElem -> CGWriter s ()
--  liftIO $ putStrLn c
writeClassElem (Class_Method x) = writeClassMethod x
writeClassElem (Class_TypedefDecl c) = unhandledCe Cursor_TypedefDecl c
writeClassElem (Class_VarDecl c) = writeVarDecl c
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


mkEnumInstance :: String -> [(String, Integer)] -> DecQ
mkEnumInstance name fields = ins
  where
    fe = funD (mkName "fromEnum") (map toClause fields)
    toClause (nm,j) = clause [conP (mkName nm) []] (normalB (litE (integerL j))) []

    te = funD (mkName "toEnum") $ map toClause' fields ++ [wild]
    toClause' (nm,j) = clause [litP (integerL j)] (normalB (conE (mkName nm))) []
    k = mkName "k"
    wild = clause [varP k]
           (normalB [| error $ "toEnum " ++ name ++
                       " got unhandled number: "++ show $(varE k) |]) []

    ins = instanceD (cxt []) [t| Enum $(conT (mkName name)) |] [fe,te]



writeCode :: [TopLevelElem] -> CGWriter s ()
writeCode tls = do
  -- filter out everything but Ogre
  let f (TopLevel_Namespace ns@(Namespace c _ _))
        | cDisplayName c == "Ogre" = Just ns
        | otherwise = Nothing
      f _ = Nothing
      ogreNamespaces = mapMaybe f tls

  mapM_ writeNamespace ogreNamespaces
