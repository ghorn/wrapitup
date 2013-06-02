{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module ParseOgre ( main
                 , CGWriterOut(..)
                 , Binding(..)
                 , writeHaskell'
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
import Data.Maybe ( mapMaybe, fromMaybe )
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Data.List ( intercalate )
--import Language.Haskell.Parser ( parseModule )

import ParseHeaders

main :: IO ()
main = do
--  let filepath = "/usr/include/OGRE/OgreBillboardChain.h"
  let filepath = "/usr/include/OGRE/Ogre.h"
--  let filepath = "/usr/include/OGRE/OgreVector3.h"
--  let filepath = "/usr/include/OGRE/OgreAnimationState.h"
      --args = ["-x","c++"]
      args = ["-x","c++","-I/usr/local/llvm/lib/clang/3.4/include"] -- ++ ["-D__STDC_CONSTANT_MACROS", "-D__STDC_LIMIT_MACROS","-c"]
      --args = ["-x","c++","-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include","-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed"]
  cgOut <- myParseHeaders filepath args
  length (cgBinding cgOut) `seq`
    putStrLn $ "wrote " ++ show (length (cgBinding cgOut)) ++ " bindings"
  putStrLn "------ unhandled class elements: ------"
  mapM_ (\(k,els) -> putStrLn $ k ++ " (" ++ show (length els) ++ " instances)")
    (M.toList $ cgUnhandledCe cgOut)
  putStrLn "\n------ unhandled top-level elements: ------"
--  mapM_ (\(k,els) -> putStrLn $ k ++ " (" ++ show (length els) ++ " instances)" ++ show els ++ "\n\n")
  mapM_ (\(k,els) -> putStrLn $ k ++ " (" ++ show (length els) ++ " instances)")
    (M.toList $ cgUnhandledTle cgOut)
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
                               , cgHaskell :: [String]
                               , cgBinding :: [Binding]
                               }
cgEmpty :: CGWriterOut
cgEmpty = CGWriterOut M.empty M.empty [] []

data Binding = Binding { bCWrapper :: String
                       , bHSWrapper :: Maybe String
                       , bLoc :: String
                       , bDesc :: String
                       }
showBinding :: Binding -> String
showBinding (Binding csrc' hssrc' loc desc) =
  init $ unlines $
  [ "------------------ " ++ loc ++ " ----------------------"
  , csrc
  , "-------------------------------------------------------"
  , hssrc
  , "-------------------------------------------------------"
  ]
  where csrc = "// " ++ desc ++ "\n// " ++ loc ++ "\n" ++ csrc'
        hssrc = init $ unlines $
                [ "-- | "
                , init $ unlines $ map ("-- >  " ++ ) $ lines csrc
                , fromMaybe "-- unimplemented :(" hssrc'
                ]


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
  put $ cg {cgHaskell = cgHaskell cg ++ [init $ unlines x]}

writeHaskell' :: (MonadIO m, MonadState CGWriterOut m) => [String] -> m ()
writeHaskell' x = do
  liftIO $ mapM_ putStrLn x
  writeHaskell x

writeBinding :: MonadState CGWriterOut m => String -> Maybe String -> String -> String -> m ()
writeBinding x y loc desc = do
  cg <- get
  put $ cg {cgBinding = cgBinding cg ++ [Binding x y loc desc]}

writeBinding' :: (MonadIO m, MonadState CGWriterOut m) => String -> Maybe String -> String -> String -> m ()
writeBinding' x y loc desc = do
  liftIO $ putStrLn $ showBinding (Binding x y loc desc)
  writeBinding x y loc desc

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

insertCommasAndNames' :: Int -> [String] -> String
insertCommasAndNames' _ [] = ""
insertCommasAndNames' k [x] = x ++ " x" ++ show k
insertCommasAndNames' k (x:xs) = x ++ " x" ++ show k ++ ", " ++ insertCommasAndNames' (k+1) xs

insertCommasAndNames :: [String] -> String
insertCommasAndNames = insertCommasAndNames' 0

argNames :: Int -> [String]
argNames n = map (('x':) . show) [0..(n-1)]

writeNonStaticClassMethod
  :: (MonadIO m, MonadState CGWriterOut m) =>
     Bool -> String -> [String] -> String -> String -> [String] -> m ()
writeNonStaticClassMethod virtual retTypeSp' context name loc ats' = do
  let cppType = cppContextNoTrailing context ++ " *"
      ats = cppType:ats'
      argsWithNames = insertCommasAndNames ats
      callThese = intercalate ", " (drop 1 $ argNames (1 + length ats'))
      proto args' = retTypeSp' ++ " " ++ cContext context ++ name ++ "(" ++ args' ++ ")"
      virt = if virtual then "virtual " else ""
      ret = init $ unlines $
            [ "extern \"C\" " ++ proto argsWithNames ++ ";"
            , proto argsWithNames ++ " {"
            , "    " ++ "return x0->" ++ name ++ "(" ++ callThese ++ ");"
            , "}"
            ]
  writeBinding ret Nothing loc (virt ++ "non-static class method")

writeStaticClassMethod
  :: (MonadIO m, MonadState CGWriterOut m) =>
     Bool -> String -> [String] -> String -> String -> [String] -> m ()
writeStaticClassMethod virtual retTypeSp' context name loc ats = do
  let args = insertCommas ats
      argsWithNames = insertCommasAndNames ats
      proto args' = retTypeSp' ++ " " ++ cContext context ++ name ++ "(" ++ args' ++ ")"
      virt = if virtual then "virtual " else ""
      ret = init $ unlines $
            [ "extern \"C\" " ++ proto args ++ ";"
            , proto argsWithNames ++ " {"
            , "    " ++ "return " ++ cppContext context ++ name ++ "(" ++ argsWithNames ++ ");"
            , "}"
            ]
  writeBinding ret Nothing loc (virt ++ "static class method")

-- this is assumed to never be static, VarDecl is the static ones
writeFieldDecl'
  :: (MonadIO m, MonadState CGWriterOut m) =>
     [Char] -> [String] -> [Char] -> String -> m ()
writeFieldDecl' retTypeSp' context name loc = do
  let cppType = cppContextNoTrailing context ++ " *"
      proto = retTypeSp' ++ " " ++ cContext context ++ name ++ "(" ++ cppType ++ " x0)"
      cSrc = init $ unlines $
            [ "extern \"C\" " ++ proto ++ ";"
            , proto ++ " {"
            , "    " ++ "return x0->" ++ name ++ ";"
            , "}"
            ]
      proto' = retTypeSp' ++ " * " ++ cContext context ++ name ++ "(" ++ cppType ++ " x0)"
      cRefSrc = init $ unlines $
            [ "extern \"C\" " ++ proto' ++ ";"
            , proto' ++ " {"
            , "    " ++ "return &(x0->" ++ name ++ ");"
            , "}"
            ]
  writeBinding cSrc Nothing loc "FieldDecl"
  writeBinding cRefSrc Nothing loc "FieldDecl (reference)"

writeFieldDecl
  :: (MonadIO (t (ClangApp s)), MonadTrans t,
      MonadState CGWriterOut (t (ClangApp s))) =>
     Cursor' -> t (ClangApp s) ()
writeFieldDecl cursor' = do
  let cursor = cCursor cursor'
  cursorTypeSp <- lift $ C.getType cursor >>= CT.getTypeSpelling >>= C.unpack

  context <- lift $ getContext cursor

  name <- lift $ C.getSpelling cursor >>= C.unpack
  let loc = cSpellingLoc cursor'

  writeFieldDecl' cursorTypeSp context name loc


cppConstructorContext :: [String] -> String
cppConstructorContext [] = error "cppConstructorContext needs something"
cppConstructorContext xs = intercalate "::" xs

cppContext :: [String] -> String
cppContext = concatMap (++ "::")
cppContextNoTrailing :: [String] -> String
cppContextNoTrailing = intercalate "::"
cContext :: [String] -> String
cContext =  concatMap (++ "_")
--hsContext :: [String] -> String
--hsContext = lowerCase . concatMap (++ "_")
--  where
--    lowerCase [] = []
--    lowerCase (x:xs) = toLower x:xs


writeClassMethod :: (MonadIO m, MonadState CGWriterOut m) => Method -> m ()
writeClassMethod m = do
  if (mStatic m)
    then writeStaticClassMethod (mVirtual m) (mRetType m) (mContext m) (mName m) (mLoc m) (mArgTypes m)
    else writeNonStaticClassMethod (mVirtual m) (mRetType m) (mContext m) (mName m) (mLoc m) (mArgTypes m)

writeClassConstructor :: (MonadIO (t (ClangApp s)), MonadTrans t,
                          MonadState CGWriterOut (t (ClangApp s))) =>
                         Cursor' -> t (ClangApp s) ()
writeClassConstructor cursor' = do
  let cursor = cCursor cursor'
  k <- lift $ C.getNumArguments cursor
  cursorType <- lift $ C.getType cursor
  variadic <- lift $ CT.isFunctionTypeVariadic cursorType
  when variadic $ error "parseMethod: variadic"
  nat <- lift $ CT.getNumArgTypes cursorType
  when (k < 0) $ error "parseMethod: num args < 0"
  when (k /= nat) $ error "parseMethod: num args /= num arg types"
  static  <- lift $ C.isStaticCppMethod cursor
  virtual <- lift $ C.isVirtualCppMethod cursor
  when virtual $ error "how to virtual constructor?"
  when static $ error "how to static constructor?"

  context <- lift $ getContext cursor
  argTypesSp <- lift $ mapM (\j -> CT.getArgType cursorType j >>= CT.getTypeSpelling >>= C.unpack)
                (take k [0..])

--  name <- lift $ C.getSpelling cursor >>= C.unpack
  let contextName = cppConstructorContext context
      retType = contextName ++ " *"
      loc = cSpellingLoc cursor'
      ats = argTypesSp
      args = insertCommas ats
      argsWithNames = insertCommasAndNames ats
      cname = cContext context ++ "new"
      proto args' = retType ++ " " ++ cname ++ "(" ++ args' ++ ")"
      csrc = init $ unlines $
             [ "extern \"C\" " ++ proto args ++ ";"
             , proto argsWithNames ++ " {"
             , "    " ++ "return new " ++ contextName ++ "(" ++ argsWithNames ++ ");"
             , "}"
             ]
      hssrc = Nothing
  writeBinding csrc hssrc loc "constructor"

writeClassDestructor :: (MonadIO (t (ClangApp s)), MonadTrans t,
                          MonadState CGWriterOut (t (ClangApp s))) =>
                         Cursor' -> t (ClangApp s) ()
writeClassDestructor cursor' = do
  let cursor = cCursor cursor'
  k <- lift $ C.getNumArguments cursor
  cursorType <- lift $ C.getType cursor
  variadic <- lift $ CT.isFunctionTypeVariadic cursorType
  when variadic $ error "writeClassDestructor: variadic"
  nat <- lift $ CT.getNumArgTypes cursorType
  when (k < 0) $ error "writeClassDestructor: num args < 0"
  when (k /= nat) $ error "writeClassDestructor: num args /= num arg types"
  when (k /= 0) $ error "writeClassDestructor: num args /= 0"
  static  <- lift $ C.isStaticCppMethod cursor
  virtual <- lift $ C.isVirtualCppMethod cursor
  when static $ error "how to static destructor?"
  let vd = if virtual then "virtual " else ""

  context <- lift $ getContext cursor
--  name <- lift $ C.getSpelling cursor >>= C.unpack
  let contextName = cppConstructorContext context
      argType = contextName ++ " *"
      loc = cSpellingLoc cursor'
      cname = cContext context ++ "delete"
      proto = "void " ++ cname ++ "(" ++ argType ++ " x0)"
      csrc = init $ unlines $
             [ "extern \"C\" " ++ proto ++ ";"
             , proto ++ " {"
             , "    " ++ "delete x0;"
             , "}"
             ]
      hssrc = strip $ prettyPrint $
              HsForeignImport (SrcLoc "<unknown>" 1 1) "ccall" HsUnsafe cname (HsIdent ("c_"++cname)) (HsTyFun (HsTyCon (UnQual (HsIdent "BadType"))) (HsTyApp (HsTyCon (UnQual (HsIdent "IO"))) (HsTyCon (Special HsUnitCon))))
  writeBinding csrc (Just hssrc) loc ("class " ++ vd ++ "destructor")



writeClassVarDecl :: (MonadIO (t (ClangApp s)), MonadTrans t,
                      MonadState CGWriterOut (t (ClangApp s))) =>
                     Cursor' -> t (ClangApp s) ()
writeClassVarDecl cursor' = do
  -- this is assumed to always be static
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
      cSrc = init $ unlines $
             [ "extern \"C\" " ++ retTypeSp ++ " " ++ cname ++ "(void);"
             , retTypeSp ++ " " ++ cname ++ "(void) {"
             , "    return " ++ cppname ++ ";"
             , "}"
             ]
      cRefSrc = init $ unlines $
                [ "extern \"C\" " ++ retTypeSp ++ " * " ++ refCname ++ "(void);"
                , retTypeSp ++ " " ++ refCname ++ "(void) {"
                , "    return &(" ++ cppname ++ ");"
                , "}"
                ]
  let hsSrc = HsForeignImport (SrcLoc "<unknown>" 1 1) "ccall" HsUnsafe cname (HsIdent ("c_"++cname)) (HsTyApp (HsTyCon (UnQual (HsIdent "IO"))) (HsTyCon (UnQual (HsIdent "BadType"))))
      hsRefSrc = HsForeignImport (SrcLoc "<unknown>" 1 1) "ccall" HsUnsafe refCname (HsIdent ("c_ref_"++cname)) (HsTyApp (HsTyCon (UnQual (HsIdent "IO"))) (HsTyCon (UnQual (HsIdent "BadName"))))
      loc = cSpellingLoc cursor'
  writeBinding cSrc (Just $ strip $ prettyPrint hsSrc) loc "class VarDecl"
  writeBinding cRefSrc (Just $ strip $ prettyPrint hsRefSrc) loc "class VarDecl (reference)"

writeClassElem :: ClassElem -> CGWriter s ()
writeClassElem (Class_Method x) = writeClassMethod x
writeClassElem (Class_VarDecl c) = writeClassVarDecl c
writeClassElem (Class_Constructor c) = writeClassConstructor c
writeClassElem (Class_Destructor c) = writeClassDestructor c
writeClassElem (Class_FieldDecl c) = writeFieldDecl c
writeClassElem (Class_ClassDecl c) = writeClassDecl c
writeClassElem (Class_StructDecl c) = writeClassDecl c
writeClassElem (Class_EnumDecl x) = writeEnumDecl x
writeClassElem (Class_FunctionTemplate c)   = unhandledCe Cursor_FunctionTemplate c
writeClassElem (Class_CXXBaseSpecifier c)   = unhandledCe Cursor_CXXBaseSpecifier c
writeClassElem (Class_CXXBoolLiteralExpr c) = unhandledCe Cursor_CXXBoolLiteralExpr c
writeClassElem (Class_UsingDeclaration c)   = unhandledCe Cursor_UsingDeclaration c
writeClassElem (Class_TypedefDecl c)        = unhandledCe Cursor_TypedefDecl c
writeClassElem (Class_UnexposedDecl c)      = unhandledCe Cursor_UnexposedDecl c
writeClassElem (Class_ConversionFunction c) = unhandledCe Cursor_ConversionFunction c
writeClassElem (Class_UnionDecl c)          = unhandledCe Cursor_UnionDecl c
writeClassElem (Class_TypeRef c)            = unhandledCe Cursor_TypeRef c
writeClassElem (Class_ClassTemplate c)      = unhandledCe Cursor_ClassTemplate c
writeClassElem (Class_FirstAttr c)          = unhandledCe Cursor_FirstAttr c
writeClassElem (Class_IntegerLiteral c)     = unhandledCe Cursor_IntegerLiteral c
writeClassElem (Class_TemplateRef c)        = unhandledCe Cursor_TemplateRef c
writeClassElem (Class_NamespaceRef c)       = unhandledCe Cursor_NamespaceRef c
writeClassElem (Class_UnaryOperator c)      = unhandledCe Cursor_UnaryOperator c

writeClassDecl :: ClassDecl -> StateT CGWriterOut (ClangApp s) ()
writeClassDecl (ClassDecl _ elems) = mapM_ writeClassElem elems

writeTle :: TopLevelElem -> CGWriter s ()
writeTle (TopLevel_TypedefDecl {}) = return ()
writeTle (TopLevel_ClassDecl c) = writeClassDecl c
writeTle (TopLevel_StructDecl c) = writeClassDecl c
writeTle (TopLevel_EnumDecl ed) = writeEnumDecl ed
writeTle (TopLevel_Constructor c) = writeClassConstructor c
writeTle (TopLevel_Namespace c) = writeNamespace c
writeTle (TopLevel_UnexposedDecl c)      = unhandledTle Cursor_UnexposedDecl c
writeTle (TopLevel_FirstAttr c)          = unhandledTle Cursor_FirstAttr c
writeTle (TopLevel_Destructor c)         = unhandledTle Cursor_Destructor c
writeTle (TopLevel_UnionDecl c)          = unhandledTle Cursor_UnionDecl c
writeTle (TopLevel_UsingDeclaration c)   = unhandledTle Cursor_UsingDeclaration c
writeTle (TopLevel_VarDecl c)            = unhandledTle Cursor_VarDecl c
writeTle (TopLevel_FunctionDecl c)       = unhandledTle Cursor_FunctionDecl c
writeTle (TopLevel_ConversionFunction c) = unhandledTle Cursor_ConversionFunction c
writeTle (TopLevel_UsingDirective c)     = unhandledTle Cursor_UsingDirective c
writeTle (TopLevel_FunctionTemplate c)   = unhandledTle Cursor_FunctionTemplate c
writeTle (TopLevel_ClassTemplate c)      = unhandledTle Cursor_ClassTemplate c
writeTle (TopLevel_CXXMethod c)          = unhandledTle Cursor_CXXMethod c
writeTle (TopLevel_ClassTemplatePartialSpecialization c) =
  unhandledTle Cursor_ClassTemplatePartialSpecialization c

writeNamespace :: Namespace -> CGWriter s ()
writeNamespace (Namespace _ tles []) = mapM_ writeTle tles
writeNamespace (Namespace c _ xs) =
  error $ "writeNamespace: " ++ show c ++
  "\ngot unhandled top level elements: " ++ show (map fromUtl xs)


writeCode :: [TopLevelElem] -> CGWriter s ()
writeCode tls = do
  -- filter out everything but Ogre
  let f (TopLevel_Namespace ns@(Namespace c _ _))
        | cDisplayName c == "Ogre" = Just ns
        | otherwise = Nothing
      f _ = Nothing
      ogreNamespaces = mapMaybe f tls

  mapM_ writeNamespace ogreNamespaces
