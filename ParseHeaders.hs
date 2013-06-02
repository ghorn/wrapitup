{-# OPTIONS_GHC -Wall #-}

module ParseHeaders ( parseHeaders
                    , parseTopLevel
                    , mkCursor
                    , getContext
                    , Cursor'(..)
                    , TopLevelElem(..)
                    , ClassDecl(..)
                    , ClassElem(..)
                    , UnhandledTopLevel(..)
                    , Namespace(..)
                    , Loc(..)
                      --            , TypedefDecl(..)
                    , EnumDecl(..)
                    , Method(..)
                      --            , Class(..)
                      --            , classSummary
                    , childMapSummary
                    , makeChildMap
                    , makeChildList
                    , printChildTree
                    , runTest
                    ) where

import Control.Monad ( when, unless )
import Control.Monad.IO.Class ( liftIO )
import Clang ( ClangApp )
import Clang.Cursor ( Cursor, CursorKind(..) )
import qualified Clang as C
import qualified Clang.Diagnostic as CD
import qualified Clang.Source as CS
import qualified Clang.Type as CT
import Clang.Traversal ( ChildVisitResult(..) )
import Clang.TranslationUnit ( TranslationUnit, TranslationUnitFlags(..),
                               withCreateIndex, withParse, getCursor )
import Clang.Type ( CXXAccessSpecifier(..) )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Either ( partitionEithers )
import Data.Map ( Map )
import qualified Data.Map as M

data Loc = Loc String Int Int Int deriving (Show, Ord, Eq)
data Cursor' = Cursor' { cCursor :: Cursor
                       , cKind :: CursorKind
                       , cDisplayName :: String
                       , cKindSpelling :: String
                       , cLoc :: Loc
                       }
instance Eq Cursor' where
  Cursor' _ _ a0 a1 a2 == Cursor' _ _ b0 b1 b2 = (a0,a1,a2) == (b0,b1,b2)
instance Ord Cursor' where
  compare (Cursor' _ _ a0 a1 a2) (Cursor' _ _ b0 b1 b2) = compare (a0,a1,a2) (b0,b1,b2)

instance Show Cursor' where
  show c = "Cursor' " ++ cDisplayName c ++ " " ++ cKindSpelling c ++ " " ++ show (cLoc c)

mkCursor :: Cursor -> ClangApp s Cursor'
mkCursor c = do
  cursorKind <-  C.getKind c
  displayName <- C.getDisplayName c >>= C.unpack
  kindSpelling <- C.getCursorKindSpelling cursorKind >>= C.unpack
  location <- C.getLocation c
  spellingLocation' <- CS.getSpellingLocation location
  loc <- case spellingLocation' of
    (Nothing,_,_,_) -> return $ Loc "<failtown>" (-1) (-1) (-1)
    (Just x,y,z,w) -> do
      x' <- C.getName x >>= C.unpack
      return $ Loc x' y z w
  return $ Cursor' c cursorKind displayName kindSpelling loc

data ClassDecl = ClassDecl Cursor' [ClassElem]
instance Show ClassDecl where
  show (ClassDecl c _) = "ClassDecl " ++ show c

data EnumDecl = EnumDecl String Loc CT.TypeKind [Either Cursor' (String, Integer)]
instance Show EnumDecl where
  show (EnumDecl tc _ _ _) = "EnumDecl " ++ tc
data Method = Method { mName :: String
                     , mStatic :: Bool
                     , mVirtual :: Bool
                     , mRetType :: String
                     , mArgTypes :: [String]
                     , mLoc :: Loc
                     , mContext :: [String]
                     } deriving Show


childMapSummary :: Map String [Cursor'] -> ClangApp s ()
childMapSummary m = liftIO $ do
  let fields :: [(String, [Cursor'])]
      fields = filter (not . null . snd) $ M.toList m
      showField :: (String, [Cursor']) -> IO ()
      showField (ftype, felems) = do
        putStrLn $ "    " ++ ftype ++ "  ("++show (length felems)++ " elements)"
        mapM_ (\x -> putStrLn $ "        " ++ cDisplayName x) felems
        --mapM_ (\x -> putStrLn $ "        " ++ show x) felems
  mapM_ showField fields


type MyChildVisitor s a = Cursor -> a -> ClangApp s a

myVisitChildren :: Cursor -> MyChildVisitor s a -> a -> ClangApp s a
myVisitChildren cursor0 userVisitor userData0 = do
  m <- liftIO $ newIORef userData0
  let cheaterVisitor cursor _parent = do
        --same <- C.isSameCursor parent cursor0
        --unless same $ error "error:myVisitChildren: cursor parent is not cursor parent"
        currentUserData <- liftIO $ readIORef m
        newUserData <- userVisitor cursor currentUserData
        liftIO $ writeIORef m newUserData
        return ChildVisit_Continue
  finalRetCode <- C.visitChildren cursor0 cheaterVisitor
  finalData <- liftIO $ readIORef m
  return $ if finalRetCode then finalData else error "visitChildren return code False"

printChildTree :: String -> Cursor -> ClangApp s ()
printChildTree spacing0 topCursor = myVisitChildren topCursor (pct spacing0) ()
  where
    pct spacing c _ = do
      c' <- mkCursor c
      liftIO $ putStrLn $ spacing ++ cKindSpelling c' ++ " " ++ cDisplayName c'
      myVisitChildren c (pct (spacing ++ "    ")) ()

makeChildMap :: Cursor -> ClangApp s (Map String [Cursor'])
makeChildMap cursor = do
  let makeMap c oldMap = do
        c' <- mkCursor c
        return $ M.insertWith (++) (cKindSpelling c') [c'] oldMap

  chMap <- myVisitChildren cursor makeMap M.empty
  return (M.map reverse chMap)

makeChildList :: Cursor -> ClangApp s [Cursor']
makeChildList cursor = do
  let makeMap c oldList = do
        c' <- mkCursor c
        return $ oldList ++ [c']
  myVisitChildren cursor makeMap []

data ClassElem = CeMethod Method
               | CeConstructor Cursor'
               | CeDestructor Cursor'
               | CeTypedefDecl Cursor'
               | CeVarDecl Cursor'
               | CeFieldDecl Cursor'
               | CeUnexposedDecl Cursor'
               | CeConversionFunction Cursor'
               | CeEnumDecl EnumDecl
               | CeFunctionTemplate Cursor'
               | CeStructDecl ClassDecl
               | CeClassDecl ClassDecl
               | CeCXXBaseSpecifier Cursor'
               | CeCXXBoolLiteralExpr Cursor'
               | CeUsingDeclaration Cursor'
               | CeTypeRef Cursor'
               | CeUnionDecl Cursor'
               | CeClassTemplate Cursor'
               | CeFirstAttr Cursor'
               | CeIntegerLiteral Cursor'
               | CeTemplateRef Cursor'
               | CeNamespaceRef Cursor'
               | CeUnaryOperator Cursor'
               deriving Show

classConstructor :: Cursor' -> Maybe (ClangApp s ClassElem)
classConstructor c = let simple x = Just $ return $ x c in
  case cKind c of
    Cursor_CXXMethod          -> Just $ fmap CeMethod (parseMethod c)
    Cursor_Constructor        -> simple CeConstructor
    Cursor_Destructor         -> simple CeDestructor
    Cursor_TypedefDecl        -> simple CeTypedefDecl
    Cursor_VarDecl            -> simple CeVarDecl
    Cursor_FieldDecl          -> simple CeFieldDecl
    Cursor_UnexposedDecl      -> simple CeUnexposedDecl
    Cursor_ConversionFunction -> simple CeConversionFunction
    Cursor_EnumDecl           -> Just $ fmap CeEnumDecl (parseEnumDecl c)
    Cursor_FunctionTemplate   -> simple CeFunctionTemplate
    Cursor_StructDecl         -> Just $ fmap CeStructDecl (parseStruct c)
    Cursor_ClassDecl          -> Just $ fmap CeClassDecl (parseClass c)
    Cursor_CXXBaseSpecifier   -> simple CeCXXBaseSpecifier
    Cursor_UsingDeclaration   -> simple CeUsingDeclaration
    Cursor_CXXBoolLiteralExpr -> simple CeCXXBoolLiteralExpr
    Cursor_TypeRef            -> simple CeTypeRef
    Cursor_UnionDecl          -> simple CeUnionDecl
    Cursor_ClassTemplate      -> simple CeClassTemplate
    Cursor_FirstAttr          -> simple CeFirstAttr
    Cursor_IntegerLiteral     -> simple CeIntegerLiteral
    Cursor_TemplateRef        -> simple CeTemplateRef
    Cursor_NamespaceRef       -> simple CeNamespaceRef
    Cursor_UnaryOperator      -> simple CeUnaryOperator
    _ -> Nothing

makeClassOrStructElems :: CXXAccessSpecifier -> Cursor' -> ClangApp s [ClassElem]
makeClassOrStructElems defaultAccess cursor' = do
  access <- liftIO $ newIORef defaultAccess

  let parseClassElems :: Cursor -> [Either (String,[Cursor']) ClassElem] ->
                         ClangApp s [Either (String,[Cursor']) ClassElem]
      parseClassElems c oldElems = do
        c' <- mkCursor c
        access' <- liftIO $ readIORef access
        case cKind c' of
          -- update the access specifier
          Cursor_CXXAccessSpecifier -> do
            as <- C.getCXXAccessSpecifier c
            liftIO $ writeIORef access as
            return oldElems
          _ -> do
            newElem <- case classConstructor c' of
              Nothing -> return $ Left (show (cKind c'), [c'])
              Just doSomething -> fmap Right doSomething
            return $ if access' == CXXPublic then newElem:oldElems else oldElems

  (unhandled, classElems) <- fmap (partitionEithers . reverse) $
                             myVisitChildren (cCursor cursor') parseClassElems []
  -- check to make sure there are no unhandled fields
  unless (null unhandled) $ do
    liftIO $ putStrLn "class got unhandled elements:"
    liftIO $ mapM_ print $ M.toList $ M.fromListWith (++) unhandled
    error "class got unhandled elements"
  return classElems

parseClass :: Cursor' -> ClangApp s ClassDecl
parseClass c = fmap (ClassDecl c) $ makeClassOrStructElems CXXPrivate c

parseStruct :: Cursor' -> ClangApp s ClassDecl
parseStruct c = fmap (ClassDecl c) $ makeClassOrStructElems CXXPublic c

--parseParmType :: String -> CT.Type -> ClangApp s ParmType
--parseParmType spaces argType = do
--  cq <- CT.isConstQualifiedType    argType
--  vq <- CT.isVolatileQualifiedType argType
--  rq <- CT.isRestrictQualifiedType argType
--  let cvr = (cq,vq,rq)
--  argKind <- CT.getKind argType
--  case argKind of CT.Type_Pointer -> do
--                    pointeeType <- CT.getPointeeType argType
--                    pointeeKind <- CT.getKind pointeeType
--                    liftIO $ putStrLn $ spaces ++ "pointeeKind: " ++ show pointeeKind
--                    pt <- parseParmType (spaces ++ "  ") pointeeType
--                    return $ ParmTypeRef cvr Pointer pt
--                  CT.Type_LValueReference -> do
--                    pointeeType <- CT.getPointeeType argType
--                    pointeeKind <- CT.getKind pointeeType
--                    liftIO $ putStrLn $ spaces ++ "pointeeKind: " ++ show pointeeKind
--                    pt <- parseParmType (spaces ++ "  ") pointeeType
--                    return $ ParmTypeRef cvr Reference pt
--                  CT.Type_RValueReference -> error $ "wtf is an RValueReference?"
--                  _ -> do
--                    liftIO $ putStrLn $ spaces ++ "got unhandled argKind: " ++ show argKind
--                    pointeeType <- CT.getPointeeType argType
--                    pointeeKind <- CT.getKind pointeeType
--                    when (CT.Type_Invalid /= pointeeKind) $
--                      error "terminating recursion on pointer type"
--                    return $ ParmTypeVal cvr argType

getContext' :: C.Cursor -> [String] -> ClangApp s [String]
getContext' c acc = do
  pc <- C.getSemanticParent c
  cursorKind <-  C.getKind pc
  displayName <- C.getDisplayName pc >>= C.unpack
  let next = getContext' pc $ acc ++ [displayName]
  case cursorKind of Cursor_ClassDecl -> next
                     Cursor_StructDecl -> next
                     Cursor_Namespace -> next
                     Cursor_ClassTemplate -> next
                     Cursor_ClassTemplatePartialSpecialization -> next
                     Cursor_TranslationUnit -> return (reverse acc)
                     x -> error $ "getContext' unhandled: " ++ show x

getContext :: C.Cursor -> ClangApp s [String]
getContext c = getContext' c []

parseMethod :: Cursor' -> ClangApp s Method
parseMethod cursor' = do
  let cursor = cCursor cursor'
  k <- C.getNumArguments cursor
  cursorType <- C.getType cursor
  variadic <- CT.isFunctionTypeVariadic cursorType
  when variadic $ error "parseMethod: variadic"
  nat <- CT.getNumArgTypes cursorType
  when (k < 0) $ error "parseMethod: num args < 0"
  when (k /= nat) $ error "parseMethod: num args /= num arg types"

  static  <- C.isStaticCppMethod cursor
  virtual <- C.isVirtualCppMethod cursor

  context <- getContext cursor

  retType <- CT.getResultType cursorType
  retTypeSp <- CT.getTypeSpelling retType >>= C.unpack
  argTypesSp <- mapM (\j -> CT.getArgType cursorType j >>= CT.getTypeSpelling >>= C.unpack)
                (take k [0..])

  name <- C.getSpelling cursor >>= C.unpack

  return Method { mName = name
                , mStatic = static
                , mVirtual = virtual
                , mRetType = retTypeSp
                , mArgTypes = argTypesSp
                , mLoc = cLoc cursor'
                , mContext = context
                }

parseEnumDecl :: Cursor' -> ClangApp s EnumDecl
parseEnumDecl cursor' = do
  let cursor = cCursor cursor'
  tp <- CT.getEnumDeclIntegerType cursor >>= CT.getKind
  -- liftIO $ putStrLn $ "getEnumDeclIntegerType: " ++ show tp
  let getInteger = case tp of
        CT.Type_UInt -> fmap fromIntegral . CT.getEnumConstantDeclUnsignedValue
        CT.Type_Int  -> fmap fromIntegral . CT.getEnumConstantDeclValue
        _ -> error $ "makeEnum needs to handle: "++show tp
      makeE c oldList = do
        c' <- mkCursor c
        case cKind c' of Cursor_EnumConstantDecl -> do
                           v  <- getInteger c
                           return $ oldList ++ [Right (cDisplayName c', v)]
                         Cursor_FirstAttr -> return $ oldList ++ [Left c']
                         k -> error $ "enum got bad type:\n" ++ show cursor' ++ "\n"
                              ++ show c' ++ "\n" ++ show k
  cl <- myVisitChildren cursor makeE []
  return $ EnumDecl (cDisplayName cursor') (cLoc cursor') tp cl

data Namespace = Namespace Cursor' [TopLevelElem] [UnhandledTopLevel]
instance Show Namespace where
  show (Namespace c _ _) = "Namespace " ++ show c

data TopLevelElem = TlFunctionDecl Cursor'
                  | TlTypedefDecl Cursor'
                  | TlUnexposedDecl Cursor'
                  | TlEnumDecl EnumDecl
                  | TlStructDecl ClassDecl
                  | TlNamespace Namespace
                  | TlUnionDecl Cursor'
                  | TlClassTemplate Cursor'
                  | TlFirstAttr Cursor'
                  | TlClassTemplatePartialSpecialization Cursor'
                  | TlFunctionTemplate Cursor'
                  | TlClassDecl ClassDecl
                  | TlVarDecl Cursor'
                  | TlCXXMethod Cursor'
                  | TlConstructor Cursor'
                  | TlDestructor Cursor'
                  | TlConversionFunction Cursor'
                  | TlUsingDeclaration Cursor'
                  | TlUsingDirective Cursor'
--                  | TlFieldDecl Cursor'
--                  | TlCXXBaseSpecifier Cursor'
                  deriving Show

parseNamespace :: Cursor' -> ClangApp s Namespace
parseNamespace c = do
  (uh, tles) <- makeTopLevelElems c
  return $ Namespace c tles uh

topLevelConstructor :: Cursor' -> Maybe (ClangApp s TopLevelElem)
topLevelConstructor c = let simple x = Just $ return (x c) in
  case cKind c of
    Cursor_EnumDecl      -> Just $ fmap TlEnumDecl (parseEnumDecl c)
    Cursor_TypedefDecl   -> simple TlTypedefDecl
    Cursor_UnexposedDecl -> simple TlUnexposedDecl
    Cursor_ClassDecl     -> Just $ fmap TlClassDecl (parseClass c)
    Cursor_StructDecl    -> Just $ fmap TlStructDecl (parseStruct c)
    Cursor_Namespace     -> Just $ fmap TlNamespace (parseNamespace c)
    Cursor_FunctionDecl  -> simple TlFunctionDecl
    Cursor_UnionDecl     -> simple TlUnionDecl
    Cursor_ClassTemplate -> simple TlClassTemplate
    Cursor_FirstAttr     -> simple TlFirstAttr
    Cursor_ClassTemplatePartialSpecialization ->
      simple TlClassTemplatePartialSpecialization
    Cursor_FunctionTemplate -> simple TlFunctionTemplate
    Cursor_VarDecl       -> simple TlVarDecl
    Cursor_CXXMethod     -> simple TlCXXMethod
    Cursor_Constructor   -> simple TlConstructor
    Cursor_Destructor    -> simple TlDestructor
    Cursor_ConversionFunction -> simple TlConversionFunction
    Cursor_UsingDeclaration -> simple TlUsingDeclaration
    Cursor_UsingDirective -> simple TlUsingDirective
    _ -> Nothing

data UnhandledTopLevel = UnhandledTopLevel { fromUtl :: (String, [Cursor']) }

makeTopLevelElems :: Cursor' -> ClangApp s ([UnhandledTopLevel], [TopLevelElem])
makeTopLevelElems cursor' = do
  let parseTopLevelElems c (oldUnhandled, oldElems) = do
        c' <- mkCursor c
        case topLevelConstructor c' of
          Nothing -> return (oldUnhandled ++ [UnhandledTopLevel (show (cKind c'), [c'])], oldElems)
          Just doSomething -> do
            ret <- doSomething
            return (oldUnhandled, oldElems ++ [ret])

  myVisitChildren (cCursor cursor') parseTopLevelElems ([],[])

--       Cursor_TypedefDecl -> do
--         td <- fmap (TypedefDecl cursor') $ makeChildList cursor
--         --ut <- CT.getTypedefDeclUnderlyingType cursor >>= CT.getKind
--         --liftIO $ putStrLn $ "underlying type: " ++ show ut
--         --liftIO $ print td
--         return (stuff ++ [CG_TypedefDecl td], unhandled)

parseTopLevel :: TranslationUnit -> ClangApp s ([UnhandledTopLevel], [TopLevelElem])
parseTopLevel tu = do
  C.enableStackTraces
  defDispOpts <- CD.defaultDisplayOptions
  let f [] acc0 = return acc0
      f (diag:ds) acc0 = do
        sev <- CD.getSeverity diag
        fmt <- CD.formatDiagnostic defDispOpts diag >>= C.unpack
        let acc = acc0 ++ [fmt]
        f ds $ case sev of CD.Diagnostic_Fatal -> acc
                           CD.Diagnostic_Error -> acc
                           _ -> acc0
  errs <- CD.getDiagnostics tu >>= flip f []
  unless (null errs) $ error $ "clang parsing error\n" ++ init (unlines errs)
  c <- getCursor tu
  c' <- mkCursor c
  makeTopLevelElems c'


parseHeaders :: String -> [String] -> IO ([UnhandledTopLevel], [TopLevelElem])
parseHeaders filepath args =
  withCreateIndex False False $ \index ->
    withParse index (Just filepath) args [] [TranslationUnit_None] parseTopLevel (error "No TXUnit!")

runTest :: IO ()
runTest = do
--  let filepath = "/usr/include/OGRE/OgreBillboardChain.h"
--  let filepath = "/usr/include/OGRE/Ogre.h"
  let filepath = "/usr/include/OGRE/OgreAnimationState.h"
      --args = ["-x","c++"]
      args = ["-x","c++","-I/usr/local/llvm/lib/clang/3.4/include"] -- ++ ["-D__STDC_CONSTANT_MACROS", "-D__STDC_LIMIT_MACROS","-c"]
      --args = ["-x","c++","-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include","-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed"]
  _ <- parseHeaders filepath args
  return ()
