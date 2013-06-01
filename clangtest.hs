{-# OPTIONS_GHC -Wall #-}

module Main ( main
            , CG_Elem(..)
            , TypedefDecl(..)
            , TopLevelElem(..)
            , ClassElem(..)
            , EnumDecl(..)
            , Class(..)
            , Cursor'(..)
--            , classSummary
            , childMapSummary
            , makeChildMap
            , printChildTree
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

data Cursor' = Cursor' { cCursor :: Cursor
                       , cKind :: CursorKind
                       , cDisplayName :: String
                       , cKindSpelling :: String
                       , cSpellingLoc :: String
                       }
instance Eq Cursor' where
  Cursor' _ _ a0 a1 a2 == Cursor' _ _ b0 b1 b2 = (a0,a1,a2) == (b0,b1,b2)
instance Ord Cursor' where
  compare (Cursor' _ _ a0 a1 a2) (Cursor' _ _ b0 b1 b2) = compare (a0,a1,a2) (b0,b1,b2)

instance Show Cursor' where
  show c = "Cursor' " ++ cDisplayName c ++ " " ++ cKindSpelling c ++ " " ++ cSpellingLoc c

mkCursor :: Cursor -> ClangApp s Cursor'
mkCursor c = do
  cursorKind <-  C.getKind c
  displayName <- C.getDisplayName c >>= C.unpack
  kindSpelling <- C.getCursorKindSpelling cursorKind >>= C.unpack
  location <- C.getLocation c
  spellingLocation' <- CS.getSpellingLocation location
  spellingLocation <- case spellingLocation' of
    (Nothing,_,_,_) -> return "Nothing"
    (Just x,y,z,w) -> do
      x' <- C.getName x >>= C.unpack
      return $ show (x',y,z,w)
  return $ Cursor' c cursorKind displayName kindSpelling spellingLocation

data Class = Class Cursor' [ClassElem]
data TypedefDecl = TypedefDecl Cursor' [Cursor'] deriving Show
data StructDecl = StructDecl Class
data EnumDecl = EnumDecl CT.TypeKind [(Cursor',CT.TypeKind,Integer)]
data Constructor = Constructor Cursor' (Map String [Cursor'])

--classSummary :: Class -> ClangApp s ()
--classSummary (Class c m) = liftIO $ do
--  putStrLn $ "class \"" ++ cDisplayName c ++ "\""
--  let fields :: [(String, [Cursor'])]
--      fields = filter (not . null . snd) $ M.toList m
--      showField :: (String, [Cursor']) -> IO ()
--      showField (ftype, felems) = do
--        putStrLn $ "    " ++ ftype ++ "  ("++show (length felems)++ " elements)"
--        mapM_ (\x -> putStrLn $ "        " ++ cDisplayName x) felems
--  mapM_ showField fields

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

data ClassElem = Class_Method Cursor'
               | Class_Constructor Cursor'
               | Class_Destructor Cursor'
               | Class_TypedefDecl Cursor'
               | Class_VarDecl Cursor'
               | Class_FieldDecl Cursor'
               | Class_UnexposedDecl Cursor'
               | Class_CXXConversionFunction Cursor'
               | Class_EnumDecl Cursor'
               | Class_StructDecl Cursor'
               | Class_FunctionTemplate Cursor'
               | Class_ClassDecl Cursor'
               | Class_CXXBaseSpecifier Cursor'
               | Class_UsingDeclaration Cursor'

makeClassOrStructMap :: CXXAccessSpecifier -> Cursor -> ClangApp s [ClassElem]
makeClassOrStructMap defaultAccess cursor = do
  cursor' <- mkCursor cursor
  liftIO $ putStrLn $ "-------- parsing class/struct \""++cDisplayName cursor'++"\" ----------"
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
          other -> do
            let elemConstructor :: Cursor' -> Either (String,[Cursor']) ClassElem
                elemConstructor = case other of
                  Cursor_CXXMethod          -> Right . Class_Method
                  Cursor_Constructor        -> Right . Class_Constructor
                  Cursor_Destructor         -> Right . Class_Destructor
                  Cursor_TypedefDecl        -> Right . Class_TypedefDecl
                  Cursor_VarDecl            -> Right . Class_VarDecl
                  Cursor_FieldDecl          -> Right . Class_FieldDecl
                  Cursor_UnexposedDecl      -> Right . Class_UnexposedDecl
                  Cursor_ConversionFunction -> Right . Class_CXXConversionFunction
                  Cursor_EnumDecl           -> Right . Class_EnumDecl
                  Cursor_FunctionTemplate   -> Right . Class_FunctionTemplate
                  Cursor_StructDecl         -> Right . Class_StructDecl
                  Cursor_ClassDecl          -> Right . Class_ClassDecl
                  Cursor_CXXBaseSpecifier   -> Right . Class_CXXBaseSpecifier
                  Cursor_UsingDeclaration   -> Right . Class_UsingDeclaration
                  x -> \cc -> Left (show x, [cc])
            return $ if access' == CXXPublic then elemConstructor c':oldElems else oldElems

  (unhandled, classElems) <- fmap (partitionEithers . reverse) $
                             myVisitChildren cursor parseClassElems []
  -- check to make sure there are no unhandled fields
  unless (null unhandled) $ do
    liftIO $ putStrLn "class got unhandled elements:"
    liftIO $ mapM_ print $ M.toList $ M.fromListWith (++) unhandled
    error "class got unhandled elements"
  return classElems

makeClassMap :: Cursor -> ClangApp s [ClassElem]
makeClassMap = makeClassOrStructMap CXXPrivate

makeStructMap :: Cursor -> ClangApp s [ClassElem]
makeStructMap = makeClassOrStructMap CXXPublic

data ParmDecl = ParmDecl deriving Show

parseParmDecl :: Cursor -> CT.Type -> ClangApp s ParmDecl
parseParmDecl argCursor argType = do
  argCursor' <- mkCursor argCursor
  when (cKind argCursor' /= Cursor_ParmDecl) $ error "parseParmDecl got non-ParmDecl"
  argKind <- CT.getKind argType
  argKindSpelling <- CT.getTypeKindSpelling argKind >>= C.unpack
  printChildTree "    ++++" argCursor
  liftIO $ putStrLn $ "    +++-" ++ show (argKind, argKindSpelling)--, argCursor')
  case argKind of CT.Type_Pointer -> do
                    pointeeType <- CT.getPointeeType argType
                    pointeeKind <- CT.getKind pointeeType
                    liftIO $ putStrLn $ "    ====pointeeKind: " ++ show pointeeKind
                    return ()
                  x -> do
                    liftIO $ putStrLn $ "    ====got unhandled argKind: " ++ show x
                    return ()
  liftIO (putStrLn "")
  return ParmDecl


makeMethod :: Cursor -> ClangApp s ()
makeMethod cursor = do
  cursor' <- mkCursor cursor
  k <- C.getNumArguments cursor
  cursorType <- C.getType cursor
  variadic <- CT.isFunctionTypeVariadic cursorType
  when variadic $ error "variadic"
  nat <- CT.getNumArgTypes cursorType
  when (k < 0) $ error "num args < 0"
  when (k /= nat) $ error "num args /= num arg types"
  liftIO $ putStrLn $ "    method: " ++ cDisplayName cursor' ++ " (" ++ show k ++ " args)"
  let parseParmDecl' j = do
        argCursor <- C.getArgument cursor j
        argType <- CT.getArgType cursorType j
        parseParmDecl argCursor argType
  parmDecls <- mapM parseParmDecl' (take k [0..])

--  liftIO $ mapM_ print argKinds
--  liftIO $ mapM_ (\x -> putStrLn $ "        " ++ show x) (zip3 argKinds argKindSpellings argCursors')
  return ()

makeEnum :: Cursor -> ClangApp s EnumDecl
makeEnum cursor = do
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
                           return $ oldList ++ [(c', tp, v)]
                         _ -> error "enum has field besides EnumConstantDecl"
  cl <- myVisitChildren cursor makeE []
  return $ EnumDecl tp cl

data CG_Elem = CG_Class Class
             | CG_TypedefDecl TypedefDecl
             | CG_StructDecl StructDecl
             | CG_EnumDecl EnumDecl
             | CG_Constructor Constructor


data TopLevelElem = TopLevel_Method Cursor'
                  | TopLevel_Constructor Cursor'
                  | TopLevel_Destructor Cursor'
                  | TopLevel_TypedefDecl Cursor'
                  | TopLevel_VarDecl Cursor'
                  | TopLevel_FieldDecl Cursor'
                  | TopLevel_UnexposedDecl Cursor'
                  | TopLevel_CXXConversionFunction Cursor'
                  | TopLevel_EnumDecl Cursor'
                  | TopLevel_StructDecl Cursor'
                  | TopLevel_FunctionTemplate Cursor'
                  | TopLevel_ClassDecl Cursor'
                  | TopLevel_CXXBaseSpecifier Cursor'
                  | TopLevel_UsingDeclaration Cursor'
                  | TopLevel_Namespace Cursor'
                  | TopLevel_FunctionDecl Cursor'
                  | TopLevel_UnionDecl Cursor'

makeTopLevelMap :: Cursor -> ClangApp s [TopLevelElem]
makeTopLevelMap cursor = do
  cursor' <- mkCursor cursor
  liftIO $ putStrLn $ "-------- parsing topLevel \""++cDisplayName cursor'++"\" ----------"

  let parseTopLevelElems :: Cursor -> [Either (String,[Cursor']) TopLevelElem] ->
                            ClangApp s [Either (String,[Cursor']) TopLevelElem]
      parseTopLevelElems c oldElems = do
        c' <- mkCursor c
        let elemConstructor :: Cursor' -> Either (String,[Cursor']) TopLevelElem
            elemConstructor = case cKind c' of
              Cursor_CXXMethod          -> Right . TopLevel_Method
              Cursor_Constructor        -> Right . TopLevel_Constructor
              Cursor_Destructor         -> Right . TopLevel_Destructor
              Cursor_TypedefDecl        -> Right . TopLevel_TypedefDecl
              Cursor_VarDecl            -> Right . TopLevel_VarDecl
              Cursor_FieldDecl          -> Right . TopLevel_FieldDecl
              Cursor_UnexposedDecl      -> Right . TopLevel_UnexposedDecl
              Cursor_ConversionFunction -> Right . TopLevel_CXXConversionFunction
              Cursor_EnumDecl           -> Right . TopLevel_EnumDecl
              Cursor_FunctionTemplate   -> Right . TopLevel_FunctionTemplate
              Cursor_StructDecl         -> Right . TopLevel_StructDecl
              Cursor_ClassDecl          -> Right . TopLevel_ClassDecl
              Cursor_CXXBaseSpecifier   -> Right . TopLevel_CXXBaseSpecifier
              Cursor_UsingDeclaration   -> Right . TopLevel_UsingDeclaration
              Cursor_Namespace          -> Right . TopLevel_Namespace
              Cursor_FunctionDecl       -> Right . TopLevel_FunctionDecl
              Cursor_UnionDecl          -> Right . TopLevel_UnionDecl
              x -> \cc -> Left (show x, [cc])
        return $ elemConstructor c':oldElems

  (unhandled, topLevelElems) <- fmap (partitionEithers . reverse) $
                                myVisitChildren cursor parseTopLevelElems []
  -- check to make sure there are no unhandled fields
  unless (null unhandled) $ do
    liftIO $ putStrLn "topLevel got unhandled elements:"
    liftIO $ mapM_ print $ M.keys $ M.fromListWith (++) unhandled
    error "topLevel got unhandled elements"
  return topLevelElems

visitNamespaceChild :: Cursor -> ([CG_Elem],Map String [Cursor'])
                       -> ClangApp s ([CG_Elem],Map String [Cursor'])
visitNamespaceChild cursor (stuff,unhandled) = do
  cursor' <- mkCursor cursor
  case cKind cursor' of
       Cursor_ClassDecl -> do
         --liftIO $ putStrLn $ "---------- parsing class \""++(cDisplayName cursor')++"\" ----------"
         cls <- fmap (Class cursor') $ makeClassMap cursor
         --classSummary cls
         return (stuff ++ [CG_Class cls], unhandled)
       Cursor_TypedefDecl -> do
         td <- fmap (TypedefDecl cursor') $ makeChildList cursor
         --ut <- CT.getTypedefDeclUnderlyingType cursor >>= CT.getKind
         --liftIO $ putStrLn $ "underlying type: " ++ show ut
         --liftIO $ print td
         return (stuff ++ [CG_TypedefDecl td], unhandled)
       Cursor_StructDecl -> do
         --liftIO $ putStrLn $ "---------- parsing struct \""++(cDisplayName cursor')++"\" ----------"
         cls <- fmap (Class cursor') $ makeStructMap cursor
         --classSummary cls
         return (stuff ++ [CG_StructDecl (StructDecl cls)], unhandled)
       Cursor_EnumDecl -> do
         --liftIO $ putStrLn $ "---------- parsing enum \""++(cDisplayName cursor')++"\" ----------"
         enum <- makeEnum cursor
         --liftIO $ mapM_ print cm
         return (stuff ++ [CG_EnumDecl enum], unhandled)
       Cursor_Constructor -> do
         --liftIO $ putStrLn $ "------ parsing CXXConstructor \""++(cDisplayName cursor')++"\" ------"
         --liftIO $ print cursor'
         cm <- makeChildMap cursor
         --childMapSummary cm
         return (stuff ++ [CG_Constructor $ Constructor cursor' cm], unhandled)
--       Cursor_CXXMethod -> do
--         --liftIO $ putStrLn $ "\n------ parsing CXXMethod \""++(cDisplayName cursor')++"\" ------"
--         --liftIO $ print cursor'
--         --makeMethod cursor
--         cm <- makeChildMap cursor
--         --childMapSummary cm
--         return $ (stuff, unhandled)
       _ -> do
         --liftIO $ putStrLn $ "unhandled element: " ++ show x ++ ": " ++ show cursor'
         return (stuff, M.insertWith (++) (cKindSpelling cursor') [cursor'] unhandled)

parseNamespace :: String -> MyChildVisitor s ([[CG_Elem]], Map String [Cursor'])
parseNamespace namespaceName cursor su@(stuff,unhandled) = do
  c <- mkCursor cursor
  if (cKind c, cDisplayName c) == (Cursor_Namespace,namespaceName)
    then do (s,u) <- myVisitChildren cursor visitNamespaceChild ([],M.empty)
            return (stuff ++ [s], M.unionWith (++) unhandled u)
    else return su


test :: TranslationUnit -> ClangApp s ([[CG_Elem]], Map String [Cursor'])
test tu = do
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
  _ <- makeTopLevelMap c
  myVisitChildren c (parseNamespace "Ogre") ([],M.empty)

main :: IO ()
main = do
--  let filepath = "/usr/include/OGRE/OgreBillboardChain.h"
  let filepath = "/usr/include/OGRE/Ogre.h"
--  let filepath = "/usr/include/OGRE/OgreAnimationState.h"
      --args = ["-x","c++"]
      args = ["-x","c++","-I/usr/local/llvm/lib/clang/3.4/include"] -- ++ ["-D__STDC_CONSTANT_MACROS", "-D__STDC_LIMIT_MACROS","-c"]
      --args = ["-x","c++","-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include","-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed"]
  (stuff,unhandled) <- withCreateIndex False False $ \index ->
      withParse index (Just filepath) args [] [TranslationUnit_None] test (error "No TXUnit!")
  putStrLn "unhandled:"
  mapM_ print (M.keys unhandled)

--  print stuff
  
  return ()
