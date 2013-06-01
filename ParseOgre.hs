{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module ParseOgre ( main
                 , CGWriterOut(..)
                 ) where


--import Control.Monad ( unless )
import Control.Monad.State.Lazy -- ( WriterT )
--import Control.Monad.IO.Class ( liftIO )
import Clang ( ClangApp )
--import Clang.Cursor ( Cursor, CursorKind(..) )
--import qualified Clang as C
--import qualified Clang.Diagnostic as CD
--import qualified Clang.Source as CS
--import qualified Clang.Type as CT
--import Clang.Traversal ( ChildVisitResult(..) )
import Clang.TranslationUnit ( TranslationUnit, TranslationUnitFlags(..),
                               withCreateIndex, withParse )
--import Clang.Type ( CXXAccessSpecifier(..) )
--import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Either ( partitionEithers )
--oimport Data.List ( partition )
import Data.Maybe ( mapMaybe )
--import Data.Map ( Map )
--import qualified Data.Map as M
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
  mapM_ putStrLn (cgUnhandled cgOut)
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
data CGWriterOut = CGWriterOut { cgUnhandled :: [String]
                               , cgC :: [String]
                               , cgHaskell :: [String]
                               }
cgEmpty :: CGWriterOut
cgEmpty = CGWriterOut [] [] []

type CGWriter s a = StateT CGWriterOut (ClangApp s) a

unhandled :: MonadState CGWriterOut m => String -> m ()
unhandled x = do
  cg <- get
  put $ cg {cgUnhandled = cgUnhandled cg ++ [x]}

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
--  writeHaskell "data 
  liftIO $ mapM_ print elems
  let cons = map (flip normalC [] . mkName . fst) elems
  hsEnum <- liftIO $ runQ $ dataD (cxt []) (mkName name) [] cons []
  hsEnumInstance <- liftIO $ runQ $ mkEnumInstance name elems
  
  writeHaskell $ "-- EnumDecl: " ++ name ++ ", " ++ loc
  writeHaskell (pprint $ hsEnum)
  writeHaskell (pprint $ hsEnumInstance)


writeClassElem :: ClassElem -> CGWriter s ()
--  liftIO $ putStrLn c
writeClassElem c = error $ "writeClassElem: unhandled " ++ show c
  
writeTle :: TopLevelElem -> CGWriter s ()
writeTle (TopLevel_TypedefDecl {}) = return ()
writeTle (TopLevel_ClassDecl (ClassDecl _ elems)) = mapM_ writeClassElem elems
writeTle (TopLevel_StructDecl (ClassDecl _ elems)) = mapM_ writeClassElem elems
writeTle (TopLevel_ClassTemplate c) = unhandled $ "ClassTemplate: " ++ show c
writeTle (TopLevel_EnumDecl ed) = writeEnumDecl ed
writeTle x = error $ "unhandled tle: " ++ show x

writeNamespace :: Namespace -> CGWriter s ()
writeNamespace (Namespace c tles []) = do
  liftIO $ putStrLn $ "-------- writing namespace " ++ cDisplayName c ++ ": " ++  cSpellingLoc c
  mapM_ writeTle tles
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
  


  
