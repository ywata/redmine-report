{-# LANGUAGE DataKinds #-}
module Redmine.Reporter.Config (mkConfigMap, Config(..), SectionDef(..), OrderMap, IdMap) where

import Control.Exception (bracket, catch, SomeException)
import System.IO (hClose, hGetContents, openFile,  IOMode(..))
import Data.Maybe (maybe)

import Data.Graph (buildG, topSort, Graph, Vertex)

import qualified Data.Map as Map

data Config kv n a b = Config {keyValue::[kv], sections:: [(SectionDef n a b)]} deriving(Show, Eq)
data SectionDef n a b = SectionDef{name::n, defs::[a], orders::[[b]]} deriving(Show, Eq)


type IdMap a b= Map.Map a b
type OrderMap a b= Map.Map a b
--readConfig:: FilePath -> IO Config
--readConfig f = bracket (openFile f ReadMode) hClose (\h -> hGetContents h >>= return . parseConfig )

--mkConfigMap :: SectionDef a b c -> (String, IdMap, OrderMap)
mkConfigMap (SectionDef n defs ords) = (n, idm, odm)
  where
    idm = mkIdMap defs
    odm = mkOrderMap ords



mkIdMap::(Integral a) => [(a, b)] -> IdMap a b
mkIdMap dfs = Map.fromList $ map (\(f, s) -> (fromIntegral f, s)) dfs


-- XXX: Data.Graph only supports Int parameterized vetex!!.
mkOrderMap:: Integral a => [[a]]  -> OrderMap Int Int
mkOrderMap os = invMap . topSort $ g
  where
    g = mkGraph $ map (map fromIntegral) os
    mkGraph :: [[Vertex]] -> Graph
    mkGraph vss = buildG bd ed
      where
        bd = (minimum $ concat vss, maximum $ concat vss)
        ed = concatMap mkPair vss
    mkPair::[t] -> [(t,t)]
    mkPair [] = []
    mkPair (a:b:[]) = [(a,b)]
    mkPair (a:bs) = (a, head bs) : mkPair bs
 

idShow:: (Ord b) => a -> Map.Map b a -> b -> a
idShow s m i =  maybe s id (Map.lookup i m)

invMap::[Int] -> Map.Map Int Int
invMap = Map.fromList . zip [1..] 


{-
a = parseConfig "[tracker]\n1 < 2\n"

main = do
  h <- readConfig "test.txt"
  return h
-}       
