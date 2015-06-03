{-# LANGUAGE OverloadedStrings #-}

--{-# OPTIONS_GHC -Wall #-}
module Main where
import Redmine.Manager
import Redmine.Types
import Redmine.Utils
import Redmine.Reporter.Config
import Data.Maybe (fromJust, isJust)
import Control.Monad (join)
import qualified Data.Map as Map (fromList, empty, lookup, Map)
import Control.Monad.Trans.Maybe

import qualified Text.PrettyPrint as PP

import Redmine.Reporter.Parser
import Redmine.Reporter
import Redmine.Rest
import Redmine.Post
import Redmine.Get

import qualified Data.List as L (find, groupBy, sortBy)
import System.Environment (getArgs)

--import Debug.Trace (trace)

import Data.ByteString.UTF8 (fromString)


--emptyParamSet = Map.empty
defaultConfigFile :: FilePath
defaultConfigFile = "test.txt" 


issueWithJournal :: Redmine.Rest.ParamRest
issueWithJournal = Map.fromList [("include","journals")]
allIssues :: Redmine.Rest.ParamRest
allIssues = Map.fromList [("status_id","*"), ("project_id", "testproject"), ("sort","issue_id:desc")]

defaultArgs :: [String]
defaultArgs = ["2015-03-21T00:00:00Z", "2015-03-25T00:00:00Z"]
timeExtention :: String
timeExtention = "T00:00:00Z"

defaultConfig :: Config (String, String) String (Integer, String) Integer (String, [Integer])
defaultConfig = Config [] []


mkArgs args = case length args of
  0 -> defaultArgs
  _ -> take 2 $  map (\x -> (x ++ timeExtention)) args ++ repeat ""

main  = do
  args' <- getArgs
  let [from, to] = map parseRHTime $ mkArgs args'
  config' <- readConfig defaultConfigFile
  let config = either (const defaultConfig) id config'

  let sects = sections config
      kv    = keyValue config
      -- FIXME: error handling. Just pass invalid information if missing. No error is reported.
      user     = maybe "" fromString $ lookup "user" kv
      password = maybe "" fromString $ lookup "password" kv
      url      = maybe "" fromString $ lookup "url" kv
      rm = RedmineMngWithAuth url user password
      
      ud = findSection "tracker" sects
      pj = findSection "project" sects
      
      pjid = getId "id" $ findSection "project" sects
      trid = getId "id" $ findSection "tracker" sects       
      
      cf = fmap (\ud -> mkConfigMap ud) ud
      (trackerIdMap, trackerOrderMap) = case cf of
        Just (_, i, o) -> (i, o)
        Nothing            -> (Map.empty, Map.empty)
{-      issue = Issue { project_Issue = ObjRef {id_ObjRef = 1}, tracker_Issue = ObjRef 1 "",
                      subject_Issue = "This is a Test.", description_Issue = "", journals_Issue = Just []}
  is <- runMaybeT $ postIssue rm issue
  return is-}
  trks <- runMaybeT $ getTrackers rm
--  prjs <- runMaybeT $ getProjects rm

  print trks

  is <- (runMaybeT $ getIssues rm allIssues)
  let mbiids = fmap (map id_Issue . filter (isTargetIssue pjid trid Nothing)) is -- drop unnecessary issues
  js <- case mbiids of
    Just iids -> do
      a <- mapM (\i -> runMaybeT $ getIssue rm i issueWithJournal) iids
      return $ Just a
    Nothing -> return Nothing
  let ass = fmap (L.groupBy eq . L.sortBy (cmp trackerOrderMap) . map (modifyIssue trackerIdMap) . map fromJust . filter isJust) js
      b = fmap (PP.render . toDoc from to . map Group ) ass
      eq a b = tracker_Issue a == tracker_Issue b
      cmp  :: OrderMap Int Int -> Issue -> Issue -> Ordering
      cmp tMap i1 i2 = compare (p i1) (p i2)
        where
          p i = r
            where r = ( Map.lookup (fromIntegral iss_id ) tMap, iss_id, id_Issue i)
                  iss_id = id_ObjRef . tracker_Issue $ i

  case b of
    Just x -> putStrLn x
    Nothing -> putStr "failed"


