{-# LANGUAGE OverloadedStrings #-}

module Redmine.Get ( getTimeEntries
                   , getTimeEntry
                   , getTimeEntriesForIssue
                   , getIssue
                   , getIssues
                   , getProjects
                   , getProjectForId
                   , getProject
                   , getTrackers
                   , getVersions
                   , getUser
                   , expandOptions
                   , increaseQueryRange
                   ) where

import Data.Monoid
import qualified Data.Map as Map
import Redmine.JSON
import Redmine.Types
import Redmine.Manager
import Redmine.Rest
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Conduit
import Network.HTTP.Base (RequestMethod(..))
import Network.HTTP.Client.TLS
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Debug.Trace



-- |The function 'getTimeEntries' fetches all the time entries.
--  They can be filtered by spenton date using spent_on=%3E%3C2013-05-01|2013-05-31
getTimeEntries :: RedmineMng -> ParamRest -> MaybeT IO [TimeEntry]
getTimeEntries mng param = MaybeT $ do
   mngConn <- newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng GET requete ( Map.union param initOpt) Nothing mngConn 
   return $ fmap time_entries res
   where requete = "/time_entries.json"

getTimeEntry :: RedmineMng -> Integer -> MaybeT IO TimeEntry
getTimeEntry mng elemId = 
   fmap time_entry (MaybeT $ runQuery mng GET requete Nothing)
   where requete = "/time_entries/" <> S8.pack (show elemId) <> ".json"
                
getTimeEntriesForIssue :: RedmineMng -> Integer-> MaybeT IO [TimeEntry]
getTimeEntriesForIssue mng issueid = MaybeT $ do
   mngConn <- newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng GET requete initOpt Nothing mngConn
   return $ fmap time_entries res
   where requete = "/time_entries/" <> S8.pack (show issueid) <> ".json"

getIssues :: RedmineMng -> ParamRest -> MaybeT IO [Issue]
getIssues mng param = MaybeT $ do
   mngConn <- liftIO $ newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng GET requete ( Map.union param initOpt) Nothing mngConn
   return $ fmap issues res
   where requete = "/issues.json"

getIssue :: RedmineMng -> Integer -> ParamRest -> MaybeT IO Issue
getIssue mng elemId param =
   fmap issue (MaybeT $ runQuery mng GET requete Nothing)
   where requete = "/issues/" <> S8.pack (show elemId) <> ".json" <> expandOptions param

getTrackers :: RedmineMng -> MaybeT IO [Tracker]
getTrackers mng = MaybeT $ runQuery mng GET (trace (show requete) requete) Nothing
  where requete = rmhost mng <> "/trackers.json" 
--   mngConn <- newManager tlsManagerSettings
--   res <- queryRedmineAvecOptions mng GET requete initOpt Nothing mngConn 
--   return $ fmap trackers res
--   where requete = "/trackers.json"

getProjects :: RedmineMng -> MaybeT IO [Project]
getProjects mng = MaybeT $ do
   mngConn <- newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng GET requete initOpt Nothing mngConn 
   return $ fmap projects res
   where requete = "/projects.json"

getProjectForId :: RedmineMng -> Integer -> MaybeT IO Project
getProjectForId mng elemId =
   MaybeT $ runQuery mng GET requete Nothing
   where requete = rmhost mng <> "/projects/" <> S8.pack (show elemId) <> ".json"

getProject :: RedmineMng -> S.ByteString -> MaybeT IO Project
getProject mng projId =
   MaybeT $ runQuery mng GET requete Nothing
   where requete = "/projects/" <> projId <> ".json"

--Get all the versions associated to a project
getVersions :: RedmineMng --The connection manager
            -> S.ByteString --The project
            -> MaybeT IO [Version]
getVersions mng proj =
   fmap versions (MaybeT $ runQuery mng GET requete Nothing)
   where requete = "/projects/" <> proj <> "/versions.json"

getVersion:: RedmineMng -> Integer -> ParamRest -> MaybeT IO Version
getVersion mng elemId param =
   fmap version (MaybeT $ runQuery mng GET requete Nothing)
   where requete = "/versions/" <> S8.pack (show elemId) <> ".json" <> expandOptions param

getUser :: RedmineMng -> Integer -> MaybeT IO User
getUser mng elemId =
    fmap user (MaybeT $ runQuery mng GET requete Nothing)
    where requete = "/users/" <> S8.pack (show elemId) <> ".json"

