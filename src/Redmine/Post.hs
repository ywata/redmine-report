{-# LANGUAGE OverloadedStrings #-}

module Redmine.Post (postIssue) where

import Data.Aeson
import Data.Monoid
import qualified Data.Map as Map
import Redmine.JSON
import Redmine.Types
import Redmine.Manager
import Redmine.Rest
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Conduit
import Network.HTTP.Base (RequestMethod(..))
import Network.HTTP.Client.TLS
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Debug.Trace


postIssue :: RedmineMng -> Issue -> MaybeT IO Issue
postIssue mng iss =
  fmap issue (MaybeT $ runQuery mng POST requete (toJsonBody iss))
  where requete = "issues.json" 



       
