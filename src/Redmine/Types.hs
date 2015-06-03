{-# LANGUAGE OverloadedStrings #-}
module Redmine.Types where

import Data.Time.Clock      (UTCTime)
import Data.Time.Calendar (Day)
import Network.HTTP.Conduit
import Data.Monoid
import Data.Typeable
import Data.Data
import Data.Aeson
import qualified Data.Text as T

data Status = Closed | Open deriving (Eq, Show)

data VersionsRsp = VersionsRsp { versions :: [Version] } deriving (Eq,Show)

data VersionRsp = VersionRsp { version :: Version } deriving (Eq,Show)

data Version = Version { id_Version      :: Integer
                       , name_Version    :: T.Text
                       , project_Version :: ObjRef
                       , desc_Version    :: T.Text
                       , status_Version  :: T.Text
                       , sharing_Version :: T.Text
                       , dueDate_Version     :: Maybe Day
                       , createdOn_Version   :: Maybe UTCTime
                       , updatedOn_Version   :: Maybe UTCTime
                       } deriving (Eq, Show)
--ajouter offset total_cout et limit
data IssuesRsp = IssuesRsp { issues :: [Issue] } deriving (Eq,Show)

--data Parent = Parent { parent :: ObjID } deriving (Eq,Show)

data IssueRsp = IssueRsp { issue :: Issue } deriving (Eq,Show)

data Issue = Issue { id_Issue :: Integer
                   , project_Issue :: ObjRef
                   , parent_Issue :: Maybe ObjID --Single issue only
                   , tracker_Issue :: ObjRef
                   , status_Issue :: ObjRef
                   , priority_Issue :: ObjRef
                   , author_Issue :: ObjRef
                   , assignedTo_Issue :: Maybe ObjRef
                   , category_Issue :: Maybe ObjRef
                   , fixedVersion_Issue :: Maybe ObjRef
                   , subject_Issue :: T.Text
                   , description_Issue :: T.Text
                   , startDate_Issue :: Maybe Day
                   , dueDate_Issue :: Maybe Day
                   , doneRatio_Issue :: Int
                   , estimatedHours_Issue :: Maybe Float
                   , spentHours_Issue :: Maybe Float --Single issue only
                   , customFields_Issue :: Maybe [CustomField]
                   , createdOn_Issue   :: Maybe UTCTime
                   , updatedOn_Issue   :: Maybe UTCTime
                   , journals_Issue :: Maybe [Journal] -- Single issue only
                   , attachements_Issue :: Maybe [Attachement]
                   , changeSets_Issue :: Maybe [ChangeSet]
                   , watchers_Issue :: Maybe [Watcher]
                   , relations_Issue :: Maybe [Relation]
                   , children_Issue :: Maybe [Child]
                   } deriving (Eq, Show)

data ChangeSet = ChangeSet { revision_ChangeSet :: T.Text
                           , user_ChangeSet :: ObjRef
                           , comments_ChangeSet :: T.Text
                           , committedOn_ChangeSet :: UTCTime
                           } deriving (Eq, Show)

data Watcher = Watcher { id_Watcher :: Integer
                       , name_Watcher :: T.Text
                       } deriving (Eq, Show)

data Child = Child { id_Child :: Integer
                   , tracker_Child :: ObjRef
                   , subject_Tracker :: T.Text
                   } deriving (Eq, Show)

data CustomField = CustomField { id_CF    :: Integer
                               , name_CF  :: T.Text
                               , value_CF :: T.Text
                               } deriving (Eq,Show)

data ObjRef = ObjRef { id_ObjRef:: Integer, name_ObjRef:: T.Text } deriving (Eq, Show)

data ObjID = ObjID { id_ObjID:: Integer } deriving (Eq, Show)

data IssueStatuses = IssueStatuses { issue_statuses :: [IssueStatus] }
                                     deriving (Eq,Show)

data IssueStatus = IssueStatus { id_IssueStatus:: Integer
                               , name_IssueStatus:: T.Text
                               , isDefault_IssueStatus:: Bool
                               , isClosed_IssueStatus:: Bool
                               } deriving (Eq, Show)

data ProjectsRsp = ProjectsRsp { projects :: [Project] } deriving (Eq,Show)

data ProjectRsp = ProjectRsp { project :: Project } deriving (Eq,Show)

data Project = Project { id_Project:: Integer
                       , name_Project:: T.Text
                       , identifier_Project:: T.Text
                       , desc_Project:: T.Text
                       , customFields_Project :: Maybe [CustomField]
                       , createdOn_Project   :: Maybe UTCTime
                       , updatedOn_Project   :: Maybe UTCTime
                       } deriving (Eq, Show)


data UsersRsp = UsersRsp { users :: [User] } deriving (Eq,Show)

data UserRsp = UserRsp { user :: User } deriving (Eq,Show)

data User = User { lastname:: T.Text
                 , createdOn_User:: Maybe UTCTime
                 , mail:: T.Text
                 , r :: Maybe UTCTime
                 , firstname:: T.Text
                 , id_User:: Integer
                 } deriving (Eq, Show)

data TrackersRsp = TrackersRsp { trackers :: [Tracker] } deriving (Eq,Show)

data TrackerRsp = TrackerRsp { tracker :: Tracker } deriving (Eq,Show)

data Tracker = Tracker { id_Tracker   :: Integer
                       , name_Tracker :: T.Text
                       } deriving (Eq, Show)


data Detail = Detail { property :: T.Text
                     , name_Detail :: T.Text
                     , old_value_Detail :: Maybe T.Text
                     , new_value_Detail :: T.Text
                     } deriving (Eq,Show)


data Journal = Journal { id_Journal :: Integer
                       , user_Journal :: ObjRef
                       , notes_Journal :: T.Text
                       , createdOn_Journal :: Maybe UTCTime
                       , details_Journal :: Maybe [Detail]
                       } deriving (Eq, Show)


data Attachement = Attachement { id_Attachement :: Integer
                               , filename_Attachement :: T.Text
                               , filesize_Attachement :: Integer
                               , contentType_Attachement :: T.Text
                               , description_Attachement :: T.Text
                               , contentUrl_Attachement :: T.Text
                               , authorName_Attachement :: ObjRef
                               , createdOn_Attachement :: UTCTime
                               } deriving (Eq, Show)


data TimeEntriesRsp = TimeEntriesRsp { time_entries :: [TimeEntry] } deriving (Eq,Show)

data TimeEntryRsp = TimeEntryRsp { time_entry :: TimeEntry } deriving (Eq,Show)

data TimeEntry = TimeEntry { id_TE       :: Integer
                           , project_TE  :: ObjRef
                           , issue_TE      :: ObjID
                           , user_TE      :: ObjRef
                           , activity_TE   :: Maybe ObjRef
                           , hours_TE      :: Maybe Float
                           , comments_TE   :: T.Text
                           , createdOn_TE   :: Maybe UTCTime
                           , updatedOn_TE   :: Maybe UTCTime
                           , spentOn_TE   :: Maybe Day
                           } deriving (Eq, Show)

data Memberships = Memberships { memberships :: [Membership]
                               } deriving (Eq,Show)

data Membership = Membership { id_Membership :: Integer
                             , project_Membership :: ObjRef
                             , user_Membership    :: ObjRef
                             , roles_Membership   :: [Role]
                             } deriving (Eq, Show)

data Roles = Roles { roles :: [Role] } deriving (Eq,Show)

data Role = Role { id_Role   :: Integer
                 , name_Role :: T.Text
                 } deriving (Eq, Show)

data RelationType = Relates | Duplicates | Duplicated | Blocks | Blocked
                  | Precedes | Follows deriving (Eq, Show)

data Relations = Relations { relations :: [Relation] } deriving (Eq,Show)

data Relation = Relation { id_Relation :: Integer
                         , issueId_Relation:: Integer
                         , issueToId_Relation:: Integer
                         , relationType_Relation:: T.Text
                         , delay_Relation:: Maybe Integer
                         } deriving (Eq, Show)

instance Monoid TimeEntriesRsp where
 mappend a b = TimeEntriesRsp (time_entries a++time_entries b)
 mempty      = TimeEntriesRsp []

instance Monoid IssuesRsp where
 mappend a b = IssuesRsp (issues a++issues b)
 mempty      = IssuesRsp []

instance Monoid ProjectsRsp where
 mappend a b = ProjectsRsp (projects a++projects b)
 mempty      = ProjectsRsp []

instance Monoid TrackersRsp where
 mappend a b = TrackersRsp (trackers a ++ trackers b)
 mempty      = TrackersRsp []

instance Monoid VersionsRsp where
 mappend a b = VersionsRsp (versions a++versions b)
 mempty      = VersionsRsp []

class Collection a where
    longueur :: a -> Int

instance Collection TimeEntriesRsp where
    longueur (TimeEntriesRsp a) = length a

instance Collection TrackersRsp where
    longueur (TrackersRsp a) = length a

instance Collection IssuesRsp where
    longueur (IssuesRsp a) = length a

instance Collection ProjectsRsp where
    longueur (ProjectsRsp a) = length a

instance Collection VersionsRsp where
    longueur (VersionsRsp a) = length a

