{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
module FutuHours.Types (
    TimeReport,
    User,
    Holiday,
    Project(..),
    Task,
    UserId(..),
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Extra (SymTag (..))
import Data.Aeson.TH    (Options (..), defaultOptions, deriveJSON)
import Data.Char        (toLower)
import Data.Hashable    (Hashable)
import Data.Text        (Text)
import Data.Typeable    (Typeable)
import GHC.Generics     (Generic)

import qualified PlanMill.EndPoints.Projects as PM

newtype UserId = UserId Int

-- TODO:
type TimeReport = SymTag "TimeReport"
type User = SymTag "User"
type Holiday = SymTag "Holiday"
type Task = SymTag "Task"

data Project = Project
    { projectId   :: !PM.ProjectId
    , projectName :: !Text
    }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance Hashable Project

$(deriveJSON
    defaultOptions{ fieldLabelModifier = map toLower . drop 7 }
    ''Project)
