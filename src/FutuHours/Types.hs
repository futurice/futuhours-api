{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module FutuHours.Types (
    TimeReport,
    User,
    Holiday,
    Project(..),
    Task,
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Extra (SymTag (..))
import Data.Aeson.TH    (Options (..), defaultOptions, deriveJSON)
import Data.Char        (toLower)
import Data.Text        (Text)

import qualified PlanMill.EndPoints.Projects as PM

-- TODO:
type TimeReport = SymTag "TimeReport"
type User = SymTag "User"
type Holiday = SymTag "Holiday"
type Task = SymTag "Task"

data Project = Project
    { projectId   :: !PM.ProjectId
    , projectName :: !Text
    }

$(deriveJSON
    defaultOptions{ fieldLabelModifier = map toLower . drop 7 }
    ''Project)
