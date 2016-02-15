{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import Data.Swagger     (ToParamSchema, ToSchema)
import Data.Text        (Text)
import Data.Typeable    (Typeable)
import GHC.Generics     (Generic)
import PlanMill.Types   (Identifier (..))
import Servant          (Capture, FromText (..))
import Servant.Docs     (ToSample (..))
import Servant.Docs     (DocCapture (..), ToCapture (..))

import qualified PlanMill.EndPoints.Projects as PM

import Orphans ()

newtype UserId = UserId Int
  deriving (Generic) -- TODO: needed for ToParamSchema

instance ToParamSchema UserId

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

instance ToSchema Project

instance ToSample Project Project where
    toSample _ = Just $ Project (Ident 42) "Projekti"

instance ToCapture (Capture "userid" UserId) where
    toCapture _ = DocCapture "userid" "PlanMill userid"

instance FromText UserId where
    fromText = fmap UserId . fromText
