{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module FutuHours.Types (
    Project(..),
    UserId(..),
    FUMUsername(..),
    PlanmillApiKey(..),
    PlanmillUserIdLookupTable,
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Extra (FromJSON, ToJSON)
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

import qualified Data.HashMap.Strict         as HM
import qualified PlanMill.EndPoints.Projects as PM
import qualified PlanMill.EndPoints.Users    as PM

import Orphans ()

newtype UserId = UserId Int
  deriving (Generic) -- TODO: needed for ToParamSchema

newtype FUMUsername = FUMUsername Text deriving (Eq, Show, Generic)
newtype PlanmillApiKey = PlanmillApiKey Text deriving (Eq, Show, Generic)

type PlanmillUserIdLookupTable = HM.HashMap FUMUsername PM.UserId

instance ToParamSchema UserId
instance ToParamSchema FUMUsername

instance ToJSON PlanmillApiKey
instance FromJSON PlanmillApiKey
instance ToSchema PlanmillApiKey

instance Hashable FUMUsername

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

instance ToCapture (Capture "userid" FUMUsername) where
    toCapture _ = DocCapture "userid" "FUM username"

instance ToSample PlanmillApiKey PlanmillApiKey where
    toSample _ = Just $ PlanmillApiKey "deadbeef12345678"

instance FromText UserId where
    fromText = fmap UserId . fromText

instance FromText FUMUsername where
    fromText = fmap FUMUsername . fromText

