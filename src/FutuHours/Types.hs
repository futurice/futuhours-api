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
    Timereport(..),
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.Extra (FromJSON, ToJSON)
import Data.Aeson.TH    (Options (..), defaultOptions, deriveJSON)
import Data.Char        (toLower)
import Data.Swagger     (ToParamSchema, ToSchema)
import PlanMill.Types   (Identifier (..))
import Servant          (Capture, FromText (..))
import Servant.Docs     (ToSample (..))
import Servant.Docs     (DocCapture (..), ToCapture (..))

import qualified Database.PostgreSQL.Simple.ToField as Postgres
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Data.HashMap.Strict            as HM
import qualified PlanMill.EndPoints.Projects    as PM
import qualified PlanMill.EndPoints.Timereports as PM
import qualified PlanMill.EndPoints.Users       as PM

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

instance Postgres.ToField FUMUsername where
    toField (FUMUsername name) = Postgres.toField name

instance Postgres.ToField PlanmillApiKey where
    toField (PlanmillApiKey key) = Postgres.toField key

instance Postgres.FromField FUMUsername where
    fromField f bs = FUMUsername <$> Postgres.fromField f bs

instance Postgres.FromField PlanmillApiKey where
    fromField f bs = PlanmillApiKey <$> Postgres.fromField f bs

instance Hashable FUMUsername

data Timereport = Timereport
    { timereportId      :: !PM.TimereportId
    , timereportComment :: !(Maybe Text)
    }
    deriving (Generic)

instance ToSample Timereport Timereport where
    toSample _ = Nothing

instance ToJSON Timereport
instance ToSchema Timereport

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

instance ToCapture (Capture "fum-id" FUMUsername) where
    toCapture _ = DocCapture "fum-id" "FUM username"

instance ToSample PlanmillApiKey PlanmillApiKey where
    toSample _ = Just $ PlanmillApiKey "deadbeef12345678"

instance FromText UserId where
    fromText = fmap UserId . fromText

instance FromText FUMUsername where
    fromText = fmap FUMUsername . fromText

