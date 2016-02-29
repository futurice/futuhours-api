{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.FutuHours.Types (
    Project(..),
    UserId(..),
    FUMUsername(..),
    PlanmillApiKey(..),
    PlanmillUserIdLookupTable,
    Timereport(..),
    Balance(..),
    Envelope(..),
    User(..),
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.Extra (FromJSON, ToJSON (..), Value (..), object, (.=))
import Data.Aeson.TH    (Options (..), defaultOptions, deriveJSON)
import Data.Char        (toLower)
import Data.Swagger     (ToParamSchema, ToSchema(..))
import PlanMill.Types   (Identifier (..))
import Servant          (Capture, FromText (..))
import Servant.Docs     (ToSample (..))
import Servant.Docs     (DocCapture (..), ToCapture (..))

import qualified Data.HashMap.Strict                  as HM
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified PlanMill.EndPoints.Projects          as PM
import qualified PlanMill.EndPoints.Timereports       as PM
import qualified PlanMill.EndPoints.Users             as PM

import qualified Data.Aeson.Types as Aeson
import qualified Data.Swagger as Swagger

import Futurice.App.FutuHours.Orphans ()

-------------------------------------------------------------------------------
-- UserId - deprecated
-------------------------------------------------------------------------------

newtype UserId = UserId Int
  deriving (Generic) -- TODO: needed for ToParamSchema

instance ToParamSchema UserId

instance ToCapture (Capture "userid" UserId) where
    toCapture _ = DocCapture "userid" "PlanMill userid"
instance FromText UserId where
    fromText = fmap UserId . fromText

-------------------------------------------------------------------------------
-- FUMUsername
-------------------------------------------------------------------------------

newtype FUMUsername = FUMUsername Text
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON FUMUsername
instance ToSchema FUMUsername
instance ToParamSchema FUMUsername

instance ToCapture (Capture "fum-id" FUMUsername) where
    toCapture _ = DocCapture "fum-id" "FUM username"

instance FromText FUMUsername where
    fromText = fmap FUMUsername . fromText

instance Postgres.ToField FUMUsername where
    toField (FUMUsername name) = Postgres.toField name

instance Postgres.FromField FUMUsername where
    fromField f bs = FUMUsername <$> Postgres.fromField f bs

instance Hashable FUMUsername

-------------------------------------------------------------------------------
-- PlanmillApiKey
-------------------------------------------------------------------------------

newtype PlanmillApiKey = PlanmillApiKey Text
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON PlanmillApiKey
instance FromJSON PlanmillApiKey
instance ToSchema PlanmillApiKey

instance ToSample PlanmillApiKey PlanmillApiKey where
    toSample _ = Just $ PlanmillApiKey "deadbeef12345678"

instance Postgres.ToField PlanmillApiKey where
    toField (PlanmillApiKey key) = Postgres.toField key

instance Postgres.FromField PlanmillApiKey where
    fromField f bs = PlanmillApiKey <$> Postgres.fromField f bs

type PlanmillUserIdLookupTable = HM.HashMap FUMUsername PM.UserId

-------------------------------------------------------------------------------
-- Timereport
-------------------------------------------------------------------------------

data Timereport = Timereport
    { timereportId      :: !PM.TimereportId
    , timereportComment :: !(Maybe Text)
    }
    deriving (Generic)

instance ToSample Timereport Timereport where
    toSample _ = Nothing

instance ToJSON Timereport
instance ToSchema Timereport

-------------------------------------------------------------------------------
-- Balance
-------------------------------------------------------------------------------

data Balance = Balance
    { balanceUser  :: !FUMUsername
    , balanceHours :: !Int
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON Balance
instance ToSchema Balance

instance ToSample Balance Balance where
    toSample _ = Just $ Balance (FUMUsername "user") 42

-------------------------------------------------------------------------------
-- Project
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Envelope
-------------------------------------------------------------------------------

newtype Envelope a = Envelope { fromEnvelope :: Vector a }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance ToJSON a => ToJSON (Envelope a) where
    toJSON (Envelope x) = object
        [ "meta" .= object
            [ "next"        .= Null
            , "previous"    .= Null
            , "offset"      .= (0 :: Int)
            , "total_count" .= length x
            , "limit"       .= (1000 :: Int)
            ]
        , "objects" .= x
        ]

instance ToSchema a => ToSchema (Envelope a) where

instance ToSample a b => ToSample (Envelope a) (Envelope b) where
    toSample _ = Nothing

-------------------------------------------------------------------------------
-- Legacy user
-------------------------------------------------------------------------------

data User = User
    { userFirstName        :: !Text
    , userDefaultWorkHours :: !Double
    , userHolidaysDaysLeft :: !Int
    , userBalance          :: !Int
    , userEmployeeType     :: !Text
    }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance ToJSON User where
  toJSON = Aeson.genericToJSON opts
      where
        opts = Aeson.defaultOptions
            { Aeson.fieldLabelModifier = camelTo . drop 4
            }

instance ToSchema User where
    declareNamedSchema = Swagger.genericDeclareNamedSchema opts
      where
        opts = Swagger.defaultSchemaOptions
            { Swagger.fieldLabelModifier = camelTo . drop 4
            }

camelTo :: String -> String
camelTo = Aeson.camelTo '_'

instance ToSample User User where
    toSample _ = Nothing
