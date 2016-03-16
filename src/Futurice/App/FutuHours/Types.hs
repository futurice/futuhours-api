{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.FutuHours.Types (
    Project(..),
    UserId(..),
    FUMUsername(..),
    FUMUsernamesParam(..),
    PlanmillApiKey(..),
    PlanmillUserLookupTable,
    PlanmillUserIdLookupTable,
    Timereport(..),
    Balance(..),
    Envelope(..),
    User(..),
    Hour(..),
    -- * Reports
    -- ** Missing hours
    MissingHoursReport(..),
    unMissingHoursReport,
    MissingHours(..),
    MissingHour(..),
    -- * Power
    PowerUser(..),
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Arrow     (first)
import Data.Aeson.Extra  (M, FromJSON, ToJSON (..), Value (..), object, (.=))
import Data.Aeson.TH     (Options (..), defaultOptions, deriveJSON)
import Data.Char         (toLower)
import Data.Csv          (DefaultOrdered (..), ToNamedRecord (..))
import Data.Swagger      (ToParamSchema, ToSchema (..))
import Futurice.Generics (sopDeclareNamedSchema, sopHeaderOrder, sopToJSON,
                          sopToNamedRecord)
import Servant           (Capture, FromText (..))
import Servant.Docs      (DocCapture (..), ToCapture (..))

import qualified Data.Aeson                           as Aeson
import qualified Data.Aeson.Types                     as Aeson
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Swagger                         as Swagger
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified PlanMill                             as PM

import Futurice.App.FutuHours.Orphans ()

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

camelTo :: String -> String
camelTo = Aeson.camelTo '_'

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

getFUMUsername :: FUMUsername -> Text
getFUMUsername (FUMUsername name) = name

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

-- | List of users
newtype FUMUsernamesParam = FUMUsernamesParam
    { getFUMUsernamesParam :: [FUMUsername] }

instance FromText FUMUsernamesParam where
    fromText = Just . FUMUsernamesParam . map FUMUsername . T.words

instance ToParamSchema FUMUsernamesParam where
    toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy Text) -- TODO: pattern

-------------------------------------------------------------------------------
-- PlanmillApiKey
-------------------------------------------------------------------------------

newtype PlanmillApiKey = PlanmillApiKey Text
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON PlanmillApiKey
instance FromJSON PlanmillApiKey
instance ToSchema PlanmillApiKey

instance Postgres.ToField PlanmillApiKey where
    toField (PlanmillApiKey key) = Postgres.toField key

instance Postgres.FromField PlanmillApiKey where
    fromField f bs = PlanmillApiKey <$> Postgres.fromField f bs

type PlanmillUserLookupTable = HM.HashMap FUMUsername PM.User
type PlanmillUserIdLookupTable = HM.HashMap FUMUsername PM.UserId

-------------------------------------------------------------------------------
-- Timereport
-------------------------------------------------------------------------------

data Timereport = Timereport
    { timereportId      :: !PM.TimereportId
    , timereportComment :: !(Maybe Text)
    }
    deriving (Generic)

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

-------------------------------------------------------------------------------
-- Legacy hour marking
-------------------------------------------------------------------------------

data Hour = Hour
    { hourAbsence         :: !Bool
    , hourBillable        :: !Bool
    , hourDay             :: !Day
    , hourDescription     :: !Text
    , hourEditable        :: !Bool
    , hourHours           :: !Double
    , hourId              :: !PM.TimereportId
    , hourProjectId       :: !PM.ProjectId
    , hourProjectCategory :: !Int
    , hourProjectName     :: !Text
    , hourStatus          :: !Int
    , hourTaskId          :: !PM.TaskId
    , hourTaskName        :: !Text
    , hourUserId          :: !PM.UserId
    , hourUser            :: !Text
    }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance ToJSON Hour where
  toJSON = Aeson.genericToJSON opts
      where
        opts = Aeson.defaultOptions
            { Aeson.fieldLabelModifier = camelTo . drop 4
            }

instance ToSchema Hour where
    declareNamedSchema = Swagger.genericDeclareNamedSchema opts
      where
        opts = Swagger.defaultSchemaOptions
            { Swagger.fieldLabelModifier = camelTo . drop 4
            }

-------------------------------------------------------------------------------
-- Reports
-------------------------------------------------------------------------------

newtype MissingHoursReport = MissingHoursReport (HashMap FUMUsername MissingHours)
    deriving (Show)

-- | TODO: rename to @getMissingHoursReport@ or so.
unMissingHoursReport :: MissingHoursReport -> HashMap FUMUsername MissingHours
unMissingHoursReport (MissingHoursReport r) = r

instance ToJSON MissingHoursReport where
    toJSON (MissingHoursReport mhs) = toJSON . HM.fromList . map (first getFUMUsername) . HM.toList $ mhs

-- | TODO:
instance ToSchema MissingHoursReport where
    declareNamedSchema _ = pure $
        Swagger.NamedSchema (Just "Missing Hours report") mempty

data MissingHours = MissingHours
   { missingHoursName     :: !Text
   , missingHoursTeam     :: !Text
   , missingHoursContract :: !Text
   , missingHoursDays     :: !(M (Map Day Double)) -- Hours per day
   }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON MissingHours where
  toJSON = Aeson.genericToJSON opts
      where
        opts = Aeson.defaultOptions
            { Aeson.fieldLabelModifier = camelTo . drop 12
            }

instance ToSchema MissingHours where
    declareNamedSchema = Swagger.genericDeclareNamedSchema opts
      where
        opts = Swagger.defaultSchemaOptions
            { Swagger.fieldLabelModifier = camelTo . drop 12
            }

data MissingHour = MissingHour
    { missingHourName     :: !Text
    , missingHourTeam     :: !Text
    , missingHourContract :: !Text
    , missingHourDay      :: !Day
    , missingHourHours    :: !Double
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''MissingHour

instance DefaultOrdered MissingHour where headerOrder = sopHeaderOrder
instance ToNamedRecord MissingHour where toNamedRecord = sopToNamedRecord
instance ToJSON MissingHour where toJSON = sopToJSON
instance ToSchema MissingHour where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Power
-------------------------------------------------------------------------------

data PowerUser = PowerUser
    { powerUserFirst :: !Text
    , powerUserLast  :: !Text
    , powerUserTeam  :: !Text
    , powerUserStart :: !(Maybe Day)
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerUser

instance DefaultOrdered PowerUser where headerOrder = sopHeaderOrder
instance ToNamedRecord PowerUser where toNamedRecord = sopToNamedRecord
instance ToJSON PowerUser where toJSON = sopToJSON
instance ToSchema PowerUser where declareNamedSchema = sopDeclareNamedSchema
