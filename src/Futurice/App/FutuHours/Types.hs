{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
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
    Envelope(..),
    User(..),
    Hour(..),
    -- * Reports
    -- ** Missing hours
    MissingHoursReport(..),
    unMissingHoursReport,
    MissingHours(..),
    MissingHour(..),
    -- ** Balance
    BalanceReport(..),
    Balance(..),
    -- * Power
    PowerUser(..),
    PowerAbsence(..),
    -- * Precalculated endpoints
    EndpointTag(..),
    -- * Flags
    Development(..),
    -- * Random
    reverseLookup,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Arrow     (first)
import Data.Aeson.Extra  (FromJSON (..), M, ToJSON (..), Value (..), object,
                          (.=))
import Data.Csv          (DefaultOrdered (..), FromRecord (..), ToField (..),
                          ToNamedRecord (..))
import Data.GADT.Compare ((:~:) (..), GCompare (..), GEq (..), GOrdering (..))
import Data.Swagger      (ToParamSchema, ToSchema (..))
import Futurice.Generics (sopDeclareNamedSchema, sopHeaderOrder, sopParseJSON,
                          sopParseRecord, sopToJSON, sopToNamedRecord)
import Lucid
import Servant           (Capture, FromHttpApiData (..))
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
#if MIN_VERSION_aeson(0,10,0)
camelTo = Aeson.camelTo2 '_'
#else
camelTo = Aeson.camelTo '_'
#endif

-------------------------------------------------------------------------------
-- Development
-------------------------------------------------------------------------------

data Development
    = Development
    | Production
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- UserId - deprecated
-------------------------------------------------------------------------------

newtype UserId = UserId Int
  deriving (Generic) -- TODO: needed for ToParamSchema

instance ToParamSchema UserId

instance ToCapture (Capture "userid" UserId) where
    toCapture _ = DocCapture "userid" "PlanMill userid"

instance FromHttpApiData UserId where
    parseUrlPiece = fmap UserId . parseUrlPiece

-------------------------------------------------------------------------------
-- FUMUsername
-------------------------------------------------------------------------------

newtype FUMUsername = FUMUsername Text
    deriving (Eq, Ord, Show, Typeable, Generic)

getFUMUsername :: FUMUsername -> Text
getFUMUsername (FUMUsername name) = name

instance ToJSON FUMUsername where
    toJSON (FUMUsername n) = toJSON n
#if MIN_VERSION_aeson(0,10,0)
    toEncoding (FUMUsername n) = toEncoding n
#endif
instance ToSchema FUMUsername
instance ToParamSchema FUMUsername

instance ToCapture (Capture "fum-id" FUMUsername) where
    toCapture _ = DocCapture "fum-id" "FUM username"

instance FromHttpApiData FUMUsername where
    parseUrlPiece = fmap FUMUsername . parseUrlPiece

instance Postgres.ToField FUMUsername where
    toField (FUMUsername name) = Postgres.toField name

instance Postgres.FromField FUMUsername where
    fromField f bs = FUMUsername <$> Postgres.fromField f bs

instance Hashable FUMUsername

instance ToField FUMUsername where
    toField = toField.  getFUMUsername

instance ToHtml FUMUsername where
    toHtml = toHtml . getFUMUsername
    toHtmlRaw = toHtmlRaw . getFUMUsername

-- | List of users
newtype FUMUsernamesParam = FUMUsernamesParam
    { getFUMUsernamesParam :: [FUMUsername] }
  deriving (Eq)

instance FromHttpApiData FUMUsernamesParam where
    parseUrlPiece = Right . FUMUsernamesParam . map FUMUsername . T.words

instance ToParamSchema FUMUsernamesParam where
    toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy Text) -- TODO: pattern

-------------------------------------------------------------------------------
-- PlanmillApiKey
-------------------------------------------------------------------------------

newtype PlanmillApiKey = PlanmillApiKey Text
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON PlanmillApiKey where
    toJSON (PlanmillApiKey k) = toJSON k
#if MIN_VERSION_aeson(0,10,0)
    toEncoding (PlanmillApiKey k) = toEncoding k
#endif
instance FromJSON PlanmillApiKey
instance ToSchema PlanmillApiKey

instance Postgres.ToField PlanmillApiKey where
    toField (PlanmillApiKey key) = Postgres.toField key

instance Postgres.FromField PlanmillApiKey where
    fromField f bs = PlanmillApiKey <$> Postgres.fromField f bs

type PlanmillUserLookupTable = HM.HashMap FUMUsername PM.User
type PlanmillUserIdLookupTable = HM.HashMap FUMUsername PM.UserId

reverseLookup :: (Eq v, Hashable v) => v -> HM.HashMap k v -> Maybe k
reverseLookup pid hm = HM.lookup pid revHm
  where
    revHm = HM.fromList . map swap . HM.toList $ hm
    swap (a, b) = (b, a)

-------------------------------------------------------------------------------
-- Timereport
-------------------------------------------------------------------------------

data Timereport = Timereport
    { timereportId      :: !PM.TimereportId
    , timereportComment :: !(Maybe Text)
    }
    deriving (Generic)

deriveGeneric ''Timereport

instance ToJSON Timereport where toJSON = sopToJSON
instance ToSchema Timereport where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Balance
-------------------------------------------------------------------------------

data Balance = Balance
    { balanceUser         :: !FUMUsername
    , balanceName         :: !Text
    , balanceHours        :: !Int
    , balanceMissingHours :: !Int
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''Balance

instance ToJSON Balance where toJSON = sopToJSON
instance ToSchema Balance where declareNamedSchema = sopDeclareNamedSchema

data BalanceReport = BalanceReport
    { balanceReportData :: !(Vector Balance)
    }

deriveGeneric ''BalanceReport

instance ToJSON BalanceReport where toJSON = sopToJSON
instance ToSchema BalanceReport where declareNamedSchema = sopDeclareNamedSchema

instance ToHtml BalanceReport where
    toHtml (BalanceReport vs) = doctypehtml_ $ do
      head_ $ do
          title_ $ "Hour balance report"
          style_ $ T.unlines
              [ ".emphasize td { font-weight: bold; background: #eee }"
              , ".emphasize2 td { font-style: italic; background: #efe; }"
              ]
      body_ $ do
          h1_ $ "Hour balance report"
          table_ $ do
              tr_ $ do
                  th_ "Username"
                  th_ "Name"
                  th_ "Balance hours"
                  th_ "Missing hours"
                  th_ "Sum (balance + missing)"
              flip traverse_ (sortBy (compare `on` balanceUser) . toList $ vs) $ \(Balance username name hours missing) -> do
                  let diff = hours + missing
                  let cls | diff <= -20 || diff >= 40   = "emphasize"
                          | hours <= -20 || hours >= 40 = "emphasize2"
                          | otherwise                   = "normal"
                  tr_ [class_ cls] $ do
                      td_ $ toHtmlRaw username
                      td_ $ toHtmlRaw name
                      td_ $ toHtmlRaw $ show hours
                      td_ $ toHtmlRaw $ show missing
                      td_ $ toHtmlRaw $ show diff
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------
-- Project
-------------------------------------------------------------------------------

data Project = Project
    { projectId   :: !PM.ProjectId
    , projectName :: !Text
    }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance Hashable Project

deriveGeneric ''Project

instance ToJSON Project where toJSON = sopToJSON
instance ToSchema Project where declareNamedSchema = sopDeclareNamedSchema

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
instance FromRecord MissingHour where parseRecord = sopParseRecord
instance ToJSON MissingHour where toJSON = sopToJSON
instance FromJSON MissingHour where parseJSON = sopParseJSON
instance ToSchema MissingHour where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Power
-------------------------------------------------------------------------------

data PowerUser = PowerUser
    { powerUserUsername :: !FUMUsername
    , powerUserFirst    :: !Text
    , powerUserLast     :: !Text
    , powerUserTeam     :: !Text
    , powerUserStart    :: !(Maybe Day)
    , powerUserEnd      :: !(Maybe Day)
    , powerUserActive   :: !Text
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerUser

instance DefaultOrdered PowerUser where headerOrder = sopHeaderOrder
instance ToNamedRecord PowerUser where toNamedRecord = sopToNamedRecord
instance ToJSON PowerUser where toJSON = sopToJSON
instance ToSchema PowerUser where declareNamedSchema = sopDeclareNamedSchema

data PowerAbsence = PowerAbsence
    { powerAbsenceUsername     :: !(Maybe FUMUsername)
    , powerAbsenceStart        :: !Day
    , powerAbsenceEnd          :: !Day
    , powerAbsencePlanmillId   :: !PM.AbsenceId
    , powerAbsenceCapacities   :: !(M (Map Day Double))
    , powerAbsenceBusinessDays :: !Int
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerAbsence

instance DefaultOrdered PowerAbsence where headerOrder = sopHeaderOrder
instance ToNamedRecord PowerAbsence where toNamedRecord = sopToNamedRecord
instance ToJSON PowerAbsence where toJSON = sopToJSON
instance ToSchema PowerAbsence where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Precalculated Endpoints
-------------------------------------------------------------------------------

data EndpointTag a where
    EMissingHoursList :: EndpointTag [MissingHour]
    -- missing hours from beginning of the previous month till today
    EPowerUsers       :: EndpointTag (Vector PowerUser)
    -- Users in planmill with some additional information
    EPowerAbsences    :: EndpointTag (Vector PowerAbsence)
    -- Absences in next 365 days
    EBalanceReport    :: EndpointTag BalanceReport
    deriving (Typeable)

instance GEq EndpointTag where
    geq EMissingHoursList EMissingHoursList = Just Refl
    geq EPowerUsers       EPowerUsers       = Just Refl
    geq EPowerAbsences    EPowerAbsences    = Just Refl
    geq EBalanceReport    EBalanceReport    = Just Refl
    geq _ _ = Nothing

instance GCompare EndpointTag where
    gcompare EMissingHoursList EMissingHoursList = GEQ
    gcompare EMissingHoursList _                 = GLT
    gcompare _                 EMissingHoursList = GGT
    gcompare EPowerUsers       EPowerUsers       = GEQ
    gcompare EPowerUsers       _                 = GLT
    gcompare _                 EPowerUsers       = GGT
    gcompare EPowerAbsences    EPowerAbsences    = GEQ
    gcompare EPowerAbsences    _                 = GLT
    gcompare _                 EPowerAbsences    = GGT
    gcompare EBalanceReport    EBalanceReport    = GEQ

deriving instance Show (EndpointTag a)
