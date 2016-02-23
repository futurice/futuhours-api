{-# LANGUAGE GADTs #-}
module FutuHours.Config (
    Config(..),
    getConfig,
    defaultPort,
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.ByteString    (ByteString)
import System.Environment (lookupEnv)
import Text.Read          (readMaybe)

import Database.PostgreSQL.Simple     (ConnectInfo (..))
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)

import qualified Data.Text                 as T
import qualified FUM
import qualified PlanMill.Types.Auth       as PM
import qualified PlanMill.Types.Identifier as PM
import qualified PlanMill.Types.User       as PM

-- | TODO: split config into two parts
data Config = Config
    { cfgPlanmillUrl       :: !String
      -- ^ Planmill url
    , cfgPlanmillAdminUser :: !PM.UserId
      -- ^ Admin user id
    , cfgPlanmillSignature :: !PM.ApiKey
      -- ^ Token
    , cfgPostgresConnInfo  :: !ConnectInfo
      -- ^ Postgres
    , cfgFumToken          :: !FUM.AuthToken
    , cfgFumBaseurl        :: !FUM.BaseUrl
    , cfgFumList           :: !FUM.ListName
    , cfgPort              :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

getConfig :: IO Config
getConfig =
    Config <$> parseEnvVar "PLANMILL_BASEURL"
           <*> parseEnvVar "PLANMILL_ADMIN"
           <*> parseEnvVar "PLANMILL_SIGNATURE"
           <*> getConnectInfo
           <*> parseEnvVar "FUM_TOKEN"
           <*> parseEnvVar "FUM_BASEURL"
           <*> parseEnvVar "FUM_LISTNAME"
           <*> parseEnvVarWithDefault "PORT" defaultPort

getConnectInfo :: IO ConnectInfo
getConnectInfo = f
    <$> parseEnvVar "POSTGRES_URL"
    <*> parseEnvVar "POSTGRES_PASS"
  where
    f connInfo pass = connInfo { connectPassword = pass }

defaultPort :: Int
defaultPort = 8000

-- | Class to parse env variables
class FromEnvVar a where
    fromEnvVar :: String -> Maybe a

-- | Parse required environment variable
parseEnvVar :: FromEnvVar a
            => String  -- ^ Environment variable
            -> IO a
parseEnvVar var =
    parseEnvVarWithDefault var (error $ "No environment variable " ++ var)

-- | Parse optional environment variable.
-- Will fail if variable is present, but is of invalid format.
parseEnvVarWithDefault :: FromEnvVar a
                       => String  -- ^ Environment variable
                       -> a       -- ^ Default value
                       -> IO a
parseEnvVarWithDefault var def = do
    val <- lookupEnv var
    case val of
        Nothing   -> pure def
        Just val' -> case fromEnvVar val' of
            Nothing -> error $
               "Cannot parse environment variable: " ++ var ++ " -- " ++ val'
            Just x  -> pure x

-- | This instance is temporary.
instance a ~ Char => FromEnvVar [a] where
    fromEnvVar = Just

instance FromEnvVar ByteString where
    fromEnvVar = Just . fromString

instance FromEnvVar T.Text where
    fromEnvVar = Just . T.pack

instance FromEnvVar PM.ApiKey where
    fromEnvVar = fmap PM.ApiKey . fromEnvVar

instance FromEnvVar (PM.Identifier a) where
    fromEnvVar = fmap PM.Ident . fromEnvVar

instance FromEnvVar FUM.AuthToken where
    fromEnvVar = fmap FUM.AuthToken . fromEnvVar

instance FromEnvVar FUM.BaseUrl where
    fromEnvVar = Just . FUM.BaseUrl

instance FromEnvVar FUM.ListName where
    fromEnvVar = Just . FUM.ListName

instance FromEnvVar ConnectInfo where
    fromEnvVar = parseDatabaseUrl

instance FromEnvVar Word64 where
    fromEnvVar = readMaybe

instance FromEnvVar Int where
    fromEnvVar = readMaybe
