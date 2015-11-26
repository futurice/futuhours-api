{-# LANGUAGE GADTs #-}
module FutuHours.Config (
    Config(..),
    getConfig,
    defaultPort,
    ) where

import Prelude        ()
import Prelude.Compat

import qualified Data.Text          as T
import           System.Environment (lookupEnv)
import           Text.Read          (readMaybe)

-- | TODO: split config into two parts
data Config = Config
    { cfgUnused :: !T.Text
      -- ^ Flowdock organisation
    , cfgPort   :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

getConfig :: IO Config
getConfig =
    Config <$> parseEnvVar "UNUSED"
           <*> parseEnvVarWithDefault "PORT" defaultPort

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

instance FromEnvVar T.Text where
    fromEnvVar = Just . T.pack

instance FromEnvVar Int where
    fromEnvVar = readMaybe
