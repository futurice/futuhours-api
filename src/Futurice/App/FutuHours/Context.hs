module Futurice.App.FutuHours.Context where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM     (TVar)
import Control.Monad.Logger       (LogLevel (..), LoggingT, filterLogger,
                                   runStderrLoggingT)
import Data.Dependent.Map         (DMap)
import Data.Functor.Compose       (Compose)
import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import PlanMill                   (Cfg (..))

import Futurice.App.FutuHours.Types

-- | We probably will have some
data Ctx = Ctx
    { ctxDevelopment        :: !Bool
    , ctxPlanmillCfg        :: !Cfg
    , ctxPostgresPool       :: !(Pool Connection)
    , ctxPlanmillUserLookup :: !PlanmillUserIdLookupTable -- ^ *TODO:* refresh this from time to time
    , ctxPrecalcEndpoints   :: !(DMap EndpointTag (Compose TVar Maybe))
    , ctxLogLevel           :: !LogLevel
    }

runFutuhoursLoggingT :: MonadIO m => Ctx -> LoggingT m a -> m a
runFutuhoursLoggingT ctx l =
    runStderrLoggingT $ filterLogger p l
  where
    p _ level = level >= ctxLogLevel ctx
