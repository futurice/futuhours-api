{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#define OVERLAPPING_
#else
#define OVERLAPPING_ {-# OVERLAPPING #-}
#endif
module Futurice.App.FutuHours.Context where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM     (TVar)
import Control.Lens               (Lens', lens)
import Control.Monad.Logger       (LogLevel (..), LoggingT, filterLogger,
                                   runStderrLoggingT)
import Data.Dependent.Map         (DMap)
import Data.Functor.Compose       (Compose)
import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Generics.SOP               (I (..), NP (..))
import Generics.SOP.Lens          (headLens, tailLens, uni)
import PlanMill                   (Cfg (..))

import Futurice.App.FutuHours.Types

-- | We probably will have some
data Ctx = Ctx
    { ctxDevelopment        :: !Development
    , ctxPlanmillCfg        :: !Cfg
    , ctxPostgresPool       :: !(Pool Connection)
    , ctxPlanmillUserLookup :: !PlanmillUserIdLookupTable -- ^ *TODO:* refresh this from time to time
    , ctxPrecalcEndpoints   :: !(DMap EndpointTag (Compose TVar Maybe))
    , ctxLogLevel           :: !LogLevel
    }

runFutuhoursLoggingT
    :: (MonadIO m, HasLogLevel env)
    => env -> LoggingT m a -> m a
runFutuhoursLoggingT env l =
    runStderrLoggingT $ filterLogger p l
  where
    p _ level = level >= (env ^. logLevel)

class HasDevelopment a where development :: Lens' a Development
class HasPlanmillCfg a where planmillCfg :: Lens' a Cfg
class HasLogLevel a where logLevel :: Lens' a LogLevel

instance HasDevelopment Ctx where
    development = lens ctxDevelopment $ \c x -> c { ctxDevelopment = x }
instance HasPlanmillCfg Ctx where
    planmillCfg = lens ctxPlanmillCfg $ \c x -> c { ctxPlanmillCfg = x }
instance HasLogLevel Ctx where
    logLevel = lens ctxLogLevel $ \c x -> c { ctxLogLevel = x }

class IsElem a xs where
    value :: Lens' (NP I xs) a
instance OVERLAPPING_ IsElem a (a ': xs) where
    value = headLens . uni
instance IsElem a xs => IsElem a (b ': xs) where
    value = tailLens . value

instance IsElem Development xs => HasDevelopment (NP I xs) where
    development = value
instance IsElem Cfg xs => HasPlanmillCfg (NP I xs) where
    planmillCfg = value
instance IsElem LogLevel xs => HasLogLevel (NP I xs) where
    logLevel = value
