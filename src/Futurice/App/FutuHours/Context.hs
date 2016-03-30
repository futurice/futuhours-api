module Futurice.App.FutuHours.Context where

import Control.Concurrent.STM     (TVar)
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
    }
