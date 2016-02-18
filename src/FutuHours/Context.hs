module FutuHours.Context where

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import PlanMill                   (Cfg (..))

import FutuHours.Types

-- | We probably will have some
data Context = Context
    { ctxPlanmillCfg        :: !Cfg
    , ctxPostgresPool       :: !(Pool Connection)
    , ctxPlanmillUserLookup :: !PlanmillUserIdLookupTable -- ^ *TODO:* refresh this from time to time
    }
