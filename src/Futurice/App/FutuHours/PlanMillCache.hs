module Futurice.App.FutuHours.PlanMillCache where

import Futurice.Prelude
import Prelude          ()

import qualified PlanMill as PM

class (Applicative m, Monad m) => MonadPlanMillCached m where
    cachedTimereports :: PM.Interval Day -> PM.UserId -> m PM.Timereports
