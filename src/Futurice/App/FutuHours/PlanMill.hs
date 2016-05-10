{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.FutuHours.PlanMill (runHaxl) where

import Futurice.Prelude
import Prelude          ()

import Haxl.Core                  (State)
import Haxl.Typed
import Database.PostgreSQL.Simple (Connection)

import Futurice.App.FutuHours.Context

import qualified Futurice.App.FutuHours.PlanmillCacheDataSource as PlanmillCacheDataSource
import qualified Futurice.App.FutuHours.PlanmillDataSource      as PlanmillDataSource

runHaxl
    :: forall a env.
       ( HasDevelopment env, HasPlanmillCfg env, HasLogLevel env
       )
    => env
    -> Connection
    -> GenTyHaxl '[PlanmillDataSource.PlanmillRequest, PlanmillCacheDataSource.PlanmillCacheRequest] () a
    -> IO a
runHaxl env conn haxl  = do
    pmDS <- PlanmillDataSource.initDataSource env conn
    pmcDS <- PlanmillCacheDataSource.initDataSource env conn :: IO (State PlanmillCacheDataSource.PlanmillCacheRequest)
    environment <- initTyEnv (pmDS `tyStateSet`
                              pmcDS `tyStateSet` tyStateEmpty) ()
    runTyHaxl environment haxl
