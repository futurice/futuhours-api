{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fcontext-stack=30 #-}
module Futurice.App.FutuHours (defaultMain) where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.Async    (async)
import Control.Concurrent.STM      (atomically, newTVarIO, writeTVar)
import Control.Monad               (foldM, forM_)
import Data.Functor.Compose        (Compose (..))
import Data.Pool                   (createPool, withResource)
import Generics.SOP                (I (..), NP (..))
import Network.Wai
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Servant.Cache.Class         (DynMapCache)
import Servant.Futurice
import System.IO                   (hPutStrLn, stderr)
import System.Metrics              (registerGcMetrics, newStore)
import System.Remote.Monitoring    (forkServerWith)
import Network.Wai.Metrics         (registerWaiMetrics, metrics)

import Distribution.Server.Framework.Cron (CronJob (..), JobFrequency (..),
                                           addCronJob, newCron)

import qualified Data.Dependent.Map            as DMap
import qualified Database.PostgreSQL.Simple    as Postgres
import qualified Network.Wai.Handler.Warp      as Warp
import qualified PlanMill                      as PM (Cfg (..))
import qualified Servant.Cache.Internal.DynMap as DynMap

-- FutuHours modules
import Futurice.App.FutuHours.API
import Futurice.App.FutuHours.Config          (Config (..), getConfig)
import Futurice.App.FutuHours.Endpoints
import Futurice.App.FutuHours.PlanMillUserIds (planMillUserIds)
import Futurice.App.FutuHours.Precalc
import Futurice.App.FutuHours.Types

import Futurice.App.FutuHours.Orphans ()

-- | API server
server :: Ctx -> Server FutuHoursAPI
server ctx = pure "Hello to futuhours api"
    :<|> addPlanmillApiKey ctx
    :<|> getBalances ctx
    :<|> (getMissingHoursReport ctx
        :<|> getMissingHoursReportList ctx
        )
    :<|> (getPowerUsers ctx
        :<|> getPowerAbsences ctx
        )
    -- :<|> getTimereports ctx
    :<|> getProjects ctx
    :<|> pure (Envelope empty)
    :<|> getLegacyUsers ctx
    :<|> (\un lte gte -> getLegacyHours lte gte ctx un)

-------------------------------------------------------------------------------
-- Startup
-------------------------------------------------------------------------------

-- | Server with docs and cache and status
server' :: DynMapCache -> Ctx -> Server FutuHoursAPI'
server' cache ctx = futuriceApiServer cache futuhoursAPI (server ctx)

-- | Wai application
app :: DynMapCache -> Ctx -> Application
app cache ctx = simpleCors $ serve futuhoursAPI' (server' cache ctx)

defaultableEndpoints :: [SomeDefaultableEndpoint]
defaultableEndpoints =
    [ SDE powerAbsencesEndpoint
    , SDE powerUsersEndpoint
    , SDE missingHoursListEndpoint
    , SDE balanceReportEndpoint
    ]

ekg :: Int -> IO Middleware
ekg port = do
    s <- newStore
    registerGcMetrics s
    wai <- registerWaiMetrics s
    -- Server is unused
    _ <- forkServerWith s "localhost" port
    pure (metrics wai)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, I'm futuhours-api server"
    Config{..} <- getConfig
    let pmCfg = PM.Cfg
            { PM.cfgUserId  = cfgPlanmillAdminUser
            , PM.cfgApiKey  = cfgPlanmillSignature
            , PM.cfgBaseUrl = cfgPlanmillUrl
            }
    -- Building the Ctx
    postgresPool <- createPool (Postgres.connect cfgPostgresConnInfo) Postgres.close 1 10 5

    -- Ad hoc context to get planmill users
    let ctx' = I cfgDevelopment :* I pmCfg :* I cfgLogLevel :* Nil
    planmillUserLookup <- withResource postgresPool $ \conn ->
        planMillUserIds ctx' conn cfgFumToken cfgFumBaseurl cfgFumList
    planmillUserLookupTVar <- newTVarIO planmillUserLookup

    -- Context without precalc endpoints
    let ctx'' = Ctx
          { ctxDevelopment        = cfgDevelopment
          , ctxPlanmillCfg        = pmCfg
          , ctxPostgresPool       = postgresPool
          , ctxPlanmillUserLookup = planmillUserLookupTVar
          , ctxPrecalcEndpoints   = DMap.empty
          , ctxLogLevel           = cfgLogLevel
          }

    precalcEndpoints <-
        let f m (SDE de) = do
                hPutStrLn stderr $ "Initialising " ++ show (defEndTag de)
                a <- async (defEndDefaultParsedParam de >>= defEndAction de ctx'')
                v <- newTVarIO a
                pure $ DMap.insert (defEndTag de) (Compose v) m
        in foldM f DMap.empty defaultableEndpoints

    let ctx = ctx'' { ctxPrecalcEndpoints = precalcEndpoints }

    -- Cron
    cron <- newCron ()

    -- Cron: planmill users

    addCronJob cron $ CronJob "Planmill Users" (TestJobFrequency $ 7 * 60) True $ do
        planmillUserLookup' <- withResource postgresPool $ \conn ->
            planMillUserIds ctx' conn cfgFumToken cfgFumBaseurl cfgFumList
        atomically $ writeTVar planmillUserLookupTVar planmillUserLookup'

    -- Cron: preprocessed endpoints
    forM_ (zip defaultableEndpoints [13, 17, 19, 23]) $ \(SDE de, d) -> do
        hPutStrLn stderr $ "Queueing cron " ++ show (defEndTag de)
        addCronJob cron $ CronJob (show $ defEndTag de) (TestJobFrequency $ d * 60) True $
            cronEndpoint de ctx

    metricsMiddleware <- ekg cfgEkgPort

    -- Startup
    cache <- DynMap.newIO
    let app' = metricsMiddleware $ app cache ctx
    hPutStrLn stderr "Now I'll start the webservice"
    Warp.run cfgPort app'
