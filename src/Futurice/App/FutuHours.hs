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

import Control.Concurrent.STM      (atomically, newTVar)
import Control.Monad               (foldM, forM_)
import Data.Functor.Compose        (Compose (..))
import Data.Pool                   (createPool, withResource)
import Network.Wai
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Servant.Cache.Class         (DynMapCache)
import Servant.Futurice
import System.IO                   (hPutStrLn, stderr)

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
    :<|> getTimereports ctx
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

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, I'm futuhours-api server"
    Config{..} <- getConfig
    let pmCfg = PM.Cfg
            { PM.cfgUserId  = cfgPlanmillAdminUser
            , PM.cfgApiKey  = cfgPlanmillSignature
            , PM.cfgBaseUrl = cfgPlanmillUrl
            }
    -- Ctx
    postgresPool <- createPool (Postgres.connect cfgPostgresConnInfo) Postgres.close 1 10 5
    planmillUserLookup <- withResource postgresPool $ \conn ->
        planMillUserIds cfgDevelopment pmCfg conn cfgFumToken cfgFumBaseurl cfgFumList

    precalcEndpoints <- atomically $
        let f m (SDE de) = do
                v <- newTVar Nothing
                pure $ DMap.insert (defEndTag de) (Compose v) m
        in foldM f DMap.empty defaultableEndpoints

    let ctx = Ctx
          { ctxDevelopment = cfgDevelopment
          , ctxPlanmillCfg = pmCfg
          , ctxPostgresPool = postgresPool
          , ctxPlanmillUserLookup = planmillUserLookup
          , ctxPrecalcEndpoints = precalcEndpoints
          , ctxLogLevel = cfgLogLevel
          }

    -- Cron
    cron <- newCron ()
    forM_ (zip defaultableEndpoints [1, 30, 120]) $ \(SDE de, d) -> do
        addCronJob cron $ CronJob "some job" (TestJobFrequency $ 30*60) False $
            cronEndpoint de ctx
        addCronJob cron $ CronJob "first some job" (TestJobFrequency d) True $
            cronEndpoint de ctx

    -- Startup
    cache <- DynMap.newIO
    let app' = app cache ctx
    hPutStrLn stderr "Now I'll start the webservice"
    Warp.run cfgPort app'
