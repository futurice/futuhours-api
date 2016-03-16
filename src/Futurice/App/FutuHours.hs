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

import Data.Pool                   (createPool, withResource)
import Network.Wai
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Servant.Cache.Class         (DynMapCache)
import Servant.Futurice
import System.IO                   (hPutStrLn, stderr)

import qualified Database.PostgreSQL.Simple    as Postgres
import qualified Network.Wai.Handler.Warp      as Warp
import qualified PlanMill                      as PM (Cfg (..))
import qualified Servant.Cache.Internal.DynMap as DynMap

-- FutuHours modules
import Futurice.App.FutuHours.API
import Futurice.App.FutuHours.Config          (Config (..), getConfig)
import Futurice.App.FutuHours.Endpoints
import Futurice.App.FutuHours.PlanMillUserIds (planMillUserIds)
import Futurice.App.FutuHours.Types

import Futurice.App.FutuHours.Orphans ()

-- | API server
server :: Context -> Server FutuHoursAPI
server ctx = pure "Hello to futuhours api"
    :<|> addPlanmillApiKey ctx
    :<|> getBalances ctx
    :<|> (getMissingHoursReport ctx
        :<|> getMissingHoursReportList ctx
        )
    :<|> (getPowerUsers ctx
        )
    :<|> getTimereports ctx
    :<|> getProjects ctx
    :<|> pure (Envelope empty)
    :<|> getLegacyUsers ctx
    :<|> (\un lte gte -> getLegacyHours lte gte ctx un)

-- | Server with docs and cache and status
server' :: DynMapCache -> Context -> Server FutuHoursAPI'
server' cache ctx = futuriceApiServer cache futuhoursAPI (server ctx)

-- | Wai application
app :: DynMapCache -> Context -> Application
app cache ctx = simpleCors $ serve futuhoursAPI' (server' cache ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, I'm futuhours-api server"
    Config{..} <- getConfig
    let pmCfg = PM.Cfg
            { PM.cfgUserId  = cfgPlanmillAdminUser
            , PM.cfgApiKey  = cfgPlanmillSignature
            , PM.cfgBaseUrl = cfgPlanmillUrl
            }
    postgresPool <- createPool (Postgres.connect cfgPostgresConnInfo) Postgres.close 1 10 5
    planmillUserLookup <- withResource postgresPool $ \conn ->
        planMillUserIds pmCfg conn cfgFumToken cfgFumBaseurl cfgFumList
    let ctx = Context pmCfg postgresPool planmillUserLookup
    cache <- DynMap.newIO
    let app' = app cache ctx
    hPutStrLn stderr "Now I'll start the webservice"
    Warp.run cfgPort app'
