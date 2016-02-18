{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module FutuHours (defaultMain) where

import Prelude        ()
import Prelude.Compat

import Data.Pool           (createPool)
import Network.Wai
import Servant
import Servant.Cache.Class (DynMapCache)
import Servant.Futurice
import System.IO           (hPutStrLn, stderr)

import qualified Database.PostgreSQL.Simple    as Postgres
import qualified Network.Wai.Handler.Warp      as Warp
import qualified PlanMill                      as PM (Cfg (..))
import qualified Servant.Cache.Internal.DynMap as DynMap

-- FutuHours modules
import FutuHours.API
import FutuHours.Config          (Config (..), getConfig)
import FutuHours.Endpoints
import FutuHours.PlanMillUserIds (planMillUserIds)

import Orphans ()

-- | API server
server :: Context -> Server FutuHoursAPI
server ctx = pure "Hello to futuhours api"
    :<|> addPlanmillApiKey ctx
    :<|> getTimereports ctx
    :<|> getProjects ctx

-- | Server with docs and cache and status
server' :: DynMapCache -> Context -> Server FutuHoursAPI'
server' cache ctx = futuriceApiServer cache futuhoursAPI futuhoursExtraDocs (server ctx)

-- | Wai application
app :: DynMapCache -> Context -> Application
app cache ctx = serve futuhoursAPI' (server' cache ctx)

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
    planmillUserLookup <- planMillUserIds pmCfg cfgFumToken cfgFumBaseurl cfgFumList
    let ctx = Context pmCfg postgresPool planmillUserLookup
    cache <- DynMap.newIO
    let app' = app cache ctx
    hPutStrLn stderr "Now I'll start the webservice"
    Warp.run cfgPort app'
