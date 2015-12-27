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

import Data.Aeson.Extra       (SymTag (..))
import Network.Wai
import Servant
import Servant.Cache.Class    (DynMapCache)
import System.IO              (hPutStrLn, stderr)

import qualified Data.Vector                   as V
import qualified Network.Wai.Handler.Warp      as Warp
import qualified PlanMill                      as PM (Cfg (..))
import qualified Servant.Cache.Internal.DynMap as DynMap

-- FutuHours modules
import FutuHours.API
import FutuHours.Config    (Config (..), getConfig)
import FutuHours.Endpoints

import Orphans ()

-- | API server
server :: Context -> Server FutuHoursAPI
server ctx = pure "Hello to futuhours api"
    :<|> pure SymTag
    :<|> pure (V.singleton SymTag)
    :<|> newTimeReport
    :<|> modifyTimeReport
    :<|> pure (V.singleton SymTag)
    :<|> getProjects ctx
    :<|> pure (V.singleton SymTag)
  where
    newTimeReport _ = pure SymTag
    modifyTimeReport _ = pure SymTag

-- | Server with docs and cache and status
server' :: DynMapCache -> Context -> Server FutuHoursAPI'
server' cache ctx = serverWithDocs cache futuhoursAPI futuhoursExtraDocs (server ctx)

-- | Wai application
app :: DynMapCache -> Context -> Application
app cache ctx = serve futuhoursAPI' (server' cache ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, I'm futuhours-api server"
    Config{..} <- getConfig
    let ctx = PM.Cfg
            { PM.cfgUserId  = cfgPlanmillAdminUser
            , PM.cfgApiKey  = cfgPlanmillSignature
            , PM.cfgBaseUrl = cfgPlanmillUrl
            }
    cache <- DynMap.newIO
    let app' = app cache ctx
    hPutStrLn stderr "Now I'll start the webservice"
    Warp.run cfgPort app'
