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

import qualified Data.Vector                   as V
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Servant.Cache.Internal.DynMap as DynMap

-- FutuHours modules
import FutuHours.API
import FutuHours.Config (Config (..), getConfig)
--import FutuHours.Types

import Orphans ()

-- | /TODO:/ We probably will have some
type Context = ()

-- | API server
server :: Context -> Server FutuHoursAPI
server _ctx = pure "Hello to futuhours api"
    :<|> pure SymTag
    :<|> pure (V.singleton SymTag)
    :<|> newTimeReport
    :<|> modifyTimeReport
    :<|> pure (V.singleton SymTag)
    :<|> pure (V.singleton SymTag)
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
    Config{..} <- getConfig
    cache <- DynMap.newIO
    let app' = app cache ()
    Warp.run cfgPort app'
