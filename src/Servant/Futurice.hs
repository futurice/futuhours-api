{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- | Module with servant helpers.
module Servant.Futurice (
    FuturiceAPI,
    futuriceApiServer,
    SwaggerSchemaEndpoint,
    swaggerDoc,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM   (atomically)
import Data.Swagger
import Futurice.Colour          (SColour)
import Servant
import Servant.Cache.Class      (DynMapCache)
import Servant.Futurice.Favicon (FutuFaviconAPI, serveFutuFavicon)
import Servant.Futurice.Status  hiding (info)
import Servant.Swagger
import Servant.Swagger.UI

import qualified Servant.Cache.Internal.DynMap as DynMap

type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

type FuturiceAPI api colour = api
    :<|> SwaggerSchemaEndpoint
    :<|> SwaggerUI "ui" SwaggerSchemaEndpoint (SwaggerSchemaEndpoint :<|> api)
    :<|> FutuFaviconAPI colour
    :<|> StatusAPI

stats :: DynMapCache -> StatusInfoIO
stats dmap = gcStatusInfo <> dynmapStats
  where
    dynmapStats :: StatusInfoIO
    dynmapStats = SIIO $ group "cache" . metric "size" <$> dynmapSize

    dynmapSize :: IO Int
    dynmapSize = atomically $ DynMap.size dmap

swaggerDoc :: HasSwagger api => Proxy api -> Swagger
swaggerDoc proxy = toSwagger proxy
    & info.title       .~ "FutuHours API"
    & info.version     .~ "2016.2.6"
    & info.description ?~ "This is an API that tests servant-swagger support "

futuriceApiServer
    :: forall api colour. (HasSwagger api, SColour colour)
    => DynMapCache
    -> Proxy api
    -> Server api
    -> Server (FuturiceAPI api colour)
futuriceApiServer cache papi server = server
    :<|> return (swaggerDoc papi)
    :<|> swaggerUIServer
    :<|> serveFutuFavicon
    :<|> serveStatus (stats cache)
