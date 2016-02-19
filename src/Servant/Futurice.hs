{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- | Module with servant helpers.
module Servant.Futurice (
    FuturiceAPI,
    futuriceApiServer,
    SwaggerSchemaEndpoint,
    DocsAPI,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM   (atomically)
import Control.Lens
import Data.Swagger
import Futurice.Colour          (SColour)
import Servant
import Servant.Cache.Class      (DynMapCache)
import Servant.Docs             (DocIntro (..), ExtraInfo, HasDocs, docsWith,
                                 markdown)
import Servant.Futurice.Favicon (FutuFaviconAPI, serveFutuFavicon)
import Servant.Futurice.Status  hiding (info)
import Servant.Swagger
import Servant.Swagger.UI
import Text.Blaze.Html          (Html)

import qualified Data.Text                     as T
import qualified Servant.Cache.Internal.DynMap as DynMap
import qualified Servant.HTML.Blaze            as Blaze
import qualified Text.Markdown                 as Markdown

type DocsAPI =
         "docs.md"   :> Get '[PlainText] T.Text
    :<|> "docs.html" :> Get '[Blaze.HTML] Html

type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

type FuturiceAPI api colour = api
    :<|> DocsAPI
    :<|> SwaggerSchemaEndpoint
    :<|> SwaggerUI "ui" SwaggerSchemaEndpoint (SwaggerSchemaEndpoint :<|> api)
    :<|> FutuFaviconAPI colour
    :<|> StatusAPI

serveDocs :: HasDocs api => Proxy api -> ExtraInfo api -> Server DocsAPI
serveDocs api extra = pure docsMd :<|> pure docsHtml
  where
    docsHtml = Markdown.markdown Markdown.def (docsMd ^. lazy)
    docsMd = T.pack docs

    docs :: String
    docs = markdown $ docsWith [intro] extra api

    intro :: DocIntro
    intro = DocIntro "Welcome" ["This is FutuHours API.", "Enjoy!"]

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
    :: forall api colour. (HasDocs api, HasSwagger api, SColour colour)
    => DynMapCache
    -> Proxy api
    -> ExtraInfo api
    -> Server api
    -> Server (FuturiceAPI api colour)
futuriceApiServer cache papi extraInfo server = server
    :<|> serveDocs papi extraInfo
    :<|> return (swaggerDoc papi)
    :<|> swaggerUIServer
    :<|> serveFutuFavicon
    :<|> serveStatus (stats cache)
