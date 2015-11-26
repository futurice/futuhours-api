{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module FutuHours.API where

import Prelude        ()
import Prelude.Compat

import Control.Concurrent.STM (atomically)
import Control.Lens
import Data.Semigroup         ((<>))
import Data.Vector            (Vector)
import Futurice.Colour
import Text.Blaze.Html        (Html)

import qualified Data.Text                     as T
import qualified Servant.Cache.Internal.DynMap as DynMap
import qualified Servant.HTML.Blaze            as Blaze
import qualified Text.Markdown                 as Markdown

import Servant
import Servant.Cache.Class      (DynMapCache)
import Servant.Docs             (DocIntro (..), DocNote (..), ExtraInfo,
                                 HasDocs, defAction, docsWith, extraInfo,
                                 markdown, notes)
import Servant.Futurice.Favicon
import Servant.Futurice.Status

import FutuHours.Types

import Orphans ()

type FutuHoursAPI =
    Get '[PlainText] T.Text
    :<|> "user"     :> Get '[JSON] User
    :<|> "hours"    :> Get '[JSON] (Vector TimeReport)
    :<|> "hours" :> "new"    :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport
    :<|> "hours" :> "modify" :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport
    :<|> "holidays" :> Get '[JSON] (Vector Holiday)
    :<|> "projects" :> Get '[JSON] (Vector Project)
    :<|> "tasks"    :> Get '[JSON] (Vector Task)
    -- TODO: absenses

futuhoursExtraDocs :: ExtraInfo FutuHoursAPI
futuhoursExtraDocs = hoursNewExtraDocs

hoursNewExtraDocs :: ExtraInfo FutuHoursAPI
hoursNewExtraDocs = extraInfo
    (Proxy :: Proxy ("hours" :> "new" :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport)) $
    defAction & notes <>~ [ DocNote "Insert new hour marking" ["The id field will be ignored"] ]

type DocsAPI =
         "docs.md"   :> Get '[PlainText] T.Text
    :<|> "docs.html" :> Get '[Blaze.HTML] Html

type FutuHoursAPI' = FutuHoursAPI
    :<|> DocsAPI
    :<|> FutuFaviconAPI ('FutuAccent 'AF3 'AC3)
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

serverWithDocs :: forall api c. (HasDocs api, SColour c)
               => DynMapCache
               -> Proxy api
               -> ExtraInfo api
               -> Server api
               -> Server (api :<|> DocsAPI :<|> FutuFaviconAPI c :<|> StatusAPI)
serverWithDocs cache p extra server = server
    :<|> serveDocs p extra
    :<|> serveFutuFavicon
    :<|> serveStatus (stats cache)

futuhoursAPI :: Proxy FutuHoursAPI
futuhoursAPI = Proxy

futuhoursAPI' :: Proxy FutuHoursAPI'
futuhoursAPI' = Proxy
