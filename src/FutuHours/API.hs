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

import Data.Text        (Text)
import Data.Vector      (Vector)
import Futurice.Colour
import Servant
import Servant.Docs     (ExtraInfo)
import Servant.Futurice

import FutuHours.Types

import Orphans ()

type LegacyFutuhoursAPI =
    "timereports" :> Capture "fum-id" FUMUsername :> Get '[JSON] (Vector Timereport)
    :<|> "projects" :> Capture "userid" UserId :> Get '[JSON] (Vector Project)

type FutuHoursAPI = Get '[PlainText] Text
    :<|> "add-planmill-token" :> Capture "fum-id" FUMUsername :> ReqBody '[JSON] PlanmillApiKey :> Put '[JSON] ()
    :<|> "legacy" :> LegacyFutuhoursAPI

{-
    :<|> "user"     :> Get '[JSON] User
    :<|> "hours"    :> Get '[JSON] (Vector TimeReport)
    :<|> "hours" :> "new"    :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport
    :<|> "hours" :> "modify" :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport
    :<|> "holidays" :> Get '[JSON] (Vector Holiday)
    :<|> "tasks"    :> Get '[JSON] (Vector Task)
-}
    -- TODO: absenses

futuhoursAPI :: Proxy FutuHoursAPI
futuhoursAPI = Proxy

futuhoursExtraDocs :: ExtraInfo FutuHoursAPI
futuhoursExtraDocs = hoursNewExtraDocs

hoursNewExtraDocs :: ExtraInfo FutuHoursAPI
hoursNewExtraDocs = mempty {- extraInfo
    (Proxy :: Proxy ("hours" :> "new" :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport)) $
    defAction & notes <>~ [ DocNote "Insert new hour marking" ["The id field will be ignored"] ]
    -}

type FutuHoursAPI' = FuturiceAPI FutuHoursAPI ('FutuAccent 'AF3 'AC3)

futuhoursAPI' :: Proxy FutuHoursAPI'
futuhoursAPI' = Proxy
