{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuHours.API where

import Futurice.Prelude
import Prelude          ()

import Futurice.Colour
import Servant
import Servant.Docs      (ExtraInfo)
import Servant.Futurice

import Futurice.App.FutuHours.Orphans ()
import Futurice.App.FutuHours.Types

type LegacyFutuhoursAPI =
    "timereports" :> Capture "fum-id" FUMUsername :> Get '[JSON] (Vector Timereport)
    :<|> "projects" :> Capture "userid" UserId :> Get '[JSON] (Vector Project)
    :<|> "holidays" :> Get '[JSON] (Envelope ()) -- TODO
    :<|> "users" :> Header "Http-Remote-User" Text :> Get '[JSON] (Envelope User) -- TODO
    :<|> "hours" :> Get '[JSON] (Envelope ()) -- TODO

type FutuHoursAPI = Get '[PlainText] Text
    :<|> "add-planmill-token" :> Capture "fum-id" FUMUsername :> ReqBody '[JSON] PlanmillApiKey :> Put '[JSON] ()
    :<|> "balances" :> Get '[JSON] (Vector Balance)
    :<|> "api" :> "v1" :> LegacyFutuhoursAPI

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
