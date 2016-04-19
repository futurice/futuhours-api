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
import Servant.HTML.Lucid
import Servant.CSV.Cassava
import Servant.Futurice

import Futurice.App.FutuHours.Orphans ()
import Futurice.App.FutuHours.Types

type LegacyFutuhoursAPI =
    --"timereports" :> Capture "fum-id" FUMUsername :> Get '[JSON] (Vector Timereport)
    -- :<|> 
    "projects" :> Capture "userid" UserId :> Get '[JSON] (Vector Project)
    :<|> "holidays" :> Get '[JSON] (Envelope ()) -- TODO
    :<|> "users" :> Header "Remote-User" Text :> Get '[JSON] (Envelope User)
    :<|> "hours" :> Header "Remote-User" Text :> QueryParam "day__lte" Day :> QueryParam "day__gte" Day :> Get '[JSON] (Envelope Hour)

type FutuHoursAPI = Get '[PlainText] Text
    :<|> "add-planmill-token" :> Capture "fum-id" FUMUsername :> ReqBody '[JSON] PlanmillApiKey :> Put '[JSON] ()
    :<|> "balances" :> Get '[HTML, JSON] BalanceReport
    :<|> "reports" :>
        ( "missinghours" :> Capture "from" Day :> Capture "to" Day :> QueryParam "users" FUMUsernamesParam :> Get '[JSON] MissingHoursReport
        :<|> "missinghours-list" :> QueryParam "from" Day :> QueryParam "to" Day :> QueryParam "users" FUMUsernamesParam :> Get '[(CSV', DefaultEncodeOpts), JSON] [MissingHour]
        )
    :<|> "power" :>
        ( "users" :> Get '[JSON] (Vector PowerUser)
        :<|> "absences" :> QueryParam "from" Day :> QueryParam "to" Day :> Get '[JSON] (Vector PowerAbsence)
        )
    :<|> "api" :> "v1" :> LegacyFutuhoursAPI

futuhoursAPI :: Proxy FutuHoursAPI
futuhoursAPI = Proxy

type FutuHoursAPI' = FuturiceAPI FutuHoursAPI ('FutuAccent 'AF3 'AC3)

futuhoursAPI' :: Proxy FutuHoursAPI'
futuhoursAPI' = Proxy
