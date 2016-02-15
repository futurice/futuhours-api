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

import Control.Lens
import Data.Vector      (Vector)
import FutuHours.Types
import Futurice.Colour
import Servant
import Servant.Docs     (DocNote (..), ExtraInfo, defAction, extraInfo, notes)
import Servant.Futurice

import qualified Data.Text as T

import Orphans ()

type FutuHoursAPI =
    Get '[PlainText] T.Text
    :<|> "user"     :> Get '[JSON] User
    :<|> "hours"    :> Get '[JSON] (Vector TimeReport)
    :<|> "hours" :> "new"    :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport
    :<|> "hours" :> "modify" :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport
    :<|> "holidays" :> Get '[JSON] (Vector Holiday)
    :<|> "projects" :> Capture "userid" UserId :> Get '[JSON] (Vector Project)
    :<|> "tasks"    :> Get '[JSON] (Vector Task)
    -- TODO: absenses

futuhoursAPI :: Proxy FutuHoursAPI
futuhoursAPI = Proxy

futuhoursExtraDocs :: ExtraInfo FutuHoursAPI
futuhoursExtraDocs = hoursNewExtraDocs

hoursNewExtraDocs :: ExtraInfo FutuHoursAPI
hoursNewExtraDocs = extraInfo
    (Proxy :: Proxy ("hours" :> "new" :> ReqBody '[JSON] TimeReport :> Post '[JSON] TimeReport)) $
    defAction & notes <>~ [ DocNote "Insert new hour marking" ["The id field will be ignored"] ]

type FutuHoursAPI' = FuturiceAPI FutuHoursAPI ('FutuAccent 'AF3 'AC3)

futuhoursAPI' :: Proxy FutuHoursAPI'
futuhoursAPI' = Proxy
