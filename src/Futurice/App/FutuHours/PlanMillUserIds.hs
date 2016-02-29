{-# LANGUAGE OverloadedStrings #-}
-- | *TODO:* extract into common library
module Futurice.App.FutuHours.PlanMillUserIds (planMillUserIds) where

import Futurice.Prelude
import Prelude          ()

import Data.Maybe                  (mapMaybe)
import Database.PostgreSQL.Simple  (Connection)
import Network.HTTP.Client         (newManager)
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Text.Regex.Applicative.Text (anySym, match)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

import qualified Control.Monad.PlanMill   as PM
import qualified FUM
import qualified PlanMill                 as PM
import qualified PlanMill.EndPoints.Users as PM

import Futurice.App.FutuHours.PlanMill
import Futurice.App.FutuHours.Types

planMillUserIds
    :: PM.Cfg
    -> Connection
    -> FUM.AuthToken
    -> FUM.BaseUrl
    -> FUM.ListName
    -> IO PlanmillUserIdLookupTable
planMillUserIds cfg conn authToken baseUrl listName = do
    manager <- newManager tlsManagerSettings
    planmillUsers <- runCachedPlanmillT conn cfg True $ PM.planmillAction PM.users
    fumUsers <- FUM.fetchList manager authToken baseUrl listName
    return $ process planmillUsers fumUsers
  where
    process :: Vector PM.User -> Vector FUM.User -> PlanmillUserIdLookupTable
    process planmillUsers
        = HM.fromList
        . mapMaybe (process' planmillUsers)
        . filter fumUserPredicate
        . V.toList

    fumUserPredicate :: FUM.User -> Bool
    fumUserPredicate = (FUM.StatusActive ==) . view FUM.userStatus

    process' :: Vector PM.User -> FUM.User -> Maybe (FUMUsername, PM.UserId)
    process' planmillUsers fumUser = case V.find p planmillUsers of
        Nothing           -> Nothing
        Just planmillUser -> Just (FUMUsername (FUM._getUserName fumUserName), planmillUser ^. PM.identifier)
      where
        fumUserName = fumUser ^. FUM.userName
        fumUserNameStr = fumUserName ^. FUM.getUserName . from packed
        p planmillUser = case match ("https://login.futurice.com/openid/" *> many anySym) (PM.uUserName planmillUser) of
            -- TODO: here we have to use enumerations api later
            Just name  -> PM.uPassive planmillUser /= Just 1 && name == fumUserNameStr
            Nothing    -> False
