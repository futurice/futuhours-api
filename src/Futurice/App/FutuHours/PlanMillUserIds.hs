{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
-- | *TODO:* extract into common library
module Futurice.App.FutuHours.PlanMillUserIds (planMillUserIds) where

import Futurice.Prelude
import Prelude          ()

import Control.Lens                (_1)
import Data.Maybe                  (mapMaybe)
import Database.PostgreSQL.Simple  (Connection)
import Network.HTTP.Client         (newManager)
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Text.Regex.Applicative.Text (anySym, match)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

import qualified FUM
import qualified PlanMill as PM

import Futurice.App.FutuHours.PlanMill
import Futurice.App.FutuHours.Types
import Futurice.App.FutuHours.Context

planMillUserIds
    :: (HasDevelopment env, HasPlanmillCfg env, HasLogLevel env)
    => env
    -> Connection
    -> FUM.AuthToken
    -> FUM.BaseUrl
    -> FUM.ListName
    -> IO PlanmillUserLookupTable
planMillUserIds env conn authToken baseUrl listName = do
    manager <- newManager tlsManagerSettings
    planmillUsers <- runHaxl env conn $ do
        us <- PM.planmillAction PM.users
        traverse (\u -> (,) <$> pure u <*> PM.enumerationValue (PM.uPassive u) "-") us
    fumUsers <- FUM.fetchList manager authToken baseUrl listName
    return $ process planmillUsers fumUsers
  where
    process :: Vector (PM.User, Text) -> Vector FUM.User -> PlanmillUserLookupTable
    process planmillUsers
        = HM.fromList
        . mapMaybe (process' planmillUsers)
        . filter fumUserPredicate
        . V.toList

    fumUserPredicate :: FUM.User -> Bool
    fumUserPredicate = (FUM.StatusActive ==) . view FUM.userStatus

    process' :: Vector (PM.User, Text) -> FUM.User -> Maybe (FUMUsername, PM.User)
    process' planmillUsers fumUser = case V.find p planmillUsers of
        Nothing    -> Nothing
        Just pair  -> Just (FUMUsername (FUM._getUserName fumUserName), pair ^. _1)
      where
        fumUserName = fumUser ^. FUM.userName
        fumUserNameStr = fumUserName ^. FUM.getUserName . from packed
        p (planmillUser, status) = case match ("https://login.futurice.com/openid/" *> many anySym) (PM.uUserName planmillUser) of
            Just name  -> status == "Active" && name == fumUserNameStr
            Nothing    -> False
