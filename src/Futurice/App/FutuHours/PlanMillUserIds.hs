{-# LANGUAGE OverloadedStrings #-}
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

-- | TODO use MonadReader with Has* classes
planMillUserIds
    :: Bool
    -> PM.Cfg
    -> Connection
    -> FUM.AuthToken
    -> FUM.BaseUrl
    -> FUM.ListName
    -> IO PlanmillUserIdLookupTable
planMillUserIds development cfg conn authToken baseUrl listName = do
    manager <- newManager tlsManagerSettings
    planmillUsers <- runCachedPlanmillT development conn cfg True $ do
        us <- PM.planmillAction PM.users
        traverse (\u -> (,) <$> pure u <*> PM.enumerationValue (PM.uPassive u) "-") us
    fumUsers <- FUM.fetchList manager authToken baseUrl listName
    return $ process planmillUsers fumUsers
  where
    process :: Vector (PM.User, Text) -> Vector FUM.User -> PlanmillUserIdLookupTable
    process planmillUsers
        = HM.fromList
        . mapMaybe (process' planmillUsers)
        . filter fumUserPredicate
        . V.toList

    fumUserPredicate :: FUM.User -> Bool
    fumUserPredicate = (FUM.StatusActive ==) . view FUM.userStatus

    process' :: Vector (PM.User, Text) -> FUM.User -> Maybe (FUMUsername, PM.UserId)
    process' planmillUsers fumUser = case V.find p planmillUsers of
        Nothing    -> Nothing
        Just pair  -> Just (FUMUsername (FUM._getUserName fumUserName), pair ^. _1 . PM.identifier)
      where
        fumUserName = fumUser ^. FUM.userName
        fumUserNameStr = fumUserName ^. FUM.getUserName . from packed
        p (planmillUser, status) = case match ("https://login.futurice.com/openid/" *> many anySym) (PM.uUserName planmillUser) of
            Just name  -> status == "Active" && name == fumUserNameStr
            Nothing    -> False
