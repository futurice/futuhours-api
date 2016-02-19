{-# LANGUAGE OverloadedStrings #-}
-- | *TODO:* extract into common library
module FutuHours.PlanMillUserIds (planMillUserIds) where

import Futurice.Prelude
import Prelude          ()

import Control.Applicative         (many)
import Control.Lens                ((^.))
import Control.Monad.Http          (evalHttpT)
import Control.Monad.Logger        (runStderrLoggingT)
import Control.Monad.Reader        (runReaderT)
import Data.Maybe                  (mapMaybe)
import Network.HTTP.Client         (newManager)
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Text.Regex.Applicative.Text (anySym, match)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V

import qualified Control.Monad.PlanMill   as PM
import qualified FUM
import qualified PlanMill                 as PM
import qualified PlanMill.EndPoints.Users as PM

import FutuHours.Types

planMillUserIds
    :: PM.Cfg
    -> FUM.AuthToken
    -> FUM.BaseUrl
    -> FUM.ListName
    -> IO PlanmillUserIdLookupTable
planMillUserIds cfg authToken baseUrl listName = do
    manager <- newManager tlsManagerSettings
    planmillUsers <- evalHttpT $ runStderrLoggingT $ flip runReaderT cfg $ PM.planmillAction PM.users
    fumUsers <- FUM.fetchList manager authToken baseUrl listName
    return $ process planmillUsers fumUsers
  where
    process :: Vector PM.User -> Vector FUM.User -> PlanmillUserIdLookupTable
    process planmillUsers = HM.fromList . mapMaybe (process' planmillUsers) . V.toList

    process' :: Vector PM.User -> FUM.User -> Maybe (FUMUsername, PM.UserId)
    process' planmillUsers fumUser = case V.find p planmillUsers of
        Nothing           -> Nothing
        Just planmillUser -> Just (FUMUsername fumUserName, planmillUser ^. PM.identifier)
      where
        fumUserName = fumUser ^. FUM.userName
        fumUserNameStr = T.unpack fumUserName
        p planmillUser = case match ("https://login.futurice.com/openid/" *> many anySym) (PM.uUserName planmillUser) of
            Just name  -> name == fumUserNameStr
            Nothing    -> False
