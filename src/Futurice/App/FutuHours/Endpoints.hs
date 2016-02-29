{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards , FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

-- | API endpoints
module Futurice.App.FutuHours.Endpoints (
    Context(..),
    addPlanmillApiKey,
    getProjects,
    getTimereports,
    getBalances,
    -- * Legacy endpoins
    getLegacyUsers,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Monad.Trans.Either       (EitherT)
import Data.List                        (nub)
import Data.Pool                        (withResource)
import Database.PostgreSQL.Simple.Fxtra (Only (..), execute, singleQuery)
import Generics.SOP                     (All)
import Servant                          (ServantErr)

import Servant.Server (err403, err404)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

import Futurice.App.FutuHours.Context
import Futurice.App.FutuHours.PlanMill
import Futurice.App.FutuHours.Types

-- Planmill modules
import           Control.Monad.PlanMill         (MonadPlanMill (..))
import qualified PlanMill                       as PM (ApiKey (..), Cfg (..),
                                                       Identifier (..),
                                                       PlanMill, identifier)
import qualified PlanMill.EndPoints.Assignments as PM (ReportableAssignment (..),
                                                       ReportableAssignments,
                                                       reportableAssignments)
import qualified PlanMill.EndPoints.Timereports as PM (Timereport (..),
                                                       Timereports, timereports)
import qualified PlanMill.EndPoints.Users       as PM (UserId, User(..), user, userTimeBalance)
import qualified PlanMill.Types.TimeBalance     as PM (TimeBalance (..))

import qualified PlanMill.Test as PM (evalPlanMillIO)

-- | Add planmill api key.
addPlanmillApiKey :: MonadIO m => Context -> FUMUsername -> PlanmillApiKey -> m ()
addPlanmillApiKey Context { ctxPostgresPool = pool } username apikey =
    liftIO $ withResource pool $ \conn -> do
        rows <- execute conn "INSERT INTO futuhours.apikeys (fum_username, planmill_apikey) VALUES (?, ?)" (username, apikey)
        print username
        print apikey
        print rows

getPlanmillApiKey :: MonadIO m => Context -> FUMUsername -> m (Maybe PlanmillApiKey)
getPlanmillApiKey  Context { ctxPostgresPool = pool } username =
    liftIO $ withResource pool $ \conn -> do
        fromOnly <$$> singleQuery conn "SELECT planmill_apikey FROM futuhours.apikeys WHERE fum_username = ? LIMIT 1" (Only username)

getPlanmillApiKey' :: (MonadIO m, Functor m) => Context -> FUMUsername -> m (Maybe PM.ApiKey)
getPlanmillApiKey' ctx username = (fmap . fmap) f (getPlanmillApiKey ctx username)
  where f (PlanmillApiKey apiKey) = PM.ApiKey (TE.encodeUtf8 apiKey)

getTimereports :: Context  -> FUMUsername -> EitherT ServantErr IO (V.Vector Timereport)
getTimereports = withPlanmillCfg $ \cfg -> liftIO $ runPlanmillT cfg getTimereports'
  where
    getTimereports' :: (MonadPlanMill m, MonadPlanMillC m PM.Timereports) => m (V.Vector Timereport)
    getTimereports' = do
        timereports <- planmillAction PM.timereports
        return $ fmap convert timereports
    convert :: PM.Timereport -> Timereport
    convert tr = Timereport (tr ^. PM.identifier) (PM.trComment tr)

-- | Return projects for user
--
-- TODO: Add short living cache (15min?)
-- TODO: see <https://github.com/futurice/futuhours-api/issues/1>
getProjects :: MonadIO m => Context -> UserId -> m (V.Vector Project)
getProjects Context { ctxPlanmillCfg = cfg } (UserId uid) =
    liftIO $ nubVector . fmap pmToFh <$> PM.evalPlanMillIO cfg planmill
  where
    -- TODO: there is somewhere better one, I'm sure.
    nubVector :: Eq a => V.Vector a -> V.Vector a
    nubVector = V.fromList . nub . V.toList

    planmill :: PM.PlanMill PM.ReportableAssignments
    planmill = PM.reportableAssignments $ PM.Ident $ fromIntegral uid

    pmToFh :: PM.ReportableAssignment -> Project
    pmToFh PM.ReportableAssignment{..} = Project raProject raProjectName

getBalances :: MonadIO m => Context -> m (V.Vector Balance)
getBalances ctx = executeCachedAdminPlanmill ctx p $
    V.fromList <$> traverse getBalance (HM.toList (ctxPlanmillUserLookup ctx))
  where
    p = Proxy :: Proxy '[PM.TimeBalance]
    getBalance (fumId, pmId) = do
        PM.TimeBalance balanceMinutes <- planmillAction $ PM.userTimeBalance pmId
        pure $ Balance fumId (round $ balanceMinutes / 60)

getLegacyUsers
    :: (MonadIO m, MonadError ServantErr m)
    => Context -> Maybe Text -> m (Envelope User)
getLegacyUsers = withLegacyPlanmill p $ \uid -> do
    u <- planmillAction $ PM.user uid
    PM.TimeBalance balance <- planmillAction $ PM.userTimeBalance uid
    let user = User
            { userFirstName        = PM.uFirstName u
            , userDefaultWorkHours = 7.5      -- TODO
            , userHolidaysDaysLeft = 999      -- TODO
            , userBalance          = round (balance / 60)
            , userEmployeeType     = "foo"    -- TODO
            }
    pure $ Envelope $ V.singleton user
  where
    p = Proxy :: Proxy '[PM.User, PM.TimeBalance]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

withPlanmillCfg :: (PM.Cfg -> IO a) -> Context -> FUMUsername -> EitherT ServantErr IO a
withPlanmillCfg action ctx username =do
    planMillId <- maybe (throwError err404) pure $ HM.lookup username (ctxPlanmillUserLookup ctx)
    apiKey     <- maybe (throwError err403) pure =<< getPlanmillApiKey' ctx username
    let cfg = (ctxPlanmillCfg ctx) { PM.cfgUserId = planMillId, PM.cfgApiKey = apiKey }
    liftIO $ action cfg

withLegacyPlanmill
    :: forall m a as. (MonadIO m, MonadError ServantErr m, All BinaryFromJSON as)
    => Proxy as
    -> (forall n. (Applicative n, MonadPlanMill n, All (MonadPlanMillC n) as) => PM.UserId -> n a)
    -> Context
    -> Maybe Text
    -> m a
withLegacyPlanmill p action ctx httpRemoteUser = do
    let mPlanMillId = do
            fumUserName <- FUMUsername <$> (httpRemoteUser <|> pure "ogre") -- TODO: remove default case
            HM.lookup fumUserName (ctxPlanmillUserLookup ctx)
    planMillId <- maybe (throwError err404) return mPlanMillId
    executeUncachedAdminPlanmill ctx p (action planMillId)

executeUncachedAdminPlanmill
    :: forall m a as. (MonadIO m, All BinaryFromJSON as)
    => Context
    -> Proxy as
    -> (forall n. (Applicative n, MonadPlanMill n, All (MonadPlanMillC n) as) => n a)
    -> m a
executeUncachedAdminPlanmill ctx _ action =
    liftIO $ withResource (ctxPostgresPool ctx) $ \conn ->
        runCachedPlanmillT conn (ctxPlanmillCfg ctx) False action

executeCachedAdminPlanmill
    :: forall m a as. (MonadIO m, All BinaryFromJSON as)
    => Context
    -> Proxy as
    -> (forall n. (Applicative n, MonadPlanMill n, All (MonadPlanMillC n) as) => n a)
    -> m a
executeCachedAdminPlanmill ctx _ action =
    liftIO $ withResource (ctxPostgresPool ctx) $ \conn ->
        runCachedPlanmillT conn (ctxPlanmillCfg ctx) True action
