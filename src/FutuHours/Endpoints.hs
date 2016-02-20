{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

-- | API endpoints
module FutuHours.Endpoints (
    Context(..),
    addPlanmillApiKey,
    getProjects,
    getTimereports,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Lens               ((^.))
import Control.Monad.Http         (HttpT, evalHttpT)
import Control.Monad.Logger       (LoggingT, runStderrLoggingT)
import Control.Monad.Reader       (ReaderT (..))
import Control.Monad.Trans.Either (EitherT)
import Data.List                  (nub)
import Data.Pool                  (withResource)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Servant                    (ServantErr)

import Servant.Server (err403, err404)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

import FutuHours.Context
import FutuHours.Types

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
import qualified PlanMill.Operational           as PM (PlanMillT, runPlanMillT)
import qualified PlanMill.Test                  as PM (evalPlanMillIO)

-- | Add planmill api key.
addPlanmillApiKey :: MonadIO m => Context -> FUMUsername -> PlanmillApiKey -> m ()
addPlanmillApiKey Context { ctxPostgresPool = pool } (FUMUsername username) (PlanmillApiKey apikey) =
    liftIO $ withResource pool $ \conn -> do
        rows <- execute conn "INSERT INTO futuhours.apikeys (fum_username, planmill_apikey) VALUES (?, ?)" (username, apikey)
        print username
        print apikey
        print rows

getPlanmillApiKey :: MonadIO m => Context -> FUMUsername -> m (Maybe PlanmillApiKey)
getPlanmillApiKey  Context { ctxPostgresPool = pool } (FUMUsername username) =
    liftIO $ withResource pool $ \conn -> do
        rows <- query conn "SELECT planmill_apikey FROM futuhours.apikeys WHERE fum_username = ? LIMIT 1" (Only username)
        return $ case rows of
            (Only apikey : _) -> Just (PlanmillApiKey apikey)
            _                 -> Nothing

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

withPlanmillCfg :: (PM.Cfg -> IO a) -> Context -> FUMUsername -> EitherT ServantErr IO a
withPlanmillCfg action ctx username =do
    planMillId <- maybe (throwError err404) pure $ HM.lookup username (ctxPlanmillUserLookup ctx)
    apiKey     <- maybe (throwError err403) pure =<< getPlanmillApiKey' ctx username
    let cfg = (ctxPlanmillCfg ctx) { PM.cfgUserId = planMillId, PM.cfgApiKey = apiKey }
    liftIO $ action cfg

type Stack = ReaderT PM.Cfg :$ LoggingT :$ HttpT IO

runPlanmillT :: PM.Cfg -> PM.PlanMillT Stack a -> IO a
runPlanmillT cfg pm  = evalHttpT $ runStderrLoggingT $ flip runReaderT cfg $ PM.runPlanMillT pm

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
