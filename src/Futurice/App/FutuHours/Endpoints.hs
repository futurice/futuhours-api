{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

-- | API endpoints
module Futurice.App.FutuHours.Endpoints (
    Context(..),
    addPlanmillApiKey,
    getProjects,
    getTimereports,
    getBalances,
    -- * Reports
    getMissingHoursReport,
    getMissingHoursReportList,
    -- * Power
    getPowerUsers,
    getPowerAbsences,
    -- * Legacy endpoins
    getLegacyUsers,
    getLegacyHours,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Monad                    (join)
import Control.Monad.Trans.Either       (EitherT)
import Data.Aeson.Extra                 (M (..))
import Data.BinaryFromJSON              (BinaryFromJSON)
import Data.List                        (nub, sortBy)
import Data.Maybe                       (fromJust)
import Data.Ord                         (comparing)
import Data.Pool                        (withResource)
import Data.Time                        (UTCTime (..), addDays)
import Data.Time.Fxtra                  (beginningOfPrevMonth,
                                         getCurrentDayInFinland)
import Database.PostgreSQL.Simple.Fxtra (Only (..), execute, singleQuery)
import Generics.SOP                     (All)
import Servant                          (ServantErr)

import Servant.Server (err400, err403, err404)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

import Futurice.App.FutuHours.Context
import Futurice.App.FutuHours.PlanMill
import Futurice.App.FutuHours.Reports.MissingHours (missingHours)
import Futurice.App.FutuHours.Types

-- Planmill modules
import           Control.Monad.PlanMill (ForallSymbols, MonadPlanMill (..))
import qualified PlanMill               as PM
import qualified PlanMill.Test          as PM (evalPlanMillIO)

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

getLegacyHours
    :: (MonadIO m, MonadError ServantErr m)
    => Maybe Day  -- ^ Day GTE, @>=@
    -> Maybe Day  -- ^ Day TTE, @<=@
    -> Context -> Maybe Text -> m (Envelope Hour)
getLegacyHours gteDay lteDay =
    case join (PM.mkResultInterval PM.IntervalStart <$> lte <*> gte) of
        Nothing       -> \_ _ -> throwError err400
        Just interval -> withLegacyPlanmill p $ \uid -> do
            let ri = interval
            ts <- planmillAction $ PM.timereportsFromIntervalFor ri uid
            pure $ Envelope $ fmap f ts
  where
    lte, gte :: Maybe UTCTime
    lte = UTCTime <$> lteDay <*> pure 0
    gte = UTCTime <$> gteDay <*> pure 0

    p = Proxy :: Proxy '[PM.Timereports]

    f :: PM.Timereport -> Hour
    f t = Hour
        { hourAbsence         = False
        , hourBillable        = PM.trBillableStatus t == 1 -- TODO: use enumerations
        , hourDay             = PM.trStart t
        , hourDescription     = fromMaybe "-" $ PM.trComment t
        , hourEditable        = False -- TODO: use status?
        , hourHours           = PM.trAmount t / 60
        , hourId              = t ^. PM.identifier
        , hourProjectId       = fromMaybe (PM.Ident 0) $ PM.trProject t
        , hourProjectCategory = 0 -- TODO
        , hourProjectName     = "TODO: project" -- TODO
        , hourStatus          = 0 -- TODO
        , hourTaskId          = PM.trTask t
        , hourTaskName        = "TODO: task" -- TODO
        , hourUserId          = PM.trPerson t
        , hourUser            = "someuser" -- TODO
        }


-------------------------------------------------------------------------------
-- Reports
-------------------------------------------------------------------------------

getMissingHoursReport
    :: (MonadIO m, MonadError ServantErr m)
    => Context
    -> Day -> Day -> Maybe FUMUsernamesParam
    -> m MissingHoursReport
getMissingHoursReport ctx a b usernames =
    case PM.mkInterval a b of
        Nothing -> throwError err400
        Just interval -> executeCachedAdminPlanmill ctx p $
            missingHours (ctxPlanmillUserLookup ctx) interval usernames'
  where
    p = Proxy :: Proxy
        '[PM.UserCapacities, PM.Timereports, PM.User, PM.Team, PM.Meta]

    usernames' :: [FUMUsername]
    usernames' = maybe [] getFUMUsernamesParam usernames

getMissingHoursReportList
    :: (Applicative m, MonadIO m, MonadError ServantErr m)
    => Context
    -> Maybe Day -> Maybe Day -> Maybe FUMUsernamesParam
    -> m [MissingHour]
getMissingHoursReportList ctx a b usernames = do
    b' <- maybe getCurrentDayInFinland pure b
    let a' = fromMaybe (beginningOfPrevMonth b') a
    r <- getMissingHoursReport ctx a' b' usernames
    pure
        . concatMap (f . snd)
        . sortBy (comparing fst)
        . HM.toList
        . unMissingHoursReport
        $ r
  where
    f (MissingHours n t c ds) = uncurry (MissingHour n t c) <$> Map.toList (getMap ds)

-------------------------------------------------------------------------------
-- Power
-------------------------------------------------------------------------------

getPowerUsers :: MonadIO m => Context -> m (Vector PowerUser)
getPowerUsers ctx = executeCachedAdminPlanmill ctx p $ traverse powerUser pmUsers
  where
    p = Proxy :: Proxy '[PM.User, PM.Team]
    pmUsers = V.fromList $ HM.elems $ ctxPlanmillUserLookup ctx

    powerUser
        :: ( Applicative n, PM.MonadPlanMill n
           , PM.MonadPlanMillC n PM.User, PM.MonadPlanMillC n PM.Team
           )
        => PM.UserId -> n PowerUser
    powerUser uid = do
        u <- PM.planmillAction $ PM.user uid
        t <- traverse (PM.planmillAction . PM.team) (PM.uTeam u)
        return $ PowerUser
            { powerUserFirst = PM.uFirstName u
            , powerUserLast  = PM.uLastName u
            , powerUserTeam  = maybe "Unknown Team" PM.tName t
            , powerUserStart = utctDay <$> PM.uHireDate u
            }

getPowerAbsences
    :: (Applicative m, MonadIO m, MonadError ServantErr m)
    => Context -> Maybe Day -> Maybe Day -> m (Vector PowerAbsence)
getPowerAbsences ctx a b = do
    b' <- maybe (addDays 365 <$> getCurrentDayInFinland) pure b
    let a' = fromMaybe (addDays (-365) $ beginningOfPrevMonth b') a
    interval <- maybe (throwError err400) pure $ PM.mkInterval a' b'
    executeCachedAdminPlanmill ctx p $ getPowerAbsences' interval
 where
    p = Proxy :: Proxy '[PM.Absences]

    getPowerAbsences'
        :: (Applicative n, PM.MonadPlanMill n, PM.MonadPlanMillC n PM.Absences)
        => PM.Interval Day
        -> n (Vector PowerAbsence)
    getPowerAbsences' interval =
        toPowerAbsence <$$>
            PM.planmillAction (PM.absencesFromInterval (toResultInterval interval))

    toPowerAbsence :: PM.Absence -> PowerAbsence
    toPowerAbsence ab = PowerAbsence
        { powerAbsenceUsername   = reverseLookup (PM.absencePerson ab) (ctxPlanmillUserLookup ctx)
        , powerAbsenceStart      = utctDay $ PM.absenceStart ab
        , powerAbsenceEnd        = utctDay $ PM.absenceFinish ab
        , powerAbsencePlanmillId = ab ^. PM.identifier
        }

toResultInterval :: PM.Interval Day -> PM.ResultInterval
toResultInterval i = fromJust $ flip PM.elimInterval i $ \a b ->
    PM.mkResultInterval PM.IntervalStart (UTCTime a 0) (UTCTime b 0)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

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
            fumUserName <- FUMUsername <$> httpRemoteUser
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
    :: forall m a as. (MonadIO m, All BinaryFromJSON as, ForallSymbols BinaryFromJSON PM.EnumDesc)
    => Context
    -> Proxy as
    -> (forall n. (Applicative n, MonadPlanMill n, All (MonadPlanMillC n) as, ForallSymbols (MonadPlanMillC n) PM.EnumDesc) => n a)
    -> m a
executeCachedAdminPlanmill ctx _ action =
    liftIO $ withResource (ctxPostgresPool ctx) $ \conn ->
        runCachedPlanmillT conn (ctxPlanmillCfg ctx) True action
