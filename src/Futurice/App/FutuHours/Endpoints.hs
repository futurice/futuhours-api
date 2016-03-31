{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators     #-}

-- | API endpoints
module Futurice.App.FutuHours.Endpoints (
    Ctx(..),
    addPlanmillApiKey,
    getProjects,
    getTimereports,
    getBalances,
    balanceReportEndpoint,
    -- * Reports
    getMissingHoursReport,
    getMissingHoursReportList,
    missingHoursListEndpoint,
    -- * Power
    getPowerUsers,
    powerUsersEndpoint,
    getPowerAbsences,
    powerAbsencesEndpoint,
    -- * Legacy endpoins
    getLegacyUsers,
    getLegacyHours,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Monad.Trans.Except       (ExceptT)
import Data.Aeson.Extra                 (M (..))
import Data.BinaryFromJSON              (BinaryFromJSON)
import Data.List                        (nub, sortBy)
import Data.Maybe                       (fromJust)
import Data.Monoid (Sum (..))
import Data.Ord                         (comparing)
import Data.Pool                        (withResource)
import Data.Time                        (UTCTime (..), addDays)
import Data.Time.Fxtra                  (beginningOfPrevMonth,
                                         getCurrentDayInFinland)
import Database.PostgreSQL.Simple.Fxtra (Only (..), execute, singleQuery)
import Generics.SOP                     (All, NP(..), I(..))
import Servant                          (ServantErr)

import Servant.Server (err400, err403, err404)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

import Futurice.App.FutuHours.Context
import Futurice.App.FutuHours.PlanMill
import Futurice.App.FutuHours.Reports.MissingHours
import Futurice.App.FutuHours.Types
import Futurice.App.FutuHours.Precalc

-- Planmill modules
import           Control.Monad.PlanMill (ForallSymbols, MonadPlanMill (..))
import qualified PlanMill               as PM
import qualified PlanMill.Test          as PM (evalPlanMillIO)

-- | Add planmill api key.
addPlanmillApiKey :: MonadIO m => Ctx -> FUMUsername -> PlanmillApiKey -> m ()
addPlanmillApiKey Ctx { ctxPostgresPool = pool } username apikey =
    liftIO $ withResource pool $ \conn -> do
        rows <- execute conn "INSERT INTO futuhours.apikeys (fum_username, planmill_apikey) VALUES (?, ?)" (username, apikey)
        print username
        print apikey
        print rows

getPlanmillApiKey :: MonadIO m => Ctx -> FUMUsername -> m (Maybe PlanmillApiKey)
getPlanmillApiKey  Ctx { ctxPostgresPool = pool } username =
    liftIO $ withResource pool $ \conn -> do
        fromOnly <$$> singleQuery conn "SELECT planmill_apikey FROM futuhours.apikeys WHERE fum_username = ? LIMIT 1" (Only username)

getPlanmillApiKey' :: (MonadIO m, Functor m) => Ctx -> FUMUsername -> m (Maybe PM.ApiKey)
getPlanmillApiKey' ctx username = (fmap . fmap) f (getPlanmillApiKey ctx username)
  where f (PlanmillApiKey apiKey) = PM.ApiKey (TE.encodeUtf8 apiKey)

getTimereports :: Ctx  -> FUMUsername -> ExceptT ServantErr IO (V.Vector Timereport)
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
getProjects :: MonadIO m => Ctx -> UserId -> m (V.Vector Project)
getProjects Ctx { ctxPlanmillCfg = cfg } (UserId uid) =
    liftIO $ nubVector . fmap pmToFh <$> PM.evalPlanMillIO cfg planmill
  where
    -- TODO: there is somewhere better one, I'm sure.
    nubVector :: Eq a => V.Vector a -> V.Vector a
    nubVector = V.fromList . nub . V.toList

    planmill :: PM.PlanMill PM.ReportableAssignments
    planmill = PM.reportableAssignments $ PM.Ident $ fromIntegral uid

    pmToFh :: PM.ReportableAssignment -> Project
    pmToFh PM.ReportableAssignment{..} = Project raProject raProjectName

getBalances :: Ctx -> ExceptT ServantErr IO BalanceReport
getBalances = servantEndpoint balanceReportEndpoint

balanceReportEndpoint
    :: DefaultableEndpoint '[] () BalanceReport
balanceReportEndpoint = DefaultableEndpoint
    { defEndTag = EBalanceReport
    , defEndDefaultParsedParam = pure ()
    , defEndDefaultParams = Nil
    , defEndParseParams = \Nil -> pure ()
    , defEndAction = balanceReport
    }
  where
    balanceReport :: Ctx -> () -> IO BalanceReport
    balanceReport ctx () = do
        interval <- getInterval
        executeCachedAdminPlanmill ctx p $
            BalanceReport . V.fromList <$>
                traverse (getBalance interval) (HM.toList (ctxPlanmillUserLookup ctx))
      where
        p = Proxy :: Proxy '[ PM.TimeBalance, PM.User, PM.Team, PM.Timereports, PM.UserCapacities, PM.Meta ]

        getInterval = do
            b <- getCurrentDayInFinland
            let a = beginningOfPrevMonth b
            return (fromJust $ PM.mkInterval a b)

        getBalance interval (fumId, pmId) = do
            mh <- missingHoursForUser interval pmId
            let mh' = getSum . foldMap Sum . getMap . missingHoursDays $ mh
            let name = missingHoursName mh
            PM.TimeBalance balanceMinutes <- planmillAction $ PM.userTimeBalance pmId
            pure $ Balance fumId name (round $ balanceMinutes / 60) (round mh')

getLegacyUsers
    :: (MonadIO m, MonadError ServantErr m)
    => Ctx -> Maybe Text -> m (Envelope User)
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
    -> Ctx -> Maybe Text -> m (Envelope Hour)
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
    => Ctx
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

missingHoursListEndpoint
    :: DefaultableEndpoint
        '[Maybe Day, Maybe Day, Maybe FUMUsernamesParam]
        (PM.Interval Day, [FUMUsername])
        [MissingHour]
missingHoursListEndpoint = DefaultableEndpoint
    { defEndTag = EMissingHoursList
    , defEndDefaultParsedParam = do
        b <- getCurrentDayInFinland
        let a = beginningOfPrevMonth b
        return (fromJust $ PM.mkInterval a b, [])
    , defEndDefaultParams = I Nothing :* I Nothing :* I Nothing :* Nil
    , defEndParseParams = \(I a :* I b :* I usernames :* Nil) -> do
        b' <- maybe getCurrentDayInFinland pure b
        let a' = fromMaybe (beginningOfPrevMonth b') a
        interval <- maybe (throwError err400) pure $ PM.mkInterval a' b'
        let usernames' = maybe [] getFUMUsernamesParam usernames
        pure (interval, usernames')
    , defEndAction = missingHoursList
    }
  where
    missingHoursList :: Ctx -> (PM.Interval Day, [FUMUsername]) -> IO [MissingHour]
    missingHoursList ctx (interval, usernames) = executeCachedAdminPlanmill ctx p $ do
        r <- missingHours (ctxPlanmillUserLookup ctx) interval usernames
        pure
            . concatMap (f . snd)
            . sortBy (comparing fst)
            . HM.toList
            . unMissingHoursReport
            $ r
      where
        p = Proxy :: Proxy
            '[PM.UserCapacities, PM.Timereports, PM.User, PM.Team, PM.Meta]
        f (MissingHours n t c ds) = uncurry (MissingHour n t c) <$> Map.toList (getMap ds)

getMissingHoursReportList
    :: Ctx
    -> Maybe Day -> Maybe Day -> Maybe FUMUsernamesParam
    -> ExceptT ServantErr IO [MissingHour]
getMissingHoursReportList = servantEndpoint missingHoursListEndpoint

-------------------------------------------------------------------------------
-- Power
-------------------------------------------------------------------------------

powerUsersEndpoint
    :: DefaultableEndpoint '[] () (Vector PowerUser)
powerUsersEndpoint = DefaultableEndpoint
    { defEndTag = EPowerUsers
    , defEndDefaultParsedParam = pure ()
    , defEndDefaultParams = Nil
    , defEndParseParams = \Nil -> pure ()
    , defEndAction = powerUsers
    }
  where
    powerUsers :: Ctx -> () -> IO (Vector PowerUser)
    powerUsers ctx () = executeCachedAdminPlanmill ctx p $ traverse powerUser pmUsers
      where
        p = Proxy :: Proxy '[PM.User, PM.Team, PM.Meta]
        pmUsers = V.fromList $ HM.toList $ ctxPlanmillUserLookup ctx

        powerUser
            :: ( Applicative n, PM.MonadPlanMill n
               , All (PM.MonadPlanMillC n) '[PM.User, PM.Team, PM.Meta]
               , ForallSymbols (PM.MonadPlanMillC n) PM.EnumDesc
               )
            => (FUMUsername, PM.UserId) -> n PowerUser
        powerUser (fumLogin, uid) = do
            u <- PM.planmillAction $ PM.user uid
            t <- traverse (PM.planmillAction . PM.team) (PM.uTeam u)
            a <- PM.enumerationValue (PM.uPassive u) "-"
            return $ PowerUser
                { powerUserUsername = fumLogin
                , powerUserFirst    = PM.uFirstName u
                , powerUserLast     = PM.uLastName u
                , powerUserTeam     = maybe "Unknown Team" PM.tName t
                , powerUserStart    = utctDay <$> PM.uHireDate u
                , powerUserActive   = a
                }

getPowerUsers :: Ctx -> ExceptT ServantErr IO (Vector PowerUser)
getPowerUsers = servantEndpoint powerUsersEndpoint

powerAbsencesEndpoint
    :: DefaultableEndpoint '[Maybe Day, Maybe Day] (PM.Interval Day) (Vector PowerAbsence)
powerAbsencesEndpoint = DefaultableEndpoint
    { defEndTag = EPowerAbsences
    , defEndDefaultParsedParam = do
        m <- getCurrentDayInFinland
        let a = beginningOfPrevMonth m
        let b = addDays 365 m
        return $ fromJust $ PM.mkInterval a b
    , defEndDefaultParams = I Nothing :* I Nothing :* Nil
    , defEndParseParams = \(I a :* I b :* Nil) -> do
        b' <- maybe (addDays 365 <$> getCurrentDayInFinland) pure b
        let a' = fromMaybe (addDays (-365) $ beginningOfPrevMonth b') a
        maybe (throwError err400) pure $ PM.mkInterval a' b'
    , defEndAction = powerAbsences
    }
  where
    powerAbsences
        :: Ctx -> PM.Interval Day -> IO (Vector PowerAbsence)
    powerAbsences ctx interval = executeCachedAdminPlanmill ctx p getPowerAbsences'
      where
        p = Proxy :: Proxy '[PM.Absences, PM.UserCapacities]

        getPowerAbsences'
            :: ( Applicative n, PM.MonadPlanMill n
               , PM.MonadPlanMillC n PM.Absences
               , PM.MonadPlanMillC n PM.UserCapacities
               )
            => n (Vector PowerAbsence)
        getPowerAbsences' = do
            absences <- PM.planmillAction (PM.absencesFromInterval (toResultInterval interval))
            traverse toPowerAbsence' absences

        toPowerAbsence'
            :: ( Applicative n, PM.MonadPlanMill n
               , PM.MonadPlanMillC n PM.UserCapacities
               )
            => PM.Absence -> n PowerAbsence
        toPowerAbsence' ab = do
            case PM.mkInterval (utctDay $ PM.absenceStart ab) (utctDay $ PM.absenceFinish ab) of
                Just interval' -> do
                    uc <- PM.planmillAction $ PM.userCapacity interval' (PM.absencePerson ab)
                    return $ toPowerAbsence ab uc
                Nothing ->
                    return $ toPowerAbsence ab mempty

        toPowerAbsence :: PM.Absence -> PM.UserCapacities -> PowerAbsence
        toPowerAbsence ab uc = PowerAbsence
            { powerAbsenceUsername     = reverseLookup (PM.absencePerson ab) (ctxPlanmillUserLookup ctx)
            , powerAbsenceStart        = utctDay $ PM.absenceStart ab
            , powerAbsenceEnd          = utctDay $ PM.absenceFinish ab
            , powerAbsencePlanmillId   = ab ^. PM.identifier
            , powerAbsenceCapacities   = M uc'
            , powerAbsenceBusinessDays = length uc'
            }
          where
            uc' = capacities uc

    capacities :: PM.UserCapacities -> Map Day Double
    capacities
        = Map.fromList
        . filter ((> 0) . snd)
        . map (\x -> (PM.userCapacityDate x, fromIntegral (PM.userCapacityAmount x) / 60.0))
        . toList

getPowerAbsences
    :: Ctx -> Maybe Day -> Maybe Day -> ExceptT ServantErr IO (Vector PowerAbsence)
getPowerAbsences = servantEndpoint powerAbsencesEndpoint

toResultInterval :: PM.Interval Day -> PM.ResultInterval
toResultInterval i = fromJust $ flip PM.elimInterval i $ \a b ->
    PM.mkResultInterval PM.IntervalStart (UTCTime a 0) (UTCTime b 0)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withPlanmillCfg :: (PM.Cfg -> IO a) -> Ctx -> FUMUsername -> ExceptT ServantErr IO a
withPlanmillCfg action ctx username =do
    planMillId <- maybe (throwError err404) pure $ HM.lookup username (ctxPlanmillUserLookup ctx)
    apiKey     <- maybe (throwError err403) pure =<< getPlanmillApiKey' ctx username
    let cfg = (ctxPlanmillCfg ctx) { PM.cfgUserId = planMillId, PM.cfgApiKey = apiKey }
    liftIO $ action cfg

withLegacyPlanmill
    :: forall m a as. (MonadIO m, MonadError ServantErr m, All BinaryFromJSON as)
    => Proxy as
    -> (forall n. (Applicative n, MonadPlanMill n, All (MonadPlanMillC n) as) => PM.UserId -> n a)
    -> Ctx
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
    => Ctx
    -> Proxy as
    -> (forall n. (Applicative n, MonadPlanMill n, All (MonadPlanMillC n) as) => n a)
    -> m a
executeUncachedAdminPlanmill ctx _ action =
    liftIO $ withResource (ctxPostgresPool ctx) $ \conn ->
        runCachedPlanmillT (ctxDevelopment ctx) conn (ctxPlanmillCfg ctx) False action

executeCachedAdminPlanmill
    :: forall m a as. (MonadIO m, All BinaryFromJSON as, ForallSymbols BinaryFromJSON PM.EnumDesc)
    => Ctx
    -> Proxy as
    -> (forall n. (Applicative n, MonadPlanMill n, All (MonadPlanMillC n) as, ForallSymbols (MonadPlanMillC n) PM.EnumDesc) => n a)
    -> m a
executeCachedAdminPlanmill ctx _ action =
    liftIO $ withResource (ctxPostgresPool ctx) $ \conn ->
        runCachedPlanmillT (ctxDevelopment ctx) conn (ctxPlanmillCfg ctx) True action
