{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | TODO: rename to MissingHours
module Futurice.App.FutuHours.Reports.MissingHours (missingHours) where

import Futurice.Prelude
import Prelude          ()

import Data.Constraint.ForallSymbols (ForallSymbols)
import Data.Maybe                    (fromJust, mapMaybe)
import Data.Time                     (UTCTime (..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import qualified PlanMill            as PM

import Futurice.App.FutuHours.Types

-- |
--
-- /TODO/
--
-- * Types
missingHours
    :: forall m f.
        ( Applicative m, PM.MonadPlanMill m, Foldable f
        , PM.MonadPlanMillC m PM.UserCapacities
        , PM.MonadPlanMillC m PM.Timereports
        , PM.MonadPlanMillC m PM.User
        , PM.MonadPlanMillC m PM.Team
        , PM.MonadPlanMillC m PM.Meta
        , ForallSymbols (PM.MonadPlanMillC m) PM.EnumDesc
        )
    => PlanmillUserIdLookupTable
    -> PM.Interval Day
    -> f FUMUsername
    -> m MissingHoursReport
missingHours pmUsers interval usernames
    = fmap (MissingHoursReport . HM.fromList)
    . (traverse . traverse) f
    . mapMaybe g
    $ usernames'
  where
    usernames' :: [FUMUsername]
    usernames' = case toList usernames of
        [] -> HM.keys pmUsers
        us -> us

    f :: PM.UserId -> m MissingHours
    f uid = do
        u <- PM.planmillAction $ PM.user uid
        t <- traverse (PM.planmillAction . PM.team) (PM.uTeam u)
        c <- PM.enumerationValue (PM.uContractType u) "Unknown Contract"
        uc <- PM.planmillAction $ PM.userCapacity interval uid
        let uc' = capacities uc
        tr <- splitTimereportsFromIntervalFor interval uid
        let tr' = reportedDays tr
        return $ MissingHours
            { missingHoursName     = PM.uFirstName u <> " " <> PM.uLastName u
            , missingHoursTeam     = maybe "Unknown Team" PM.tName t
            , missingHoursContract = c
            , missingHoursDays     = uc' `Set.difference` tr'
            }

    capacities :: PM.UserCapacities -> Set Day
    capacities
        = Set.fromList
        . map PM.userCapacityDate
        . filter (isPositive . PM.userCapacityAmount)
        . toList

    reportedDays :: PM.Timereports -> Set Day
    reportedDays
        = Set.fromList
        . map PM.trStart
        . toList

    g :: FUMUsername -> Maybe (FUMUsername, PM.UserId)
    g n = (,) n <$> HM.lookup n pmUsers

isPositive :: (Num a, Ord a) => a -> Bool
isPositive = (>10)

splitTimereportsFromIntervalFor
    :: forall m. (PM.MonadPlanMill m, PM.MonadPlanMillC m PM.Timereports)
    => PM.Interval Day -> PM.UserId -> m PM.Timereports
splitTimereportsFromIntervalFor interval uid = single interval
  where
    single :: PM.Interval Day -> m PM.Timereports
    single i = PM.planmillAction $
        PM.timereportsFromIntervalFor (toResultInterval i) uid

    toResultInterval :: PM.Interval Day -> PM.ResultInterval
    toResultInterval i = fromJust $ flip PM.elimInterval i $ \a b ->
        PM.mkResultInterval PM.IntervalStart (UTCTime a 0) (UTCTime b 0)

{-
splitInterval
    :: (Enum a, Ord a, Show a, Typeable a)
    => PM.Interval a -> [PM.Interval a]
splitInterval = PM.elimInterval $ \a b ->
    (\x -> unsafeMkInterval x x) <$> [a..b]
  where
    unsafeMkInterval a b = fromJust $ PM.mkInterval a b
-}
