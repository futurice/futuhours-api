{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | TODO: rename to MissingHours
module Futurice.App.FutuHours.Reports.MissingHours (
    missingHours,
    missingHoursForUser,
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.Extra                 (M (..))
import Data.Maybe                       (mapMaybe)
import Futurice.Constraint.ForallSymbol (ForallFSymbol)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified PlanMill            as PM

import Futurice.App.FutuHours.PlanMillCache
import Futurice.App.FutuHours.Types

missingHoursForUser
    :: ( PM.MonadPlanMill m, Applicative m
       , PM.MonadPlanMillC m PM.User
       , PM.MonadPlanMillC m PM.Team
       , PM.MonadPlanMillC m PM.UserCapacities
       , PM.MonadPlanMillC m PM.Meta
       , ForallFSymbol (PM.MonadPlanMillC m) PM.EnumDesc
       , MonadPlanMillCached m
       )
    => PM.Interval Day
    -> PM.UserId
    -> m MissingHours
missingHoursForUser interval uid = do
    u <- PM.planmillAction $ PM.user uid
    t <- traverse (PM.planmillAction . PM.team) (PM.uTeam u)
    c <- PM.enumerationValue (PM.uContractType u) "Unknown Contract"
    uc <- PM.planmillAction $ PM.userCapacity interval uid
    let uc' = capacities uc
    tr <- cachedTimereports interval uid
    let tr' = reportedDays tr
    return $ MissingHours
        { missingHoursName     = PM.uFirstName u <> " " <> PM.uLastName u
        , missingHoursTeam     = maybe "Unknown Team" PM.tName t
        , missingHoursContract = c
        , missingHoursDays     = M (Map.differenceWith minus uc' tr')
        }
  where
    -- For now show only days without any hour markings
    minus :: Double -> Double -> Maybe Double
    minus a b
        | b > 0      = Nothing
        | otherwise  = Just a

    capacities :: PM.UserCapacities -> Map Day Double
    capacities
        = Map.fromList
        . filter (isPositive . snd)
        . map (\x -> (PM.userCapacityDate x, fromIntegral (PM.userCapacityAmount x) / 60.0))
        . toList

    reportedDays :: PM.Timereports -> Map Day Double
    reportedDays
        = Map.fromList
        . map (\x -> (PM.trStart x, PM.trAmount x / 60.0))
        . toList

-- |
--
-- /TODO/
--
-- * Types
missingHours
    :: forall m f.
        ( Applicative m, PM.MonadPlanMill m, Foldable f
        , PM.MonadPlanMillC m PM.UserCapacities
        , PM.MonadPlanMillC m PM.User
        , PM.MonadPlanMillC m PM.Team
        , PM.MonadPlanMillC m PM.Meta
        , ForallFSymbol (PM.MonadPlanMillC m) PM.EnumDesc
       , MonadPlanMillCached m
        )
    => PlanmillUserLookupTable
    -> PM.Interval Day
    -> f FUMUsername
    -> m MissingHoursReport
missingHours pmUsers interval usernames
    = fmap (MissingHoursReport . HM.fromList)
    . (traverse . traverse) (missingHoursForUser interval)
    . mapMaybe g
    $ usernames'
  where
    usernames' :: [FUMUsername]
    usernames' = case toList usernames of
        [] -> HM.keys pmUsers
        us -> us

    g :: FUMUsername -> Maybe (FUMUsername, PM.UserId)
    g n = (,) n . (^.PM.identifier) <$> HM.lookup n pmUsers

isPositive :: (Num a, Ord a) => a -> Bool
isPositive = (>0)
