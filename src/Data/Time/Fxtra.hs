module Data.Time.Fxtra (
    getCurrentDayInFinland,
    tz,
    beginningOfPrevMonth,
    module Data.Time,
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.Time
import Data.Time.Zones (TZ, loadSystemTZ, timeZoneForUTCTime)
import System.IO.Unsafe (unsafePerformIO)

getCurrentDayInFinland :: MonadIO m => m Day
getCurrentDayInFinland = liftIO $ f <$> getCurrentTime
  where
    f :: UTCTime -> Day
    f u = localDay
        . zonedTimeToLocalTime
        . utcToZonedTime (timeZoneForUTCTime tz u) $ u

-- | TODO: use template haskell
tz :: TZ
tz = unsafePerformIO $ loadSystemTZ "Europe/Helsinki"
{-# NOINLINE tz #-}

beginningOfPrevMonth :: Day -> Day
beginningOfPrevMonth = fromGregorian' . f. toGregorian
  where
    f (y, 1, _) = (y - 1, 12, 1)
    f (y, m, _) = (y, m - 1, 1)

fromGregorian' :: (Integer, Int, Int) -> Day
fromGregorian' (y, m, d) = fromGregorian y m d

