{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.FutuHours.DecayCache where

import Futurice.Prelude
import Prelude          ()

import Data.Binary.Tagged  (taggedDecodeOrFail, taggedEncode)
import Data.BinaryFromJSON (BinaryFromJSON)
import Numeric.Interval.NonEmpty ((...))

import qualified Data.Map                         as Map
import qualified Database.PostgreSQL.Simple.Fxtra as Postgres
import qualified PlanMill                         as PM

import Futurice.App.FutuHours.Orphans ()

type TableName = String

data DecayCacheable a = DecayCacheable
    { dcTableName  :: !TableName
    , dcExtractDay :: !(a -> Day)
    }

timereportsCacheable :: DecayCacheable PM.Timereport
timereportsCacheable = DecayCacheable "timereports" PM.trStart

populateData
    :: forall a f. (BinaryFromJSON a, Foldable f)
    => DecayCacheable a
    -> Postgres.Connection
    -> PM.UserId
    -> f a
    -> IO ()
populateData (DecayCacheable tableName extractDay) conn userId xs
    | null xs   = pure ()
    | otherwise = traverse_ f [a..b]
  where
    m :: Map Day [a]
    m = Map.fromListWith (++) $ map (\x ->(extractDay x, [x])) $ toList xs

    (a, _) = Map.findMin m
    (b, _) = Map.findMax m

    f :: Day -> IO ()
    f d = Postgres.withTransaction conn $ do
        void $ Postgres.execute conn deleteQuery (userId, d)
        void $ Postgres.execute conn insertQuery (userId, d, Postgres.Binary bs)
      where
        ys = fromMaybe [] $ Map.lookup d m
        bs  = taggedEncode ys

    insertQuery :: Postgres.Query
    insertQuery = fromString $ unwords
        [ "INSERT INTO futuhours." <> tableName
        , "(userid, day, data) VALUES (?, ?, ?);"
        ]

    deleteQuery :: Postgres.Query
    deleteQuery = fromString $ unwords
        [ "DELETE FROM futuhours." <> tableName
        , "WHERE userid = ? AND day = ?;"
        ]

selectData
    :: BinaryFromJSON a
    => DecayCacheable a
    -> Postgres.Connection
    -> PM.UserId
    -> PM.Interval Day
    -> IO [a]
selectData (DecayCacheable tableName _) conn userId interval = do
    let a = PM.elimInterval const interval
    let b = PM.elimInterval (\_ -> id) interval
    results <- Postgres.query conn selectQuery (userId, a, b)
    case traverse (taggedDecodeOrFail . Postgres.fromBinary . Postgres.fromOnly) results of
        Left _   -> pure [] 
        Right xs -> pure $ concatMap (\(_, _, x) -> x) xs
  where
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords
        [ "SELECT data FROM futuhours." <> tableName
        , "WHERE userid = ? AND day BETWEEN ? AND ?;"
        ]

updateData
    :: (BinaryFromJSON a, Foldable f)
    => DecayCacheable a 
    -> Postgres.Connection
    -> PM.UserId
    -> (PM.UserId -> PM.Interval Day -> IO (f a))
        -- ^ fetch function
    -> IO ()
updateData dc@(DecayCacheable tableName _) conn userId fetch = do
    results <- Postgres.query conn selectQuery (Postgres.Only userId) 
    case results of
        ((Just mi, Just ma):_) -> do
            let interval = mi ... ma
            xs <- fetch userId interval
            populateData dc conn  userId xs
        _ -> pure ()
  where
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords
        [ "SELECT MIN(day) as mi, MAX(day) as ma FROM futuhours." <> tableName
        , "WHERE futuhours.variance(futuhours." <> tableName <> "decay(age(day)), r) < current_timestamp - updated"
        , "AND userid = ?;"
        ]
