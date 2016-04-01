{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.FutuHours.PlanMill where

import Futurice.Prelude
import Prelude          ()

import Control.Monad.Http         (HttpT, evalHttpT)
import Control.Monad.Logger       (LogLevel (..), LoggingT, MonadLogger,
                                   filterLogger, logDebug, logWarn,
                                   runStderrLoggingT)
import Control.Monad.Reader       (ReaderT (..))
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, gets,
                                   modify')
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Maybe  (MaybeT (..))
import Data.Binary.Tagged         (taggedDecodeOrFail, taggedEncode)
import Data.BinaryFromJSON        (BinaryFromJSON, PlanMillDynMap,
                                   PlanMillTypeable)
import Database.PostgreSQL.Simple (Connection, Only (..))

import Control.Monad.CryptoRandom.Extra (CRandT, GenError, HashDRBG,
                                         MonadInitHashDRBG (..),
                                         evalCRandTThrow)

import qualified Data.BinaryFromJSON              as PlanMillDynMap (insert,
                                                                     lookup)
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple.Fxtra as Postgres

import qualified PlanMill               as PM (Cfg (..), PlanMill (..))
import qualified PlanMill.Eval          as PM (evalPlanMill)
import qualified PlanMill.Operational   as PM (GenPlanMillT, runGenPlanMillT)
import qualified PlanMill.Types.Request as PM (requestUrlParts)
import qualified PlanMill.Types.UrlPart as PM (fromUrlParts)

type Stack = ReaderT PM.Cfg :$ LoggingT :$ HttpT IO
type InnerStack =
    ReaderT PM.Cfg :$ CRandT HashDRBG GenError :$ StateT (PlanMillDynMap Text) :$ Stack

runPlanmillT :: forall a. PM.Cfg -> PM.GenPlanMillT BinaryFromJSON Stack a -> IO a
runPlanmillT cfg pm =
    evalHttpT $ runStderrLoggingT $ flip runReaderT cfg $ do
        g <- mkHashDRBG
        flip evalStateT mempty $ flip evalCRandTThrow g $ flip runReaderT cfg $ action
  where
    action :: InnerStack a
    action = PM.runGenPlanMillT evalPlanMill (lift . lift . lift) pm

    evalPlanMill :: forall b. BinaryFromJSON b => PM.PlanMill b -> InnerStack b
    evalPlanMill = PM.evalPlanMill

runCachedPlanmillT
    :: forall a.
       Bool                                    -- ^ Development
    -> Connection                              -- ^ Postgres connection
    -> PM.Cfg                                  -- ^ Planmill config
    -> Bool                                    -- ^ whether to ask cache
    -> PM.GenPlanMillT BinaryFromJSON Stack a  -- ^ Action
    -> IO a
runCachedPlanmillT development conn cfg askCache pm =
    evalHttpT $ runStderrLoggingT $ filterLogger logPredicate $ flip runReaderT cfg $ do
        g <- mkHashDRBG
        flip evalStateT mempty $ flip evalCRandTThrow g $ flip runReaderT cfg $ action
  where
    logPredicate _ l = l >= LevelInfo

    action :: InnerStack a
    action = PM.runGenPlanMillT evalPlanMill (lift . lift . lift) pm

    evalPlanMill :: forall b. BinaryFromJSON b => PM.PlanMill b -> InnerStack b
    evalPlanMill req = if not askCache
        then evaledPlanMill
        else do
            r <- runMaybeT $
                let fromLocal = MaybeT $ lookupS url
                    fromDb = do
                        $(logDebug) $ "Looking in Postgres cache: " <> url
                        r' <- MaybeT $ liftIO $ Postgres.singleQuery conn selectQuery (Only url)
                        x <- extract url r'
                        -- Store in current execution cache
                        insertS url x
                        pure x
                in fromLocal <|> fromDb
            maybe evaledPlanMill return r
      where
        selectQuery :: Postgres.Query
        selectQuery
            | development = "SELECT data FROM futuhours.cache WHERE path = ? and updated + interval '300 minutes' > current_timestamp;"
            | otherwise   = "SELECT data FROM futuhours.cache WHERE path = ? and updated + interval '8 minutes' * (r + 1) > current_timestamp;"


        url' :: Text
        url' = T.pack $ PM.fromUrlParts (PM.requestUrlParts req)

        qs' :: Text
        qs' = T.pack . show $ case req of
            PM.PlanMillGet qs _ -> qs
            PM.PlanMillPagedGet qs _ -> qs
            PM.PlanMillPost _ _ -> []

        url :: Text
        url = url' <> qs'

        evaledPlanMill :: InnerStack b
        evaledPlanMill = do
            $(logDebug) $ "Requesting API: " <> url
            x <- PM.evalPlanMill req
            -- Store in postgres
            let bs = taggedEncode x
            liftIO $ Postgres.withTransaction conn $ do
                void $ Postgres.execute conn "DELETE FROM futuhours.cache WHERE path = ?" (Only url)
                void $ Postgres.execute conn "INSERT INTO futuhours.cache (path, data) VALUES (?, ?)" (url, Postgres.Binary bs)
            pure x

-- | Extract cache value from single db query result
extract
    :: (Alternative m, MonadLogger m, BinaryFromJSON a)
    => Text
    -> Only (Postgres.Binary BSL.ByteString)
    -> m a
extract url (Only (Postgres.Binary bs)) = case taggedDecodeOrFail bs of
    Right (bsLeft, _, x)
        | BSL.null bsLeft -> return x
        | otherwise       -> do
            $(logDebug) $ "Didn't consume all input from cache: " <> url
            empty
    Left (_, _, err) -> do
            $(logWarn) $ "Cannot decode cached value: " <> url <> " -- " <> T.pack err
            empty

lookupS
    :: (Ord k, PlanMillTypeable v, MonadState (PlanMillDynMap k) m)
    => k -> m (Maybe v)
lookupS k = gets (PlanMillDynMap.lookup k)

insertS
    :: (Ord k, PlanMillTypeable v, MonadState (PlanMillDynMap k) m)
    => k -> v -> m ()
insertS k v = modify' (PlanMillDynMap.insert k v)
