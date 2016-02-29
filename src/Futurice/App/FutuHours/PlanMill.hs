{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.FutuHours.PlanMill where

import Futurice.Prelude
import Prelude          ()

import Control.Monad              (void)
import Control.Monad.Http         (HttpT, evalHttpT)
import Control.Monad.Logger       (LoggingT, logDebug, logWarn,
                                   runStderrLoggingT)
import Control.Monad.Reader       (ReaderT (..))
import Control.Monad.Trans.Class  (lift)
import Data.Aeson.Compat          (FromJSON)
import Data.Binary.Tagged         (HasSemanticVersion, HasStructuralInfo,
                                   taggedDecodeOrFail, taggedEncode)
import Database.PostgreSQL.Simple (Connection, Only (..))

import Control.Monad.CryptoRandom.Extra (CRandT, GenError, HashDRBG,
                                         MonadInitHashDRBG (..),
                                         evalCRandTThrow)

import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple.Fxtra as Postgres

import qualified PlanMill               as PM (Cfg (..), PlanMill)
import qualified PlanMill.Eval          as PM (evalPlanMill)
import qualified PlanMill.Operational   as PM (GenPlanMillT, runGenPlanMillT)
import qualified PlanMill.Types.Request as PM (requestUrlParts)
import qualified PlanMill.Types.UrlPart as PM (fromUrlParts)

type Stack = ReaderT PM.Cfg :$ LoggingT :$ HttpT IO
type InnerStack = ReaderT PM.Cfg :$ CRandT HashDRBG GenError :$ Stack

class (Binary a, HasSemanticVersion a, HasStructuralInfo a, FromJSON a) => BinaryFromJSON a
instance (Binary a, HasSemanticVersion a, HasStructuralInfo a, FromJSON a) => BinaryFromJSON a

runPlanmillT :: forall a. PM.Cfg -> PM.GenPlanMillT BinaryFromJSON Stack a -> IO a
runPlanmillT cfg pm =
    evalHttpT $ runStderrLoggingT $ flip runReaderT cfg $ do
        g <- mkHashDRBG
        flip evalCRandTThrow g $ flip runReaderT cfg $ action
  where
    action :: InnerStack a
    action = PM.runGenPlanMillT evalPlanMill (lift . lift) pm

    evalPlanMill :: forall b. BinaryFromJSON b => PM.PlanMill b -> InnerStack b
    evalPlanMill = PM.evalPlanMill

runCachedPlanmillT :: forall a. Connection -> PM.Cfg -> PM.GenPlanMillT BinaryFromJSON Stack a -> IO a
runCachedPlanmillT conn cfg pm =
    evalHttpT $ runStderrLoggingT $ flip runReaderT cfg $ do
        g <- mkHashDRBG
        flip evalCRandTThrow g $ flip runReaderT cfg $ action
  where
    action :: InnerStack a
    action = PM.runGenPlanMillT evalPlanMill (lift . lift) pm

    evalPlanMill :: forall b. BinaryFromJSON b => PM.PlanMill b -> InnerStack b
    evalPlanMill req = do
        r <- liftIO $ Postgres.singleQuery conn "SELECT data FROM futuhours.cache WHERE path = ? and updated + interval '150 minutes' > current_timestamp;" (Only url)
        case r of
            Nothing -> evaledPlanMill
            Just (Only (Postgres.Binary bs)) -> case taggedDecodeOrFail bs of
                Right (bsLeft, _, x)
                    | BSL.null bsLeft -> pure x
                    | otherwise       -> do
                        $(logDebug) $ "Didn't consume all input from cache: " <> url
                        evaledPlanMill
                Left (_, _, err) -> do
                        $(logWarn) $ "Cannot decode cached value: " <> url <> " -- " <> T.pack err
                        evaledPlanMill
      where
        url :: Text
        url = T.pack $ PM.fromUrlParts (PM.requestUrlParts req)

        evaledPlanMill :: InnerStack b
        evaledPlanMill = do
            x <- PM.evalPlanMill req
            let bs = taggedEncode x
            liftIO $ Postgres.withTransaction conn $ do
                void $ Postgres.execute conn "DELETE FROM futuhours.cache WHERE path = ?" (Only url)
                void $ Postgres.execute conn "INSERT INTO futuhours.cache (path, data) VALUES (?, ?)" (url, Postgres.Binary bs)
            pure x
