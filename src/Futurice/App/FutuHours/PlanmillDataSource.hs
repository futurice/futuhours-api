{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | TODO: remove orphans
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.FutuHours.PlanmillDataSource
    ( PlanmillRequest
    , initDataSource
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.Async         (async, wait)
import Control.Monad.Catch              (handle)
import Control.Monad.CryptoRandom.Extra (CRandT, GenError, HashDRBG,
                                         evalCRandTThrow, mkHashDRBG)
import Control.Monad.Http               (HttpT, evalHttpT)
import Control.Monad.Logger             (LogLevel, LoggingT, MonadLogger,
                                         logDebug, logError, logWarn, logInfo)
import Control.Monad.Reader             (ReaderT, runReaderT)
import Data.Binary.Tagged               (taggedDecodeOrFail, taggedEncode)
import Data.BinaryFromJSON              (BinaryFromJSON)
import Futurice.Has
import Generics.SOP                     (I (..))
import Haxl.Core
import Haxl.Typed

import qualified Data.ByteString.Lazy             as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple.Fxtra as Postgres

import Futurice.App.FutuHours.Context

import qualified PlanMill             as PM
import qualified PlanMill.Operational as PM

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

newtype PlanmillRequest a = PMR (PM.PlanMillAction BinaryFromJSON a)

deriving instance Show (PlanmillRequest a)
deriving instance Typeable PlanmillRequest
deriving instance Eq (PlanmillRequest a)

instance Show1 PlanmillRequest where show1 = show

instance Hashable (PlanmillRequest a) where
    hashWithSalt salt (PMR r) = hashWithSalt salt r

instance StateKey PlanmillRequest where
    data State PlanmillRequest = PMRS Postgres.Connection PM.Cfg LogLevel

instance DataSourceName PlanmillRequest where
    dataSourceName _ = "PlanmillDataSource"

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

initDataSource
    :: forall env. (HasDevelopment env, HasPlanmillCfg env, HasLogLevel env)
    => env
    -> Postgres.Connection
    -> IO (State PlanmillRequest)
initDataSource e conn = pure $
    PMRS conn (e ^. planmillCfg) (e ^. logLevel)

instance In' PlanmillRequest r => PM.MonadPlanMill (GenTyHaxl r u) where
    type MonadPlanMillC (GenTyHaxl r u) = BinaryFromJSON
    planmillAction = GenTyHaxl . dataFetch . PMR . PM.PlanMillAction

-------------------------------------------------------------------------------
-- Fetching
-------------------------------------------------------------------------------

type M = CRandT HashDRBG GenError :$ ReaderT PM.Cfg :$ LoggingT :$ HttpT IO

-- | Postgres cache key, i.e. path
newtype Key = Key { getKey :: Text }
    deriving (Eq, Ord, Show)

instance Postgres.ToField Key where
    toField = Postgres.toField . getKey

instance Postgres.FromField Key where
    fromField f mbs = Key <$> Postgres.fromField f mbs

instance Hashable Key where
    hashWithSalt salt = hashWithSalt salt . getKey

-- | Blocked fetch together with it's key
data P = P
    { pKey  :: !Key
    , _pReq :: !(BlockedFetch PlanmillRequest)
    }

makeP :: BlockedFetch PlanmillRequest -> P
makeP bf@(BlockedFetch (PMR (PM.PlanMillAction req)) _) = P key bf
  where
    key :: Key
    key = Key $ url <> qs

    url :: Text
    url = T.pack $ PM.fromUrlParts (PM.requestUrlParts req)

    qs :: Text
    qs = T.pack . show $ case req of
        PM.PlanMillGet qs' _      -> qs'
        PM.PlanMillPagedGet qs' _ -> qs'
        PM.PlanMillPost _ _       -> []

-- | Results from DB
type CacheLookup = HashMap Key BSL.ByteString

makeCacheLookup :: [(Key, Postgres.Binary BSL.ByteString)] -> CacheLookup
makeCacheLookup = HM.fromList . (fmap . fmap) Postgres.fromBinary

instance DataSource u PlanmillRequest where
    fetch (PMRS conn cfg ll) _flags _userEnv blockedFetches =
        AsyncFetch $ \inner -> do
            a <- async action
            inner
            wait a
      where
        action = do
            -- Precalculate keys for each fetch
            let blockedFetches' = makeP <$> blockedFetches
            -- Ask all at once from DB
            cache <- makeCacheLookup <$> Postgres.query conn selectQuery (Postgres.Only . Postgres.In $ pKey <$> blockedFetches')
            -- Make rg
            g <- mkHashDRBG
            -- Go thru each request individual, using postgres results
            evalHttpT
                . runFutuhoursLoggingT (I ll)
                . flip runReaderT cfg
                . flip evalCRandTThrow g
                $ do $(logInfo) $ "Blocked fetches " <> T.pack (show $ HM.size cache) <> " / " <> T.pack (show $ length blockedFetches')
                     traverse_ (singleFetch cache) blockedFetches'

        singleFetch :: CacheLookup -> P -> M ()
        singleFetch cache (P key (BlockedFetch (PMR (PM.PlanMillAction req)) v)) = do
            -- Lookup in cache
            inCache <- join <$> extract key `traverse` HM.lookup key cache
            -- If not found perform fetch
            res <- maybe (singleFetch' key req) pure inCache
            -- Return
            liftIO $ putSuccess v res

        -- | Perform actual fetch and store in DB
        singleFetch' :: forall a. BinaryFromJSON a => Key -> PM.PlanMill a -> M a
        singleFetch' key req = do
            $(logDebug) $ "Requesting API: " <> getKey key
            x <- PM.evalPlanMill req
            -- Store in postgres
            let bs = taggedEncode x
            handle omitSqlError $ liftIO $ void' $ Postgres.query conn
                "SELECT futuhours.upsert_cache (?, ?)"
                (key, Postgres.Binary bs)
            pure x

        void' :: Functor m => m [Postgres.Only ()] -> m ()
        void' x = () <$ x

selectQuery :: Postgres.Query
selectQuery = "SELECT path, data FROM futuhours.cache WHERE path in ? and updated + interval '200 minutes' * (r + 1) > current_timestamp;"

-- | Extract cache value from single db query result
extract
    :: forall m a. (MonadLogger m, BinaryFromJSON a)
    => Key
    -> BSL.ByteString
    -> m (Maybe a)
extract key bs = case taggedDecodeOrFail bs of
    Right (bsLeft, _, x)
        | BSL.null bsLeft -> return $ Just x
        | otherwise       -> do
            $(logDebug) $ "Didn't consume all input from cache: " <> getKey key
            return Nothing
    Left (_, _, err) -> do
            $(logWarn) $ "Cannot decode cached value: " <> getKey key <> " -- " <> T.pack err
            return Nothing

omitSqlError :: MonadLogger m => Postgres.SqlError -> m ()
omitSqlError err = do
    $(logError) $ T.pack $ show err
    return ()
