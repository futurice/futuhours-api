{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | Module for precalculated values.
module Futurice.App.FutuHours.Precalc where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.Async   (async, wait)
import Control.Concurrent.STM     (atomically, readTVar, writeTVar)
import Control.Monad.Logger       (logInfo, logWarn)
import Control.Monad.Trans.Class  (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor.Compose       (Compose (..))
import Generics.SOP
import Generics.SOP.Curry
import Servant                    (ServantErr, err500)

import qualified Data.Dependent.Map as DMap
import qualified Data.Text          as T

import Futurice.App.FutuHours.Context
import Futurice.App.FutuHours.Types

executeEndpoint
    :: Ctx
    -> EndpointTag a
    -> IO a  -- ^ action
    -> IO ()
executeEndpoint Ctx { ctxPrecalcEndpoints = m } e a = case DMap.lookup e m of
    Nothing   -> pure ()
    Just tvar -> do
        x <- a
        x' <- async (pure x)
        atomically $ writeTVar (getCompose tvar) x'

lookupEndpoint :: Ctx -> EndpointTag a -> IO (Maybe a)
lookupEndpoint Ctx { ctxPrecalcEndpoints = m } e =
    case DMap.lookup e m of
        Nothing   -> pure Nothing
        Just tvar -> do
            a <- atomically $ readTVar $ getCompose tvar
            Just <$> wait a

data DefaultableEndpoint (xs :: [*]) (p :: *) (r :: *) = DefaultableEndpoint
    { defEndTag                :: EndpointTag r
    , defEndDefaultParsedParam :: IO p
    , defEndDefaultParams      :: NP I xs
    , defEndParseParams        :: NP I xs -> ExceptT ServantErr IO p
    , defEndAction             :: Ctx -> p -> IO r
    }

data SomeDefaultableEndpoint where
    SDE :: DefaultableEndpoint xs p r -> SomeDefaultableEndpoint

servantEndpoint
    :: forall xs p r. (All (Generics.SOP.Compose Eq I) xs)
    => DefaultableEndpoint xs p r
    -> Ctx
    -> HFn xs (ExceptT ServantErr IO r)
servantEndpoint de ctx = hCurry endpoint
  where
    endpoint :: NP I xs -> ExceptT ServantErr IO r
    endpoint xs
        | xs == defEndDefaultParams de = runFutuhoursLoggingT ctx $ do
            l <- liftIO $ lookupEndpoint ctx (defEndTag de)
            case l of
                -- Shouldn't happen
                Nothing -> do
                    $(logWarn) $ "non-cached endpoint: " <> (T.pack $ show $ defEndTag de)
                    throwM $ err500
                Just r  -> do
                    $(logInfo) $ "cached endpoint: " <> (T.pack $ show $ defEndTag de)
                    return r
        | otherwise = do
            p <- defEndParseParams de xs
            lift $ defEndAction de ctx p

cronEndpoint
    :: DefaultableEndpoint xs p r
    -> Ctx
    -> IO ()
cronEndpoint de ctx = executeEndpoint
    ctx
    (defEndTag de)
    (defEndDefaultParsedParam de >>= defEndAction de ctx)
