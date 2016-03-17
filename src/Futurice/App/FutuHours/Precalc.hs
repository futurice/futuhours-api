{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | Module for precalculated values.
module Futurice.App.FutuHours.Precalc where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM     (atomically, readTVar, writeTVar)
import Control.Monad.Trans.Class  (MonadTrans (..))
import Control.Monad.Trans.Either (EitherT)
import Data.Functor.Compose       (Compose (..))
import Generics.SOP
import Generics.SOP.Curry
import Servant                    (ServantErr)

import qualified Data.Dependent.Map as DMap

import Futurice.App.FutuHours.Context
import Futurice.App.FutuHours.Types

executeEndpoint
    :: Context
    -> EndpointTag a
    -> IO a  -- ^ action
    -> IO ()
executeEndpoint Context { ctxPrecalcEndpoints = m } e a = case DMap.lookup e m of
    Nothing   -> pure ()
    Just tvar -> do
        x <- a
        atomically $ writeTVar (getCompose tvar) (Just x)

lookupEndpoint :: Context -> EndpointTag a -> IO (Maybe a)
lookupEndpoint Context { ctxPrecalcEndpoints = m } e =
    case DMap.lookup e m of
        Nothing   -> pure Nothing
        Just tvar -> atomically $ readTVar $ getCompose tvar

data DefaultableEndpoint (xs :: [*]) (p :: *) (r :: *) = DefaultableEndpoint
    { defEndTag
        :: EndpointTag r
    , defEndDefaultParsedParam
        :: IO p
    , defEndDefaultParams
        :: NP I xs
    , defEndParseParams
        :: NP I xs -> EitherT ServantErr IO p
    , defEndAction
        :: Context -> p -> IO r
    }

servantEndpoint
    :: forall xs p r. (All (Generics.SOP.Compose Eq I) xs)
    => DefaultableEndpoint xs p r
    -> Context
    -> HFn xs (EitherT ServantErr IO r)
servantEndpoint de ctx = hCurry endpoint
  where
    endpoint :: NP I xs -> EitherT ServantErr IO r
    endpoint xs
        | xs == defEndDefaultParams de = lift $ do
            l <- lookupEndpoint ctx (defEndTag de)
            case l of
                -- Shouldn't happen
                Nothing -> do
                    p <- defEndDefaultParsedParam de
                    defEndAction de ctx p
                Just r  -> return r
        | otherwise = do
            p <- defEndParseParams de xs
            lift $ defEndAction de ctx p

cronEndpoint
    :: DefaultableEndpoint xs p r
    -> Context
    -> IO ()
cronEndpoint de ctx = executeEndpoint
    ctx
    (defEndTag de)
    (defEndDefaultParsedParam de >>= defEndAction de ctx)
