{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.FutuHours.Orphans () where

import Prelude        ()
import Prelude.Compat

import Data.Proxy     (Proxy (..))
import Data.Text      (Text)
import PlanMill.Types (Identifier (..))
import Servant.Docs   (ToSample (..))

import Data.Aeson.Compat (Value(..))

import qualified Data.Vector as V

import Data.Swagger

instance ToSample Text Text where
    toSample _ = Nothing

instance ToSample a b => ToSample (V.Vector a) (V.Vector b) where
    toSample _ = Just . V.fromList . map snd . toSamples $ (Proxy :: Proxy a)

instance ToSchema (Identifier a)

instance ToSample () () where
    toSample _ = Just ()

instance ToSample Value Value where
    toSample _ = Nothing

instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "JSON Value") s
      where
        s = mempty
