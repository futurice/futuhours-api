{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans () where

import Prelude        ()
import Prelude.Compat

import Data.Proxy     (Proxy (..))
import Data.Text      (Text)
import PlanMill.Types (Identifier (..))
import Servant.Docs   (ToSample (..))

import qualified Data.Vector as V

import Data.Swagger

instance ToSample Text Text where
    toSample _ = Nothing

instance ToSample a b => ToSample (V.Vector a) (V.Vector b) where
    toSample _ = Just . V.fromList . map snd . toSamples $ (Proxy :: Proxy a)

instance ToSchema (Identifier a)

instance ToSample () () where
    toSample _ = Just ()
