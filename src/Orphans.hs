{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans () where

import Data.Aeson.Extra (SymTag (..))
import Data.Proxy       (Proxy (..))
import Data.Text        (Text)
import FutuHours.Types  (Project (..))
import PlanMill.Types   (Identifier (..))
import Servant.Docs     (ToSample (..))

import qualified Data.Vector as V

instance ToSample Text Text where
    toSample _ = Nothing

instance ToSample a b => ToSample (V.Vector a) (V.Vector b) where
    toSample _ = Just . V.fromList . map snd . toSamples $ (Proxy :: Proxy a)

instance ToSample (SymTag tag) (SymTag tag) where
    toSample _ = Just SymTag

instance ToSample Project Project where
    toSample _ = Just $ Project (Ident 42) "Projekti"
