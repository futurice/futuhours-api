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

import Data.Aeson.Extra (SymTag (..))
import Data.Proxy       (Proxy (..))
import Data.Text        (Text)
import FutuHours.Types  (Project (..), UserId (..))
import PlanMill.Types   (Identifier (..))
import Servant          (Capture, FromText (..))
import Servant.Docs     (DocCapture (..), ToCapture (..), ToSample (..))

import qualified Data.Vector as V

instance ToSample Text Text where
    toSample _ = Nothing

instance ToSample a b => ToSample (V.Vector a) (V.Vector b) where
    toSample _ = Just . V.fromList . map snd . toSamples $ (Proxy :: Proxy a)

instance ToSample (SymTag tag) (SymTag tag) where
    toSample _ = Just SymTag

-------------------------------------------------------------------------------
-- TODO: Move to FutuHours.Types when stable
-------------------------------------------------------------------------------

instance ToSample Project Project where
    toSample _ = Just $ Project (Ident 42) "Projekti"

instance ToCapture (Capture "userid" UserId) where
    toCapture _ = DocCapture "userid" "PlanMill userid"

instance FromText UserId where
    fromText = fmap UserId . fromText
