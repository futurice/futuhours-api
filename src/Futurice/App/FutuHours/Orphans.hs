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

import Data.Aeson.Compat  (Value (..))
import Data.Swagger
import Data.Time          (Day)
import Data.Time.Parsers  (day)
import PlanMill.Types     (Identifier (..))
import Servant            (FromText (..))
import Text.Parsec        (parse)
import Text.Parsec.String ()

instance ToSchema (Identifier a)

instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "JSON Value") s
      where
        s = mempty

instance FromText Day where
    fromText t = either (const Nothing) return $
        parse day "FromText Day" t
