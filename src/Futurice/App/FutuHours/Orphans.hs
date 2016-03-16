{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.FutuHours.Orphans () where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.Compat  (Value (..))
import Data.Aeson.Extra   (M (..), ToJSONKey (..))
import Data.Csv           (ToField (..))
import Data.Swagger
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

instance ToField Day where
    toField = fromString . show

instance ToJSONKey Day where
    toJSONKey a = show a ^. packed

instance ToSchema v => ToSchema (M (Map k v)) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String v))

instance ToField (Identifier a) where
    toField (Ident x) = toField x
