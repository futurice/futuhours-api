{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies, ScopedTypeVariables, RankNTypes #-}
-- | "Generics.SOP" derivation for record types (i.e. products).
module Futurice.Generics (
    -- * QuickCheck
    sopArbitrary,
    sopShrink,
    -- * Cassava
    sopToNamedRecord,
    sopHeaderOrder,
    -- * Aeson
    sopToJSON,
    -- * Swagger
    sopDeclareNamedSchema,
    ) where

import Futurice.Prelude hiding (Generic, from)
import Prelude          ()

import Control.Lens      (Lens', lens, (&))
import Data.ByteString   (ByteString)
import Data.Char         (toLower)
import Generics.SOP
import Generics.SOP.Lens

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Csv             as Csv
import qualified Data.Swagger         as Swagger
import qualified Data.Swagger.Declare as Swagger
import qualified Data.Vector          as V
import qualified Test.QuickCheck      as QC
import qualified GHC.Exts             as Exts

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

sopArbitrary
    :: (Generic a, All QC.Arbitrary xs, Code a ~ '[xs]) 
    => QC.Gen a
sopArbitrary = to . SOP . Z <$> sopArbitrary'

sopArbitrary' :: All QC.Arbitrary xs => QC.Gen (NP I xs)
sopArbitrary' = hsequence $ hcpure (Proxy :: Proxy QC.Arbitrary) QC.arbitrary

-- | TODO: not implemented
sopShrink
    :: (Generic a, All QC.Arbitrary xs, Code a ~ '[xs])
    => a -> [a]
sopShrink _ = []

-------------------------------------------------------------------------------
-- Cassava
-------------------------------------------------------------------------------

sopToNamedRecord
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, All Csv.ToField xs, Code a ~ '[xs])
    => a
    -> Csv.NamedRecord
sopToNamedRecord
    = Csv.namedRecord
    . sopToNamedRecord' fieldInfos 
    . (^. unsop . unSingletonS)
    . from
  where
    fieldInfos = datatypeInfo (Proxy :: Proxy a) ^.
        constructorInfo . unSingletonP . fieldInfo

sopToNamedRecord'
    :: All Csv.ToField xs => NP FieldInfo xs -> NP I xs
    -> [(ByteString, ByteString)]
sopToNamedRecord' fs' xs' = go fs' xs'
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs'

    go :: All Csv.ToField xs => NP FieldInfo xs -> NP I xs -> [(ByteString, ByteString)]
    go Nil Nil = []
    go (FieldInfo f :* fs) (I x :* xs) =
        Csv.namedField (fromString $ processFieldName prefix f) x : go fs xs
    go _ _ = error "sopToNamedRecord' go: impossible happened"

sopHeaderOrder
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, Code a ~ '[xs])
    => a  -- ^ Unused, only for compatibility with @cassava@'s 'headerOrder'
    -> Csv.Header
sopHeaderOrder _ = V.fromList (sopHeaderOrder' fieldInfos)
  where
    fieldInfos = datatypeInfo (Proxy :: Proxy a) ^.
        constructorInfo . unSingletonP . fieldInfo

sopHeaderOrder' :: SListI xs => NP FieldInfo xs -> [ByteString]
sopHeaderOrder' fs = hcollapse (hmap f fs)
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs

    f :: FieldInfo a -> K ByteString a
    f (FieldInfo n) = K . fromString . processFieldName prefix $ n

-------------------------------------------------------------------------------
-- Aeson
-------------------------------------------------------------------------------

sopToJSON
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, All Aeson.ToJSON xs, Code a ~ '[xs])
    => a
    -> Aeson.Value
sopToJSON
    = Aeson.object 
    . sopToJSON' fieldInfos
    . (^. unsop . unSingletonS)
    . from
  where
    fieldInfos = datatypeInfo (Proxy :: Proxy a) ^.
        constructorInfo . unSingletonP . fieldInfo

sopToJSON'
    :: All Aeson.ToJSON xs => NP FieldInfo xs -> NP I xs
    -> [Aeson.Pair]
sopToJSON' fs' xs' = go fs' xs'
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs'

    go :: All Aeson.ToJSON xs => NP FieldInfo xs -> NP I xs -> [Aeson.Pair]
    go Nil Nil = []
    go (FieldInfo f :* fs) (I x :* xs) =
        (fromString $ processFieldName prefix f) Aeson..= x : go fs xs
    go _ _ = error "sopToNamedRecord' go: impossible happened"

-------------------------------------------------------------------------------
-- swagger
-------------------------------------------------------------------------------

type SwaggerM = Swagger.Declare (Swagger.Definitions Swagger.Schema)

sopDeclareNamedSchema
    :: forall a xs proxy.
       (Generic a, HasDatatypeInfo a, All Swagger.ToSchema xs, Code a ~ '[xs])
    => proxy a
    -> SwaggerM Swagger.NamedSchema
sopDeclareNamedSchema _ =
    pure (Swagger.NamedSchema (Just $ name ^. packed) schema)
  where
    name = "MySchema" :: String
    schema = mempty
      & Swagger.type_ .~ Swagger.SwaggerObject
      & Swagger.properties .~ Exts.fromList (hcollapse (hcmap (Proxy :: Proxy Swagger.ToSchema) prop fieldInfos))
      & Swagger.required .~ hcollapse (hmap req fieldInfos)

    prefix :: String
    prefix = longestFieldInfoPrefix fieldInfos

    req :: forall y. FieldInfo y -> K Text y
    req (FieldInfo n) = K $ processFieldName prefix n ^. packed

    prop :: forall y. Swagger.ToSchema y => FieldInfo y -> K (Text, Swagger.Referenced Swagger.Schema) y
    prop (FieldInfo n) = K (n', s)
      where
        n' = processFieldName prefix n ^. packed
        s = Swagger.toSchemaRef (Proxy :: Proxy y)

    fieldInfos :: NP FieldInfo xs
    fieldInfos = datatypeInfo proxy ^. constructorInfo . unSingletonP . fieldInfo

    proxy :: Proxy a
    proxy = Proxy


-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

longestCommonPrefix :: Eq a => [a] -> [a] -> [a]
longestCommonPrefix [] _ = []
longestCommonPrefix _ [] = []
longestCommonPrefix (x:xs) (y:ys)
    | x == y    = x : longestCommonPrefix xs ys
    | otherwise = []

longestFieldInfoPrefix :: NP FieldInfo xs -> String
longestFieldInfoPrefix Nil = ""
longestFieldInfoPrefix (FieldInfo _ :* Nil) = ""
longestFieldInfoPrefix (FieldInfo a :* FieldInfo b :* Nil) =
    longestCommonPrefix a b
longestFieldInfoPrefix (FieldInfo a :* xs) =
    longestCommonPrefix a (longestFieldInfoPrefix xs)

lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs

processFieldName :: String -> String -> String
processFieldName pfx = lowerFirst . drop (length pfx)

-------------------------------------------------------------------------------
-- generics-sop-lens
-------------------------------------------------------------------------------

-- | TODO: move to generics-sop-lens
constructorInfo :: Lens' (DatatypeInfo xss) (NP ConstructorInfo xss)
constructorInfo = lens g s
  where
    g :: DatatypeInfo xss -> NP ConstructorInfo xss
    g (ADT _ _ cs)    = cs
    g (Newtype _ _ c) = c :* Nil

    s :: DatatypeInfo xss -> NP ConstructorInfo xss -> DatatypeInfo xss
    s (ADT m n _)     cs          = ADT m n cs
    s (Newtype m n _) (c :* Nil)  = Newtype m n c
    s _ _ = error "constructorInfo set: impossible happened"

fieldInfo :: Lens' (ConstructorInfo xs) (NP FieldInfo xs)
fieldInfo = lens g s
  where
    g :: ConstructorInfo xs -> NP FieldInfo xs
    g (Record _ fs) = fs
    g _             = error "fieldInfo get: only record supported"

    s :: ConstructorInfo xs -> NP FieldInfo xs -> ConstructorInfo xs
    s (Record n _) fs = Record n fs
    s _ _             = error "fieldInfo set: only record supported" 

--everyP :: (forall a. Lens' (f a) (g a)) -> Lens' (NP f xs) (NP g xs)
--everyP = undefined
