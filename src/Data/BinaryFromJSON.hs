{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.BinaryFromJSON (
    BinaryFromJSON,
    -- * Own version of typeable.
    --
    -- | This can be removed when we drop support for 7.8, in favour of 'Typeable'.
    PlanMillTypeable,
    PlanMillDynamic,
    toPMDyn,
    fromPMDyn,
    -- * Planmill dynamic map
    PlanMillDynMap,
    insert,
    lookup,
    size,
    ) where

import Futurice.Prelude hiding (lookup)
import Prelude          ()

import Data.Aeson                    (FromJSON)
import Data.Binary.Tagged            (HasSemanticVersion, HasStructuralInfo)
import Data.Constraint.ForallSymbols (Dict (..), ForallSymbols (..))
import GHC.TypeLits                  (KnownSymbol, symbolVal)
import Unsafe.Coerce                 (unsafeCoerce)

import qualified Data.Map as Map
import qualified PlanMill as PM

data PlanMillTypeRep
    = PMMeta
    | PMTeam
    | PMTimeBalance
    | PMTimereport
    | PMUser
    | PMUserCapacity
    | PMAbsence
    -- That's why!
    | PMEnumDesc String
    -- Special
    | PMVector PlanMillTypeRep
    deriving (Eq, Show)

instance PlanMillTypeable PM.Meta where planmillTypeRep _ = PMMeta
instance PlanMillTypeable PM.Team where planmillTypeRep _ = PMTeam
instance PlanMillTypeable PM.TimeBalance where planmillTypeRep _ = PMTimeBalance
instance PlanMillTypeable PM.Timereport where planmillTypeRep _ = PMTimereport
instance PlanMillTypeable PM.User where planmillTypeRep _ = PMUser
instance PlanMillTypeable PM.UserCapacity where planmillTypeRep _ = PMUserCapacity
instance PlanMillTypeable PM.Absence where planmillTypeRep _ = PMAbsence

instance KnownSymbol sym => PlanMillTypeable (PM.EnumDesc sym) where
    planmillTypeRep _ = PMEnumDesc (symbolVal (Proxy :: Proxy sym))

instance PlanMillTypeable a => PlanMillTypeable (Vector a) where
    planmillTypeRep _ = PMVector (planmillTypeRep (Proxy :: Proxy a))

-------------------------------------------------------------------------------
-- PlanMillTypeable machinery
-------------------------------------------------------------------------------

class PlanMillTypeable a where
    planmillTypeRep :: Proxy a -> PlanMillTypeRep

planmillTypeOf :: forall a. PlanMillTypeable a => a -> PlanMillTypeRep
planmillTypeOf _ = planmillTypeRep (Proxy :: Proxy a)

data PlanMillDynamic where
    MkPlanMillDynamic :: PlanMillTypeable a => a -> PlanMillDynamic

toPMDyn :: PlanMillTypeable a => a -> PlanMillDynamic
toPMDyn = MkPlanMillDynamic

fromPMDyn :: forall a. PlanMillTypeable a => PlanMillDynamic -> Maybe a
fromPMDyn (MkPlanMillDynamic b)
    | at == bt  = Just $ unsafeCoerce b
    | otherwise = Nothing
  where
    at = planmillTypeRep (Proxy :: Proxy a)
    bt = planmillTypeOf b

-------------------------------------------------------------------------------
-- Dynamic Map
-------------------------------------------------------------------------------

newtype PlanMillDynMap k = DynMap (Map k PlanMillDynamic)

insert
    :: (Ord k, PlanMillTypeable v)
    => k -> v -> PlanMillDynMap k -> PlanMillDynMap k
insert k v (DynMap m) = DynMap $ Map.insert k (toPMDyn v) m

lookup
    :: (Ord k, PlanMillTypeable v)
    => k -> PlanMillDynMap k -> Maybe v
lookup k (DynMap m) = Map.lookup k m >>= fromPMDyn

size :: PlanMillDynMap k -> Int
size (DynMap m) = Map.size m

instance Ord k => Semigroup (PlanMillDynMap k) where
    DynMap a <> DynMap b = DynMap (a <> b)

instance Ord k => Monoid (PlanMillDynMap k) where
    mempty = DynMap mempty
    mappend = (<>)

-------------------------------------------------------------------------------
-- Class for making cached planmill actions
-------------------------------------------------------------------------------

class (Binary a, HasSemanticVersion a, HasStructuralInfo a, FromJSON a, PlanMillTypeable a) => BinaryFromJSON a
instance (Binary a, HasSemanticVersion a, HasStructuralInfo a, FromJSON a, PlanMillTypeable a) => BinaryFromJSON a

instance (ForallSymbols FromJSON e, ForallSymbols Binary e, ForallSymbols HasStructuralInfo e, ForallSymbols HasSemanticVersion e, ForallSymbols PlanMillTypeable e)
    => ForallSymbols BinaryFromJSON e
  where
    specialiseUniversal _ f s =
        case proxies of
            (Dict, Dict, Dict, Dict, Dict) -> Dict
      where
        proxies =
            ( specialiseUniversal (Proxy :: Proxy FromJSON) f s
            , specialiseUniversal (Proxy :: Proxy Binary) f s
            , specialiseUniversal (Proxy :: Proxy HasStructuralInfo) f s
            , specialiseUniversal (Proxy :: Proxy HasSemanticVersion) f s
            , specialiseUniversal (Proxy :: Proxy PlanMillTypeable) f s
            )

instance ForallSymbols PlanMillTypeable PM.EnumDesc where
    specialiseUniversal _ _ _ = Dict
