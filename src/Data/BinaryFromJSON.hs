{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.BinaryFromJSON (
    BinaryFromJSON,
    ) where

import Futurice.Prelude hiding (lookup)
import Prelude          ()

import Data.Aeson                       (FromJSON)
import Data.Binary.Tagged               (HasSemanticVersion, HasStructuralInfo)
import Futurice.Constraint.ForallSymbol (Dict (..), ForallFSymbol (..))

class (Binary a, HasSemanticVersion a, HasStructuralInfo a, FromJSON a, Show a, Typeable a) => BinaryFromJSON a
instance (Binary a, HasSemanticVersion a, HasStructuralInfo a, FromJSON a, Show a, Typeable a) => BinaryFromJSON a
instance ( ForallFSymbol FromJSON e
         , ForallFSymbol Binary e
         , ForallFSymbol HasStructuralInfo e
         , ForallFSymbol HasSemanticVersion e
         , ForallFSymbol Show e
         , ForallFSymbol Typeable e
         )
    => ForallFSymbol BinaryFromJSON e
  where
    instFSymbol _ f s =
        case proxies of
            (Dict, Dict, Dict, Dict, Dict, Dict) -> Dict
      where
        proxies =
            ( instFSymbol (Proxy :: Proxy FromJSON) f s
            , instFSymbol (Proxy :: Proxy Binary) f s
            , instFSymbol (Proxy :: Proxy HasStructuralInfo) f s
            , instFSymbol (Proxy :: Proxy HasSemanticVersion) f s
            , instFSymbol (Proxy :: Proxy Show) f s
            , instFSymbol (Proxy :: Proxy Typeable) f s
            )
