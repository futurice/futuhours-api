{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Generics.SOP.Curry where

import Generics.SOP

type family HFn (xs :: [*]) (r :: *) :: *
type instance HFn '[]       r = r
type instance HFn (x ': xs) r = x -> HFn xs r

{-
hUncurry :: HFn xs r -> NP I xs -> r
hUncurry = undefined
-}

hCurry :: forall xs r. SListI xs => (NP I xs -> r) -> HFn xs r
hCurry f = hCurry' (sList :: SList xs) f
  where
    hCurry' :: SList ys -> (NP I ys -> r) -> HFn ys r
    hCurry' SNil  g = g Nil
    hCurry' SCons g = \y -> hCurry' sList (\ys -> g (I y :* ys))
