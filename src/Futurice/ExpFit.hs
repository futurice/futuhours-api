{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.ExpFit (expfn, expfit) where

import Linear
import Numeric.AD.Mode.Forward (AD, Forward, auto, jacobian')

-- | Fit three points into @k * exp (a * t) + b@ function.
--
-- We solve this using Newton method
--
-- @
-- x_n+1 = x_n - jacobian f n ^ -1 * f n
-- @
--
-- /Example:/
--
-- @
-- λ *Futurice.ExpFit > let kab = expfit (14, 15) (21, 3* 60) (180, 24 * 60) :: (Double, Double, Double)
-- λ *Futurice.ExpFit > kab
-- (-1918.6504389149056,-1.634461945864552e-2,1541.2250057697918)
-- λ *Futurice.ExpFit > expfn kab 14
-- 15.00000000000523
-- λ *Futurice.ExpFit > expfn kab 21
-- 179.999999999995
-- λ *Futurice.ExpFit > expfn kab 180
-- 1439.999999999822
-- λ *Futurice.ExpFit > expfn kab 16
-- 64.08449561243242
-- -- @
expfit
    :: forall a. (Floating a, Epsilon a)
    => (a, a) -> (a, a) -> (a, a)
    -> (a, a, a)
expfit (x1, y1) (x2, y2) (x3, y3) = toTriple . convergedOrNth 1000 $
    findZeroV3
        (\(V3 k a b) -> V3
            (k * exp (a * auto x1) + b - auto y1)
            (k * exp (a * auto x2) + b - auto y2)
            (k * exp (a * auto x3) + b - auto y3))
        (V3 1 1 0)
  where
    toTriple :: V3 a -> (a, a, a)
    toTriple (V3 a b c) = (a, b, c)

    convergedOrNth :: Int -> [V3 a] -> V3 a
    convergedOrNth = go
      where
        go _ []                = return (0/0)
        go _ [x]               = x
        go 0 (x:_)             = x
        go n (x:xs@(y:_))
            | nearZero (x - y) = y
            | otherwise        = go (n-1) xs

expfn :: Floating a => (a, a, a) -> a -> a
expfn (k, a, b) t = k * exp (a * t) + b

findZeroV3
    :: forall a. (Floating a, Epsilon a)
    => (forall s. V3 (AD s (Forward a)) -> V3 (AD s (Forward a)))
    -> V3 a    -- ^ Initial point
    -> [V3 a]
findZeroV3 f = iterate go where
  go x = xn where
    j = jacobian' f x

    y :: V3 a
    y = fmap fst j

    y' :: M33 a
    y' = fmap snd j

    xn :: V3 a
    xn | nearZero y  = x
       | otherwise   = x - inv33 y' !* y
