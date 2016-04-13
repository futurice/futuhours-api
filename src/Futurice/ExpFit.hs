{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Futurice.ExpFit (expfn, expfit) where

import Linear
import Numeric.AD
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse, Tape)

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
-- λ *Futurice.ExpFit > let kab = expfit (14, 15) (21, 3* 60) (180, 24 * 60)
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
    :: (Double, Double) -> (Double, Double) -> (Double, Double)
    -> (Double, Double, Double)
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

    convergedOrNth :: Int -> [V3 Double] -> V3 Double
    convergedOrNth = go
      where
        go _ []                = return (0/0)
        go _ [x]               = x
        go 0 (x:_)             = x
        go n (x:xs@(y:_))
            | nearZero (x - y) = y
            | otherwise        = go (n-1) xs

expfn :: (Double, Double, Double) -> Double -> Double
expfn (k, a, b) t = k * exp (a * t) + b

findZeroV3
    :: (forall s. Reifies s Tape => V3 (Reverse s Double) -> V3 (Reverse s Double))
    -> V3 Double    -- ^ Initial point
    -> [V3 Double]
findZeroV3 f = iterate go where
  go x = xn where
    j = jacobian' f x

    y :: V3 Double
    y = fmap fst j

    y' :: M33 Double
    y' = fmap snd j

    xn | nearZero y  = x
       | otherwise   = x - inv33 y' !* y
