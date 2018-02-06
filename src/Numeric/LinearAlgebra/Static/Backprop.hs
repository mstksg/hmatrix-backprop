{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Numeric.LinearAlgebra.Static.Backprop (
    H.R
  , H.ℝ
  , vec2
  , vec3
  , vec4
  , (&)
  , (#)
  , H.range
  , H.dim
  , H.L
  , H.Sq
  , row
  , col
  , (|||)
  , (===)
  , tr
  , H.eye
  , diag
  , H.ℂ
  , H.C
  , H.M
  , H.Her
  , H.𝑖
  , (<>)
  , (#>)
  , (<.>)
  ) where

import           GHC.TypeLits
import           Numeric.Backprop
import qualified Numeric.LinearAlgebra        as HU
import qualified Numeric.LinearAlgebra.Static as H

vec2
    :: Reifies s W
    => BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s (H.R 2)
vec2 = liftOp2 . op2 $ \x y ->
    ( H.vec2 x y
    , \(HU.toList . H.extract -> [dx,dy]) -> (dx, dy)
    )

vec3
    :: Reifies s W
    => BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s (H.R 3)
vec3 = liftOp3 . op3 $ \x y z ->
    ( H.vec3 x y z
    , \(HU.toList . H.extract -> [dx,dy,dz]) -> (dx, dy, dz)
    )

vec4
    :: Reifies s W
    => BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s (H.R 4)
vec4 x y z w = liftOp o (x :< y :< z :< w :< Ø)
  where
    o :: Op '[H.ℝ, H.ℝ, H.ℝ, H.ℝ] (H.R 4)
    o = Op $ \(x' ::< y' ::< z' ::< w' ::< Ø) ->
      ( H.vec4 x' y' z' w'
      , \(HU.toList . H.extract -> [dx,dy,dz,dw]) -> dx ::< dy ::< dz ::< dw ::< Ø
      )

(&) :: (Reifies s W, KnownNat n, 1 <= n, KnownNat (n + 1))
    => BVar s (H.R n)
    -> BVar s H.ℝ
    -> BVar s (H.R (n + 1))
(&) = liftOp2 . op2 $ \xs x ->
    ( xs H.& x
    , \(H.split->(dxs,dx)) -> (dxs, fst (H.headTail dx))
    )
infixl 4 &

(#) :: (Reifies s W, KnownNat n, KnownNat m)
    => BVar s (H.R n)
    -> BVar s (H.R m)
    -> BVar s (H.R (n + m))
(#) = liftOp2 . op2 $ \x y -> ( x H.# y, H.split )
infixl 4 #

-- linspace

-- build ???

row :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.L 1 n)
row = liftOp1 $ opIso H.row H.unrow

col :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.L n 1)
col = liftOp1 $ opIso H.col H.uncol

(|||) :: (Reifies s W, KnownNat c, KnownNat r1, KnownNat r1, KnownNat (r1 + r2))
      => BVar s (H.L c r1)
      -> BVar s (H.L c r2)
      -> BVar s (H.L c (r1 + r2))
(|||) = liftOp2 . op2 $ \x y -> (x H.||| y, H.splitCols)

(===) :: (Reifies s W, KnownNat c, KnownNat r1, KnownNat r1, KnownNat (r1 + r2))
      => BVar s (H.L r1        c)
      -> BVar s (H.L r2        c)
      -> BVar s (H.L (r1 + r2) c)
(===) = liftOp2 . op2 $ \x y -> (x H.=== y, H.splitRows)

tr  :: (Reifies s W, HU.Transposable m mt, HU.Transposable mt m, Num m, Num mt)
    => BVar s m
    -> BVar s mt
tr = liftOp1 . op1 $ \x -> (H.tr x, H.tr)

diag
    :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.Sq n)
diag = liftOp1 . op1 $ \x -> (H.diag x, H.takeDiag)

(<>)
    :: (Reifies s W, KnownNat m, KnownNat k, KnownNat n)
    => BVar s (H.L m k)
    -> BVar s (H.L k n)
    -> BVar s (H.L m n)
(<>) = liftOp2 . op2 $ \x y ->
    ( x H.<> y
    , \d -> (d H.<> H.tr y, H.tr x H.<> d)
    )

(#>)
    :: (Reifies s W, KnownNat m, KnownNat n)
    => BVar s (H.L m n)
    -> BVar s (H.R n)
    -> BVar s (H.R m)
(#>) = liftOp2 . op2 $ \xs y ->
    ( xs H.#> y
    , \d -> (d `H.outer` y, H.tr xs H.#> d)
    )

(<.>)
    :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.R n)
    -> BVar s H.ℝ
(<.>) = liftOp2 . op2 $ \x y ->
    ( x H.<.> y
    , \d -> let d' = H.konst d
            in  (d' * y, x * d')
    )
