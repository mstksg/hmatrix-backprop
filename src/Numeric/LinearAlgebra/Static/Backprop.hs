{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
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
  , split
  , headTail
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
  , norm_0
  , norm_1
  , norm_2
  , norm_Inf
  , mean
  , meanCov
  , meanL
  , mul
  , app
  , dot
  , dvmap
  , dmmap
  ) where

import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           Lens.Micro hiding            ((&))
import           Numeric.Backprop
import           Numeric.Backprop.Tuple
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

split
    :: (Reifies s W, KnownNat n, KnownNat m)
    => BVar s (H.R (n + m))
    -> (BVar s (H.R n), BVar s (H.R m))
split v = (t ^^. _1, t ^^. _2)      -- should we just return the T2 ?
  where
    t = ($ v) . liftOp1 . op1 $ \x ->
        ( tupT2 (H.split x)
        , uncurry (H.#) . t2Tup
        )

headTail
    :: (Reifies s W, KnownNat n, 1 <= n)
    => BVar s (H.R n)
    -> (BVar s H.ℝ, BVar s (H.R (n - 1)))
headTail v = (t ^^. _1, t ^^. _2)
  where
    t = ($ v) . liftOp1 . op1 $ \x ->
            ( tupT2 (H.headTail x)
            , \(T2 d ds) -> (H.konst d :: H.R 1) H.# ds
            )

-- TODO: linspace

-- TODO: build ???

row :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.L 1 n)
row = liftOp1 $ opIso H.row H.unrow

col :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.L n 1)
col = liftOp1 $ opIso H.col H.uncol

(|||) :: (Reifies s W, KnownNat c, KnownNat r1, KnownNat (r1 + r2))
      => BVar s (H.L c r1)
      -> BVar s (H.L c r2)
      -> BVar s (H.L c (r1 + r2))
(|||) = liftOp2 . op2 $ \x y -> (x H.||| y, H.splitCols)

(===) :: (Reifies s W, KnownNat c, KnownNat r1, KnownNat (r1 + r2))
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

-- | Length
norm_0
    :: (Reifies s W, H.Normed a, Num a)
    => BVar s a
    -> BVar s H.ℝ
norm_0 = liftOp1 . op1 $ \x -> (H.norm_0 x, const 0)

-- | Sum of absolute values
norm_1
    :: (Reifies s W, H.Normed a, Num a, H.Sized H.ℝ a d)
    => BVar s a
    -> BVar s H.ℝ
norm_1 = liftOp1 . op1 $ \x -> (H.norm_1 x, (* signum x) . H.konst)

-- | Square root of sum of squares
norm_2
    :: (Reifies s W, H.Normed a, Num a, H.Sized H.ℝ a d)
    => BVar s a
    -> BVar s H.ℝ
norm_2 = liftOp1 . op1 $ \x -> let n = H.norm_2 x
                               in (n, \d -> x * H.konst (d / n))
                                  -- TODO: handle n == 0

-- | Maximum absolute value
norm_Inf
    :: (Reifies s W, H.Normed a, Num a, H.Sized H.ℝ a d, HU.Container d H.ℝ)
    => BVar s a
    -> BVar s H.ℝ
norm_Inf = liftOp1 . op1 $ \x ->
    let n :: H.ℝ
        n = H.norm_Inf x
        setD d = HU.cmap $ \e -> if abs e == n
                                   then signum e * d
                                   else 0
    in  (n, \d -> fromJust . H.create . setD d . H.extract $ x)

mean
    :: (Reifies s W, KnownNat n, 1 <= n)
    => BVar s (H.R n)
    -> BVar s H.ℝ
mean = liftOp1 . op1 $ \x -> (H.mean x, H.konst . (/ H.norm_0 x))

meanCov
    :: forall m n s. (Reifies s W, KnownNat n, KnownNat m, 1 <= m)
    => BVar s (H.L m n)
    -> (BVar s (H.R n), BVar s (H.Sym n))
meanCov v = (t ^^. _1, t ^^. _2)
  where
    m = fromInteger $ natVal (Proxy @m)
    t = ($ v) . liftOp1 . op1 $ \x ->
        ( tupT2 (H.meanCov x)
        , \(T2 dMean dCov) ->
              let Just gradMean = H.withColumns (replicate m dMean) H.exactDims
                  gradCov       = undefined
              in  gradMean + gradCov
        )

-- | 'meanCov', but if you know you won't use the covariance.
meanL
    :: forall m n s. (Reifies s W, KnownNat n, KnownNat m, 1 <= m)
    => BVar s (H.L m n)
    -> BVar s (H.R n)
meanL = liftOp1 . op1 $ \x ->
    ( fst (H.meanCov x)
    , fromJust . (`H.withColumns` H.exactDims) . replicate m
    )
  where
    m = fromInteger $ natVal (Proxy @m)


--     => BVar s a
--     -> BVar s H.ℝ
-- norm_Inf = liftOp1 . op1 $ \x ->
--     let n :: H.ℝ
--         n = H.norm_Inf x
--         setD d = HU.cmap $ \e -> if abs e == n
--                                    then signum e * d
--                                    else 0
--     in  (n, \d -> fromJust . H.create . setD d . H.extract $ x)

mul :: ( Reifies s W
       , KnownNat m
       , KnownNat k
       , KnownNat n
       , H.Domain field vec mat
       , Num (mat m k)
       , Num (mat k n)
       , Num (mat m n)
       , HU.Transposable (mat m k) (mat k m)
       , HU.Transposable (mat k n) (mat n k)
       )
    => BVar s (mat m k)
    -> BVar s (mat k n)
    -> BVar s (mat m n)
mul = liftOp2 . op2 $ \x y ->
    ( x `H.mul` y
    , \d -> (d `H.mul` H.tr y, H.tr x `H.mul` d)
    )

app :: ( Reifies s W
       , KnownNat m
       , KnownNat n
       , H.Domain field vec mat
       , Num (mat m n)
       , Num (vec n)
       , Num (vec m)
       , HU.Transposable (mat m n) (mat n m)
       )
    => BVar s (mat m n)
    -> BVar s (vec n)
    -> BVar s (vec m)
app = liftOp2 . op2 $ \xs y ->
    ( xs `H.app` y
    , \d -> (d `H.outer` y, H.tr xs `H.app` d)
    )

dot :: ( Reifies s W
       , KnownNat n
       , H.Domain field vec mat
       , H.Sized field (vec n) d
       , Num (vec n)
       )
    => BVar s (vec n)
    -> BVar s (vec n)
    -> BVar s field
dot = liftOp2 . op2 $ \x y ->
    ( x `H.dot` y
    , \d -> let d' = H.konst d
            in  (d' * y, x * d')
    )

-- TODO: cross
-- TODO: diagR

dvmap
    :: (Reifies s W, KnownNat n, H.Domain field vec mat, Num (vec n), Num field)
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
dvmap f = liftOp1 . op1 $ \x ->
    ( H.dvmap (evalBP f) x
    , (H.dvmap (gradBP f) x *)
    )

dmmap
    :: (Reifies s W, KnownNat n, KnownNat m, H.Domain field vec mat, Num (mat n m), Num field)
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (mat n m)
    -> BVar s (mat n m)
dmmap f = liftOp1 . op1 $ \x ->
    ( H.dmmap (evalBP f) x
    , (H.dmmap (gradBP f) x *)
    )

