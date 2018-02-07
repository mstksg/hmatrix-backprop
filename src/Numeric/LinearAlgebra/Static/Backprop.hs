{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE PolyKinds                                #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

-- | https://people.maths.ox.ac.uk/gilesm/files/NA-08-01.pdf

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
  , vector
  , linspace
  , H.range
  , H.dim
  , H.L
  , H.Sq
  , row
  , col
  , (|||)
  , (===)
  , splitRows
  , splitCols
  , unrow
  , uncol
  , tr
  , H.eye
  , diag
  , matrix
  , H.ℂ
  , H.C
  , H.M
  , H.Her
  , H.𝑖
  , (<>)
  , (#>)
  , (<.>)
  , svd
  , H.Eigen
  , eigensystem
  , eigenvalues
  , chol
  , H.Normed
  , norm_0
  , norm_1
  , norm_2
  , norm_Inf
  , mean
  , meanCov
  , meanL
  , H.Domain
  , mul
  , app
  , dot
  , cross
  , diagR
  , dvmap
  , dvmap'
  , dmmap
  , dmmap'
  , outer
  , zipWithVector
  , zipWithVector'
  , det
  , invlndet
  , lndet
  , inv
  , toRows
  , toColumns
  , fromRows
  , fromColumns
  , takeDiag
  , H.Sym
  , sym
  , mTm
  , (<·>)
  ) where

import           Data.Maybe
import           Data.Proxy
import           Foreign.Storable
import           GHC.TypeLits
import           Lens.Micro hiding            ((&))
import           Numeric.Backprop
import           Numeric.Backprop.Tuple
import           Unsafe.Coerce
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Sized            as SV
import qualified Numeric.LinearAlgebra        as HU
import qualified Numeric.LinearAlgebra.Devel  as HU
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
-- TODO: opIsoN

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
    :: (Reifies s W, KnownNat p, KnownNat n, p <= n)
    => BVar s (H.R n)
    -> (BVar s (H.R p), BVar s (H.R (n - p)))
split v = (t ^^. _1, t ^^. _2)      -- should we just return the T2 ?
  where
    t = liftOp1 (opIso (tupT2         . H.split)
                       (uncurry (H.#) . t2Tup  )    -- TODO: uncurryT2
                ) v

headTail
    :: (Reifies s W, KnownNat n, 1 <= n)
    => BVar s (H.R n)
    -> (BVar s H.ℝ, BVar s (H.R (n - 1)))
headTail v = (t ^^. _1, t ^^. _2)
  where
    t = liftOp1 (opIso (tupT2 . H.headTail)
                       (\(T2 d dx) -> (H.konst d :: H.R 1) H.# dx)
                ) v

-- | Potentially extremely bad for anything but short lists!!!
vector
    :: forall n s. (Reifies s W, KnownNat n)
    => [BVar s H.ℝ]
    -> BVar s (H.R n)
vector vs = case SV.fromList @n vs of
    Nothing  -> error "vector: invalid number of elements"
    Just vs' ->
        liftOp1 (opIso (fromJust . H.create   . VG.convert . SV.fromSized)
                       (fromJust . SV.toSized . VG.convert . H.extract   )
                )
                (collectVar vs')

linspace
    :: forall n s. (Reifies s W, KnownNat n)
    => BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s (H.R n)
linspace = liftOp2 . op2 $ \l u ->
    ( H.linspace (l, u)
    , \d -> let n1 = fromInteger $ natVal (Proxy @n) - 1
                dDot = ((H.range - 1) H.<.> d) / n1
                dSum = HU.sumElements . H.extract $ d
            in  (dSum - dDot, dDot)
    )

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
infixl 3 |||

(===) :: (Reifies s W, KnownNat c, KnownNat r1, KnownNat (r1 + r2))
      => BVar s (H.L r1        c)
      -> BVar s (H.L r2        c)
      -> BVar s (H.L (r1 + r2) c)
(===) = liftOp2 . op2 $ \x y -> (x H.=== y, H.splitRows)
infixl 2 ===

splitRows
    :: (Reifies s W, KnownNat p, KnownNat m, KnownNat n, p <= m)
    => BVar s (H.L m n)
    -> (BVar s (H.L p n), BVar s (H.L (m - p) n))
splitRows v = (t ^^. _1, t ^^. _2)
  where
    t = liftOp1 (opIso (tupT2 . H.splitRows)
                       (\(T2 dx dy) -> dx H.=== dy)
                ) v

splitCols
    :: (Reifies s W, KnownNat p, KnownNat m, KnownNat n, KnownNat (n - p), p <= n)
    => BVar s (H.L m n)
    -> (BVar s (H.L m p), BVar s (H.L m (n - p)))
splitCols v = (t ^^. _1, t ^^. _2)
  where
    t = liftOp1 (opIso (tupT2 . H.splitCols)
                       (\(T2 dx dy) -> dx H.||| dy)
                ) v

unrow
    :: (Reifies s W, KnownNat n)
    => BVar s (H.L 1 n)
    -> BVar s (H.R n)
unrow = liftOp1 $ opIso H.unrow H.row

uncol
    :: (Reifies s W, KnownNat n)
    => BVar s (H.L n 1)
    -> BVar s (H.R n)
uncol = liftOp1 $ opIso H.uncol H.col

tr  :: (Reifies s W, HU.Transposable m mt, HU.Transposable mt m, Num m, Num mt)
    => BVar s m
    -> BVar s mt
tr = liftOp1 . op1 $ \x -> (H.tr x, H.tr)

diag
    :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.Sq n)
diag = liftOp1 . op1 $ \x -> (H.diag x, H.takeDiag)

-- | Potentially extremely bad for anything but short lists!!!
matrix
    :: forall m n s. (Reifies s W, KnownNat m, KnownNat n)
    => [BVar s H.ℝ]
    -> BVar s (H.L m n)
matrix vs = case SV.fromList @(m * n) vs of
    Nothing  -> error "matrix: invalid number of elements"
    Just vs' ->
        liftOp1 (opIso (fromJust . H.create   . HU.reshape n . VG.convert . SV.fromSized)
                       (fromJust . SV.toSized . VG.convert   . HU.flatten . H.extract   )
                )
                (collectVar vs')
  where
    n = fromInteger $ natVal (Proxy @n)


-- TODO: her??
-- cannot: no Num instance for Her

(<>)
    :: (Reifies s W, KnownNat m, KnownNat k, KnownNat n)
    => BVar s (H.L m k)
    -> BVar s (H.L k n)
    -> BVar s (H.L m n)
(<>) = mul
infixr 8 <>

(#>)
    :: (Reifies s W, KnownNat m, KnownNat n)
    => BVar s (H.L m n)
    -> BVar s (H.R n)
    -> BVar s (H.R m)
(#>) = app
infixr 8 #>

(<.>)
    :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.R n)
    -> BVar s H.ℝ
(<.>) = dot
infixr 8 <.>

-- | Can only get the singular values, for now.  Let me know if you find an
-- algorithm that can compute the gradients based on differentials for the
-- other matricies!
svd :: forall m n s. (Reifies s W, KnownNat m, KnownNat n)
    => BVar s (H.L m n)
    -> BVar s (H.R n)
svd = liftOp1 . op1 $ \x ->
    let (u, σ, v) = H.svd x
    in  ( σ
        , \dΣ -> u H.<> H.diagR 0 dΣ H.<> H.tr v
        )

helpEigen :: KnownNat n => H.Sym n -> (H.R n, H.L n n, H.L n n, H.L n n)
helpEigen x = (l, v, H.inv v, H.tr v)
  where
    (l, v) = H.eigensystem x

eigensystem
    :: forall n s. (Reifies s W, KnownNat n)
    => BVar s (H.Sym n)
    -> (BVar s (H.R n), BVar s (H.L n n))
eigensystem u = (t ^^. _1, t ^^. _2)
  where
    n :: Int
    n = fromInteger $ natVal (Proxy @n)
    o :: Op '[H.Sym n] (T2 (H.R n) (H.L n n))
    o = op1 $ \x ->
        let (l, v, vInv, vTr) = helpEigen x
            Just lRep = H.withRows (replicate n l) H.exactDims
            fMat   = (1 - H.eye) * (lRep - H.tr lRep)
        in  ( T2 l v
            , \(T2 dL dV) -> unsafeCoerce $         -- is this correct?
                       H.tr vInv
                  H.<> (H.diag dL + fMat * (vTr H.<> dV))
                  H.<> vTr
            )
    t = liftOp1 o u

eigenvalues
    :: forall n s. (Reifies s W, KnownNat n)
    => BVar s (H.Sym n)
    -> BVar s (H.R n)
eigenvalues = liftOp1 . op1 $ \x ->
    let (l, _, vInv, vTr) = helpEigen x
    in  ( l
        , \dL -> unsafeCoerce $         -- is this correct?
                 H.tr vInv H.<> H.diag dL H.<> vTr
        )

-- | https://arxiv.org/abs/1602.07527
chol
    :: forall n s. (Reifies s W, KnownNat n)
    => BVar s (H.Sym n)
    -> BVar s (H.Sq n)
chol = liftOp1 . op1 $ \x ->
    let l = H.chol x
        lInv = H.inv l
        phi :: H.Sq n
        phi = H.build $ \i j -> case compare i j of
                                  LT -> 1
                                  EQ -> 0.5
                                  GT -> 0
        -- TODO: imperative algorithm?
    in  ( l
        , \dL -> let s = H.tr lInv H.<> (phi * (H.tr l H.<> dL)) H.<> lInv
                 in  unsafeCoerce $ s + H.tr s - H.eye * s  -- correct?
        )

-- | number of non-zero items
--
-- not really well defined?
norm_0
    :: (Reifies s W, H.Normed a, Num a)
    => BVar s a
    -> BVar s H.ℝ
norm_0 = liftOp1 . op1 $ \x -> (H.norm_0 x, const 0)

-- | Sum of absolute values
--
-- Does this work for matricies?
norm_1
    :: (Reifies s W, H.Normed a, Num a, H.Sized H.ℝ a d)
    => BVar s a
    -> BVar s H.ℝ
norm_1 = liftOp1 . op1 $ \x -> (H.norm_1 x, (* signum x) . H.konst)

-- | Square root of sum of squares
--
-- Be aware that gradient diverges when the norm is zero
--
-- Does this work for matricies?
norm_2
    :: (Reifies s W, H.Normed a, Num a, H.Sized H.ℝ a d)
    => BVar s a
    -> BVar s H.ℝ
norm_2 = liftOp1 . op1 $ \x ->
    let n = H.norm_2 x
    in (n, \d -> x * H.konst (d / n))

-- | Maximum absolute value
--
-- Does this work for matricies?
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
{-# ANN norm_Inf "HLint: ignore Use camelCase" #-}

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
        , \(T2 dMean dCov) ->       -- TODO: replace with uncurryT2
              let Just gradMean = H.withColumns (replicate m dMean) H.exactDims
                  gradCov       = undefined dCov
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

cross
    :: ( Reifies s W
       , H.Domain field vec mat
       , Num (vec 3)
       )
    => BVar s (vec 3)
    -> BVar s (vec 3)
    -> BVar s (vec 3)
cross = liftOp2 . op2 $ \x y ->
    ( x `H.cross` y
    , \d -> (y `H.cross` d, d `H.cross` x)  -- is sign correct?
    )

diagR
    :: forall m n k field vec mat s.
      ( Reifies s W
       , H.Domain field vec mat
       , Num (vec k)
       , Num (mat m n)
       , KnownNat m
       , KnownNat n
       , KnownNat k
       , HU.Container HU.Vector field
       , H.Sized field (mat m n) HU.Matrix
       , H.Sized field (vec k) HU.Vector
       )
    => BVar s field
    -> BVar s (vec k)
    -> BVar s (mat m n)
diagR = liftOp2 . op2 $ \c x ->
    ( H.diagR c x
    , \d -> ( HU.sumElements . H.extract $ H.diagR 1 (0 :: vec k) * d
            , fromJust . H.create . HU.takeDiag . H.extract $ d
            )
    )

dvmap
    :: ( Reifies s W
       , Num (vec n)
       , Storable field
       , Storable (field, field)
       , H.Sized field (vec n) HU.Vector
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
dvmap f = liftOp1 . op1 $ \x ->
    let (y, dx) = HU.unzipVector $ VG.map (backprop f) (H.extract x)
    in  ( fromJust (H.create y)
        , \d -> d * fromJust (H.create dx)
        )

-- | A version of 'dvmap' that is less performant but is based on
-- 'H.zipWithVector' from 'H.Domain'.
dvmap'
    :: ( Reifies s W
       , KnownNat n
       , H.Domain field vec mat
       , Num (vec n)
       , Num field
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
dvmap' f = liftOp1 . op1 $ \x ->
    ( H.dvmap (evalBP f) x
    , (H.dvmap (gradBP f) x *)
    )

dmmap
    :: forall n m mat field s.
       ( Reifies s W
       , KnownNat m
       , Num (mat n m)
       , Storable (field, field)
       , H.Sized field (mat n m) HU.Matrix
       , HU.Element field
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (mat n m)
    -> BVar s (mat n m)
dmmap f = liftOp1 . op1 $ \x ->
    let (y', dx') = HU.unzipVector
                  . VG.map (backprop f)
                  . HU.flatten
                  $ H.extract x
    in  ( fromJust . H.create . HU.reshape m $ y'
        , \d -> (* d) . fromJust . H.create . HU.reshape m $ dx'
        )
  where
    m :: Int
    m = fromInteger $ natVal (Proxy @m)

dmmap'
    :: ( Reifies s W
       , KnownNat n
       , KnownNat m
       , H.Domain field vec mat
       , Num (mat n m)
       , Num field
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (mat n m)
    -> BVar s (mat n m)
dmmap' f = liftOp1 . op1 $ \x ->
    ( H.dmmap (evalBP f) x
    , (H.dmmap (gradBP f) x *)
    )

outer
    :: ( Reifies s W
       , KnownNat m
       , KnownNat n
       , H.Domain field vec mat
       , HU.Transposable (mat n m) (mat m n)
       , Num (vec n)
       , Num (vec m)
       , Num (mat n m)
       )
    => BVar s (vec n)
    -> BVar s (vec m)
    -> BVar s (mat n m)
outer = liftOp2 . op2 $ \x y ->
    ( x `H.outer` y
    , \d -> ( d `H.app` y
            , H.tr d `H.app` x)
    )

zipWithVector
    :: ( Reifies s W
       , Num (vec n)
       , Storable field
       , Storable (field, field, field)
       , H.Sized field (vec n) HU.Vector
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
    -> BVar s (vec n)
zipWithVector f = liftOp2 . op2 $ \(H.extract->x) (H.extract->y) ->
    let (z,dx,dy) = VG.unzip3
                  $ VG.zipWith (\x' y' ->
                      let (z', (dx', dy')) = backprop2 f x' y'
                      in  (z', dx', dy')
                    ) x y
    in  ( fromJust (H.create z)
        , \d -> (d * fromJust (H.create dx), d * fromJust (H.create dy))
        )

-- | A version of 'zipWithVector' that is less performant but is based on
-- 'H.zipWithVector' from 'H.Domain'.
zipWithVector'
    :: ( Reifies s W
       , KnownNat n
       , H.Domain field vec mat
       , Num (vec n)
       , Num field
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
    -> BVar s (vec n)
zipWithVector' f = liftOp2 . op2 $ \x y ->
    ( H.zipWithVector (evalBP2 f) x y
    , \d -> let dx = H.zipWithVector (\x' -> fst . gradBP2 f x') x y
                dy = H.zipWithVector (\x' -> snd . gradBP2 f x') x y
            in  (d * dx, d * dy)
    )

det :: ( Reifies s W
       , KnownNat n
       , Num (mat n n)
       , H.Domain field vec mat
       , H.Sized field (mat n n) d
       , HU.Transposable (mat n n) (mat n n)
       )
    => BVar s (mat n n)
    -> BVar s field
det = liftOp1 . op1 $ \x ->
    let xDet = H.det x
        xInv = H.inv x
    in  ( xDet, \d -> H.konst (d * xDet) * H.tr xInv )

invlndet
    :: forall n mat field vec d s.
       ( Reifies s W
       , KnownNat n
       , Num (mat n n)
       , H.Domain field vec mat
       , H.Sized field (mat n n) d
       , HU.Transposable (mat n n) (mat n n)
       )
    => BVar s (mat n n)
    -> (BVar s (mat n n), (BVar s field, BVar s field))
invlndet v = (t ^^. _1, (t ^^. _2, t ^^. _3))
  where
    o :: Op '[mat n n] (T3 (mat n n) field field)
    o = op1 $ \x ->
      let (i,(ldet, s)) = H.invlndet x
      in  ( T3 i ldet s
          , \(T3 dI dLDet _) ->
                let gradI    = -dI * (i `H.mul` i)
                    gradLDet = H.konst dLDet * H.tr i
                in  gradI + gradLDet
          )
    t = liftOp1 o v

lndet
    :: forall n mat field vec d s.
       ( Reifies s W
       , KnownNat n
       , Num (mat n n)
       , H.Domain field vec mat
       , H.Sized field (mat n n) d
       , HU.Transposable (mat n n) (mat n n)
       )
    => BVar s (mat n n)
    -> BVar s field
lndet = liftOp1 . op1 $ \x ->
          let (i,(ldet,_)) = H.invlndet x
          in  (ldet, (* H.tr i) . H.konst)

-- TODO: expm ???
-- https://people.maths.ox.ac.uk/gilesm/files/NA-08-01.pdf

-- TODO: sqrtm ???
-- http://people.cs.umass.edu/~smaji/projects/matrix-sqrt/

inv :: ( Reifies s W
       , KnownNat n
       , Num (mat n n)
       , H.Domain field vec mat
       )
    => BVar s (mat n n)
    -> BVar s (mat n n)
inv = liftOp1 . op1 $ \x ->
    let xInv = H.inv x
    in  ( xInv, \d -> -d * (xInv `H.mul` xInv) )

-- TODO: withRows/withCols ??
-- Is it possible or meaningful?

rowsV :: (KnownNat m, KnownNat n) => H.L m n -> SV.Vector m (H.R n)
rowsV = fromJust . SV.fromList . H.toRows

colsV :: (KnownNat m, KnownNat n) => H.L m n -> SV.Vector n (H.R m)
colsV = fromJust . SV.fromList . H.toColumns

vRows :: (KnownNat m, KnownNat n) => SV.Vector m (H.R n) -> H.L m n
vRows = (`H.withRows` fromJust . H.exactDims) . SV.toList

vCols :: (KnownNat m, KnownNat n) => SV.Vector n (H.R m) -> H.L m n
vCols = (`H.withColumns` fromJust . H.exactDims) . SV.toList

toRows
    :: forall m n s. (Reifies s W, KnownNat m, KnownNat n)
    => BVar s (H.L m n)
    -> SV.Vector m (BVar s (H.R n))
toRows = sequenceVar . liftOp1 (opIso rowsV vRows)

toColumns
    :: forall m n s. (Reifies s W, KnownNat m, KnownNat n)
    => BVar s (H.L m n)
    -> SV.Vector n (BVar s (H.R m))
toColumns = sequenceVar . liftOp1 (opIso colsV vCols)

fromRows
    :: forall m n s. (Reifies s W, KnownNat m, KnownNat n)
    => SV.Vector m (BVar s (H.R n))
    -> BVar s (H.L m n)
fromRows = liftOp1 (opIso vRows rowsV) . collectVar

fromColumns
    :: forall m n s. (Reifies s W, KnownNat m, KnownNat n)
    => SV.Vector m (BVar s (H.R n))
    -> BVar s (H.L m n)
fromColumns = liftOp1 (opIso vRows rowsV) . collectVar

takeDiag
    :: ( Reifies s W
       , KnownNat n
       , H.Diag (mat n n) (vec n)
       , H.Domain field vec mat
       , Num (vec n)
       , Num (mat n n)
       , Num field
       )
    => BVar s (mat n n)
    -> BVar s (vec n)
takeDiag = liftOp1 . op1 $ \x ->
    ( H.takeDiag x
    , H.diagR 0
    )

sym :: (Reifies s W, KnownNat n)
    => BVar s (H.Sq n)
    -> BVar s (H.Sym n)
sym = liftOp1 . op1 $ \x ->
    ( H.sym x
    , H.unSym . H.sym . H.unSym
    )

mTm :: (Reifies s W, KnownNat m, KnownNat n)
    => BVar s (H.L m n)
    -> BVar s (H.Sym n)
mTm = liftOp1 . op1 $ \x ->
    ( H.mTm x
    , \d -> 2 * (x H.<> H.unSym d)
    )

-- TODO: unSym ??? not possible?  the gradient function would be a lie,
-- potentially.
--
-- But, does that matter?

(<·>)
    :: (Reifies s W, KnownNat n)
    => BVar s (H.R n)
    -> BVar s (H.R n)
    -> BVar s H.ℝ
(<·>) = dot
infixr 8 <·>

