{-# LANGUAGE CPP                                      #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE PolyKinds                                #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# OPTIONS_GHC -fno-warn-orphans                     #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

-- |
-- Module      : Numeric.LinearAlgebra.Static.Backprop
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- A wrapper over "Numeric.LinearAlgebra.Static" (type-safe vector and
-- matrix operations based on blas/lapack) that allows its operations to
-- work with <https://hackage.haskell.org/package/backprop backprop>.  Also
-- provides orphan instances of 'Backprop' for types in
-- "Numeric.LinearAlgebra.Static".
--
-- In short, these functions are "lifted" to work with 'BVar's.
--
-- Using 'evalBP' will run the original operation:
--
-- @
-- 'evalBP' :: (forall s. 'Reifies' s 'W'. 'BVar' s a -> 'BVar' s b) -> a -> b
-- @
--
-- But using 'gradBP' or 'backprop' will give you the gradient:
--
-- @
-- 'gradBP' :: (forall s. 'Reifies' s 'W'. 'BVar' s a -> 'BVar' s b) -> a -> a
-- @
--
-- These can act as a drop-in replacement to the API of
-- "Numeric.LinearAlgebra.Static".  Just change your imports, and your
-- functions are automatically backpropagatable.  Useful types are all
-- re-exported.
--
-- Also contains 'sumElements' 'BVar' operation.
--
-- Formulas for gradients come from the following papers:
--
--     * https://people.maths.ox.ac.uk/gilesm/files/NA-08-01.pdf
--     * http://www.dtic.mil/dtic/tr/fulltext/u2/624426.pdf
--     * http://www.cs.cmu.edu/~zkolter/course/15-884/linalg-review.pdf
--     * https://arxiv.org/abs/1602.07527
--
-- Some functions are notably unlifted:
--
--     * 'H.svd': I can't find any resources that allow you to backpropagate
--     if the U and V matrices are used!  If you find one, let me know, or
--     feel free to submit a PR!  Because of this, Currently only a version
--     that exports only the singular values is exported.
--     * 'H.svdTall', 'H.svdFlat': Not sure where to start for these
--     * 'qr': Same story.
--     https://github.com/tensorflow/tensorflow/issues/6504 might yield
--     a clue?
--     * 'H.her': No 'Num' instance for 'H.Her' makes this impossible at
--     the moment with the current backprop API
--     * 'H.exmp': Definitely possible, but I haven't dug deep enough to
--     figure it out yet!  There is a description here
--     https://people.maths.ox.ac.uk/gilesm/files/NA-08-01.pdf but it
--     requires some things I am not familiar with yet.  Feel free to
--     submit a PR!
--     * 'H.sqrtm': Also likely possible.  Maybe try to translate
--     http://people.cs.umass.edu/~smaji/projects/matrix-sqrt/ ?  PRs
--     welcomed!
--     * 'H.linSolve': Haven't figured out where to start!
--     * 'H.</>': Same story
--     * Functions returning existential types, like 'H.withNullSpace',
--     'H.withOrth', 'H.withRows', etc.; not quite sure what the best way
--     to handle these are at the moment.
--     * 'H.withRows' and 'H.withColumns' made "type-safe", without
--     existential types, with 'fromRows' and 'fromColumns'.

module Numeric.LinearAlgebra.Static.Backprop (
  -- * Vector
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
  -- * Matrix
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
  -- * Complex
  , H.ℂ
  , H.C
  , H.M
  , H.𝑖
  -- * Products
  , (<>)
  , (#>)
  , (<.>)
  -- * Factorizations
  , svd
  , svd_
  , H.Eigen
  , eigensystem
  , eigenvalues
  , chol
  -- * Norms
  , H.Normed
  , norm_0
  , norm_1V
  , norm_1M
  , norm_2V
  , norm_2M
  , norm_InfV
  , norm_InfM
  -- * Misc
  , mean
  , meanCov
  , meanL
  , cov
  , H.Disp(..)
  -- ** Domain
  , H.Domain
  , mul
  , app
  , dot
  , cross
  , diagR
  , vmap
  , vmap'
  , dvmap
  , mmap
  , mmap'
  , dmmap
  , outer
  , zipWithVector
  , zipWithVector'
  , dzipWithVector
  , det
  , invlndet
  , lndet
  , inv
  -- ** Conversions
  , toRows
  , toColumns
  , fromRows
  , fromColumns
  -- ** Misc Operations
  , konst
  , sumElements
  , extractV
  , extractM
  , create
  , H.Diag
  , takeDiag
  , H.Sym
  , sym
  , mTm
  , unSym
  , (<·>)
  -- * Backprop types re-exported
  -- | Re-exported for convenience.
  --
  -- @since 0.1.1.0
  , BVar
  , Backprop
  , Reifies
  , W
  ) where

import           Data.Bifunctor
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Proxy
import           Foreign.Storable
import           GHC.TypeLits
import           Lens.Micro hiding                   ((&))
import           Numeric.Backprop
import           Numeric.Backprop.Class
import           Unsafe.Coerce
import qualified Data.Vector                         as V
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Generic.Sized           as SVG
import qualified Data.Vector.Sized                   as SV
import qualified Data.Vector.Storable.Sized          as SVS
import qualified Numeric.Backprop.Explicit           as BE
import qualified Numeric.LinearAlgebra               as HU
import qualified Numeric.LinearAlgebra.Static        as H
import qualified Numeric.LinearAlgebra.Static.Vector as H
import qualified Prelude.Backprop                    as B

#if MIN_VERSION_base(4,11,0)
import           Prelude hiding               ((<>))
#endif

instance Backprop (H.R n) where
    zero = zeroNum
    add = addNum
    one = oneNum

instance Backprop (H.C n) where
    zero = zeroNum
    add = addNum
    one = oneNum

instance (KnownNat n, KnownNat m) => Backprop (H.L n m) where
    zero = zeroNum
    add = addNum
    one = oneNum

instance (KnownNat n, KnownNat m) => Backprop (H.M n m) where
    zero = zeroNum
    add = addNum
    one = oneNum

instance KnownNat n => Backprop (H.Sym n) where
    zero = zeroNum
    add = addNum
    one = oneNum

vec2
    :: Reifies s W
    => BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s (H.R 2)
vec2 = isoVar2 H.vec2 (\(H.rVec->v) -> (SVS.index v 0, SVS.index v 1))
{-# INLINE vec2 #-}

vec3
    :: Reifies s W
    => BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s (H.R 3)
vec3 = isoVar3 H.vec3 (\(H.rVec->v) -> (SVS.index v 0, SVS.index v 1, SVS.index v 2))
{-# INLINE vec3 #-}

vec4
    :: Reifies s W
    => BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s H.ℝ
    -> BVar s (H.R 4)
vec4 vX vY vZ vW = isoVarN
    (\(Identity x :& Identity y :& Identity z :& Identity w :& RNil) -> H.vec4 x y z w)
    (\(H.rVec->v) -> Identity (SVS.index v 0)
                  :& Identity (SVS.index v 1)
                  :& Identity (SVS.index v 2)
                  :& Identity (SVS.index v 3)
                  :& RNil
    )
    (vX :& vY :& vZ :& vW :& RNil)
{-# INLINE vec4 #-}

(&) :: (KnownNat n, 1 <= n, KnownNat (n + 1), Reifies s W)
    => BVar s (H.R n)
    -> BVar s H.ℝ
    -> BVar s (H.R (n + 1))
(&) = isoVar2 (H.&) (\(H.split->(dxs,dy)) -> (dxs, fst (H.headTail dy)))
infixl 4 &
{-# INLINE (&) #-}

(#) :: (KnownNat n, KnownNat m, Reifies s W)
    => BVar s (H.R n)
    -> BVar s (H.R m)
    -> BVar s (H.R (n + m))
(#) = isoVar2 (H.#) H.split
infixl 4 #
{-# INLINE (#) #-}

split
    :: forall p n s. (KnownNat p, KnownNat n, p <= n, Reifies s W)
    => BVar s (H.R n)
    -> (BVar s (H.R p), BVar s (H.R (n - p)))
split v = (t ^^. _1, t ^^. _2)
  where
    t = isoVar H.split (uncurry (H.#)) v
    {-# NOINLINE t #-}
{-# INLINE split #-}

headTail
    :: (Reifies s W, KnownNat n, 1 <= n)
    => BVar s (H.R n)
    -> (BVar s H.ℝ, BVar s (H.R (n - 1)))
headTail v = (t ^^. _1, t ^^. _2)
  where
    t = isoVar H.headTail
                   (\(d, dx) -> (H.konst d :: H.R 1) H.# dx)
                   v
    {-# NOINLINE t #-}
{-# INLINE headTail #-}

-- | Potentially extremely bad for anything but short lists!!!
vector
    :: forall n s. Reifies s W
    => SV.Vector n (BVar s H.ℝ)
    -> BVar s (H.R n)
vector = BE.isoVar afSV
            (H.vecR . SVG.convert) (SVG.convert . H.rVec)
       . collectVar
{-# INLINE vector #-}

linspace
    :: forall n s. (KnownNat n, Reifies s W)
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
{-# INLINE linspace #-}

row :: Reifies s W
    => BVar s (H.R n)
    -> BVar s (H.L 1 n)
row = isoVar H.row H.unrow
{-# INLINE row #-}

col :: (KnownNat n, Reifies s W)
    => BVar s (H.R n)
    -> BVar s (H.L n 1)
col = isoVar H.col H.uncol
{-# INLINE col #-}

(|||) :: (KnownNat c, KnownNat r1, KnownNat (r1 + r2), Reifies s W)
      => BVar s (H.L c r1)
      -> BVar s (H.L c r2)
      -> BVar s (H.L c (r1 + r2))
(|||) = isoVar2 (H.|||) H.splitCols
infixl 3 |||
{-# INLINE (|||) #-}

(===) :: (KnownNat c, KnownNat r1, KnownNat (r1 + r2), Reifies s W)
      => BVar s (H.L r1        c)
      -> BVar s (H.L r2        c)
      -> BVar s (H.L (r1 + r2) c)
(===) = isoVar2 (H.===) H.splitRows
infixl 2 ===
{-# INLINE (===) #-}

splitRows
    :: forall p m n s. (KnownNat p, KnownNat m, KnownNat n, p <= m, Reifies s W)
    => BVar s (H.L m n)
    -> (BVar s (H.L p n), BVar s (H.L (m - p) n))
splitRows v = (t ^^. _1, t ^^. _2)
  where
    t = isoVar H.splitRows (uncurry (H.===)) v
    {-# NOINLINE t #-}
{-# INLINE splitRows #-}

splitCols
    :: forall p m n s. (KnownNat p, KnownNat m, KnownNat n, KnownNat (n - p), p <= n, Reifies s W)
    => BVar s (H.L m n)
    -> (BVar s (H.L m p), BVar s (H.L m (n - p)))
splitCols v = (t ^^. _1, t ^^. _2)
  where
    t = isoVar H.splitCols (uncurry (H.|||)) v
    {-# NOINLINE t #-}
{-# INLINE splitCols #-}

unrow
    :: (KnownNat n, Reifies s W)
    => BVar s (H.L 1 n)
    -> BVar s (H.R n)
unrow = isoVar H.unrow H.row
{-# INLINE unrow #-}

uncol
    :: (KnownNat n, Reifies s W)
    => BVar s (H.L n 1)
    -> BVar s (H.R n)
uncol = isoVar H.uncol H.col
{-# INLINE uncol #-}

tr  :: (HU.Transposable m mt, HU.Transposable mt m, Backprop m, Reifies s W)
    => BVar s m
    -> BVar s mt
tr = isoVar H.tr H.tr
{-# INLINE tr #-}

diag
    :: (KnownNat n, Reifies s W)
    => BVar s (H.R n)
    -> BVar s (H.Sq n)
diag = liftOp1 . op1 $ \x -> (H.diag x, H.takeDiag)
{-# INLINE diag #-}

-- | Potentially extremely bad for anything but short lists!!!
matrix
    :: forall m n s. (KnownNat m, KnownNat n, Reifies s W)
    => [BVar s H.ℝ]
    -> BVar s (H.L m n)
matrix = maybe (error "matrix: invalid number of elements")
               ( isoVar (H.vecL . SVG.convert . runABP) (ABP . SVG.convert . H.lVec)
               . collectVar
               . ABP
               )
       . SV.fromList @(m * n)
{-# INLINE matrix #-}

-- | Matrix product
(<>)
    :: (KnownNat m, KnownNat k, KnownNat n, Reifies s W)
    => BVar s (H.L m k)
    -> BVar s (H.L k n)
    -> BVar s (H.L m n)
(<>) = mul
infixr 8 <>
{-# INLINE (<>) #-}

-- | Matrix-vector product
(#>)
    :: (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (H.L m n)
    -> BVar s (H.R n)
    -> BVar s (H.R m)
(#>) = app
infixr 8 #>
{-# INLINE (#>) #-}

-- | Dot product
(<.>)
    :: (KnownNat n, Reifies s W)
    => BVar s (H.R n)
    -> BVar s (H.R n)
    -> BVar s H.ℝ
(<.>) = dot
infixr 8 <.>
{-# INLINE (<.>) #-}

-- | Can only get the singular values, for now.  Let me know if you find an
-- algorithm that can compute the gradients based on differentials for the
-- other matricies!
--
svd :: forall m n s. (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (H.L m n)
    -> BVar s (H.R n)
svd = liftOp1 . op1 $ \x ->
    let (u, σ, v) = H.svd x
    in  ( σ
        , \(dΣ :: H.R n) -> (u H.<> H.diagR 0 dΣ) H.<> H.tr v
                -- must manually associate because of bug in diagR in
                -- hmatrix-0.18.2.0
        )
{-# INLINE svd #-}

-- | Version of 'svd' that returns the full SVD, but if you attempt to find
-- the gradient, it will fail at runtime if you ever use U or V.
svd_
    :: forall m n s. (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (H.L m n)
    -> (BVar s (H.L m m), BVar s (H.R n), BVar s (H.L n n))
svd_ r = (t ^^. _1, t ^^. _2, t ^^. _3)
  where
    o :: Op '[H.L m n] (H.L m m, H.R n, H.L n n)
    o = op1 $ \x ->
        let msv@(u, _, v) = H.svd x
        in  ( msv
            , \(dU, dΣ, dV) ->
                    if H.norm_0 dU == 0 && H.norm_0 dV == 0
                      then (u H.<> H.diagR 0 dΣ) H.<> H.tr v
                      else error "svd_: Cannot backprop if U and V are used."
            )
    {-# INLINE o #-}
    t = liftOp1 o r
    {-# NOINLINE t #-}
{-# INLINE svd_ #-}

helpEigen :: KnownNat n => H.Sym n -> (H.R n, H.L n n, H.L n n, H.L n n)
helpEigen x = (l, v, H.inv v, H.tr v)
  where
    (l, v) = H.eigensystem x
{-# INLINE helpEigen #-}

-- | /NOTE/ The gradient is not necessarily symmetric!  The gradient is not
-- meant to be retireved directly; insteadl, 'eigenvalues' is meant to be
-- used as a part of a larger computation, and the gradient as an
-- intermediate step.
eigensystem
    :: forall n s. (KnownNat n, Reifies s W)
    => BVar s (H.Sym n)
    -> (BVar s (H.R n), BVar s (H.L n n))
eigensystem u = (t ^^. _1, t ^^. _2)
  where
    o :: Op '[H.Sym n] (H.R n, H.L n n)
    o = op1 $ \x ->
        let (l, v, vInv, vTr) = helpEigen x
            lRep = H.rowsL . SV.replicate $ l
            fMat = (1 - H.eye) * (lRep - H.tr lRep)
        in  ( (l, v)
            , \(dL, dV) -> unsafeCoerce $
                       H.tr vInv
                  H.<> (H.diag dL + fMat * (vTr H.<> dV))
                  H.<> vTr
            )
    {-# INLINE o #-}
    t = liftOp1 o u
    {-# NOINLINE t #-}
{-# INLINE eigensystem #-}

-- | /NOTE/ The gradient is not necessarily symmetric!  The gradient is not
-- meant to be retireved directly; insteadl, 'eigenvalues' is meant to be
-- used as a part of a larger computation, and the gradient as an
-- intermediate step.
eigenvalues
    :: forall n s. (KnownNat n, Reifies s W)
    => BVar s (H.Sym n)
    -> BVar s (H.R n)
eigenvalues = liftOp1 . op1 $ \x ->
    let (l, _, vInv, vTr) = helpEigen x
    in  ( l
        , \dL -> unsafeCoerce $
                 H.tr vInv H.<> H.diag dL H.<> vTr
        )
{-# INLINE eigenvalues #-}

-- | Algorithm from https://arxiv.org/abs/1602.07527
--
-- The paper also suggests a potential imperative algorithm that might
-- help.  Need to benchmark to see what is best.
--
-- /NOTE/ The gradient is not necessarily symmetric!  The gradient is not
-- meant to be retireved directly; insteadl, 'eigenvalues' is meant to be
-- used as a part of a larger computation, and the gradient as an
-- intermediate step.
chol
    :: forall n s. (KnownNat n, Reifies s W)
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
    in  ( l
        , \dL -> let s = H.tr lInv H.<> (phi * (H.tr l H.<> dL)) H.<> lInv
                 in  unsafeCoerce $ s + H.tr s - H.eye * s
        )
{-# INLINE chol #-}

-- | Number of non-zero items
norm_0
    :: (H.Normed a, Backprop a, Reifies s W)
    => BVar s a
    -> BVar s H.ℝ
norm_0 = liftOp1 . op1 $ \x -> (H.norm_0 x, const (zero x))
{-# INLINE norm_0 #-}

-- | Sum of absolute values
norm_1V
    :: (KnownNat n, Reifies s W)
    => BVar s (H.R n)
    -> BVar s H.ℝ
norm_1V = liftOp1 . op1 $ \x -> (H.norm_1 x, (* signum x) . H.konst)
{-# INLINE norm_1V #-}

-- | Maximum 'H.norm_1' of columns
norm_1M
    :: (KnownNat n, KnownNat m, Reifies s W)
    => BVar s (H.L n m)
    -> BVar s H.ℝ
norm_1M = liftOp1 . op1 $ \x ->
    let n = H.norm_1 x
    in  (n, \d -> let d' = H.konst d
                  in  H.colsL
                    . SV.map (\c -> if H.norm_1 c == n
                                      then d' * signum c
                                      else 0
                             )
                    . H.lCols
                    $ x
        )
{-# INLINE norm_1M #-}

-- | Square root of sum of squares
--
-- Be aware that gradient diverges when the norm is zero
norm_2V
    :: (KnownNat n, Reifies s W)
    => BVar s (H.R n)
    -> BVar s H.ℝ
norm_2V = liftOp1 . op1 $ \x ->
    let n = H.norm_2 x
    in (n, \d -> x * H.konst (d / n))
{-# INLINE norm_2V #-}

-- | Maximum singular value
norm_2M
    :: (KnownNat n, KnownNat m, Reifies s W)
    => BVar s (H.L n m)
    -> BVar s H.ℝ
norm_2M = liftOp1 . op1 $ \x ->
    let n = H.norm_2 x
        (head.H.toColumns->u1,_,head.H.toColumns->v1) = H.svd x
    in (n, \d -> H.konst d * (u1 `H.outer` v1))
{-# INLINE norm_2M #-}

-- | Maximum absolute value
norm_InfV
    :: (KnownNat n, Reifies s W)
    => BVar s (H.R n)
    -> BVar s H.ℝ
norm_InfV = liftOp1 . op1 $ \x ->
    let n :: H.ℝ
        n = H.norm_Inf x
    in  (n, \d -> H.vecR
                . SVS.map (\e -> if abs e == n
                                   then signum e * d
                                   else 0
                          )
                . H.rVec
                $ x
        )
{-# ANN norm_InfV "HLint: ignore Use camelCase" #-}
{-# INLINE norm_InfV #-}

-- | Maximum 'H.norm_1' of rows
norm_InfM
    :: (KnownNat n, KnownNat m, Reifies s W)
    => BVar s (H.L n m)
    -> BVar s H.ℝ
norm_InfM = liftOp1 . op1 $ \x ->
    let n = H.norm_Inf x
    in  (n, \d -> let d' = H.konst d
                  in  H.rowsL
                    . SV.map (\c -> if H.norm_1 c == n
                                      then d' * signum c
                                      else 0
                             )
                    . H.lRows
                    $ x
        )
{-# ANN norm_InfM "HLint: ignore Use camelCase" #-}
{-# INLINE norm_InfM #-}

mean
    :: (KnownNat n, 1 <= n, Reifies s W)
    => BVar s (H.R n)
    -> BVar s H.ℝ
mean = liftOp1 . op1 $ \x -> (H.mean x, H.konst . (/ H.norm_0 x))
{-# INLINE mean #-}

gradCov
    :: forall m n. (KnownNat m, KnownNat n)
    => H.L m n
    -> H.R n
    -> H.Sym n
    -> H.L m n
gradCov x μ dσ = H.rowsL
               . SV.map (subtract (dDiffsSum / m))
               . H.lRows
               $ dDiffs
  where
    diffs = H.rowsL . SV.map (subtract μ) . H.lRows $ x
    dDiffs = H.konst (2/n) * (diffs H.<> H.tr (H.unSym dσ))
    dDiffsSum = sum . H.toRows $ dDiffs
    m = fromIntegral $ natVal (Proxy @m)
    n = fromIntegral $ natVal (Proxy @n)
{-# INLINE gradCov #-}

-- | Mean and covariance.  If you know you only want to use one or the
-- other, use 'meanL' or 'cov'.
meanCov
    :: forall m n s. (KnownNat n, KnownNat m, 1 <= m, Reifies s W)
    => BVar s (H.L m n)
    -> (BVar s (H.R n), BVar s (H.Sym n))
meanCov v = (t ^^. _1, t ^^. _2)
  where
    m = fromInteger $ natVal (Proxy @m)
    t = ($ v) . liftOp1 . op1 $ \x ->
        let ms@(μ, _) = H.meanCov x
        in  ( ms
            , \(dμ, dσ) ->
                let gradMean = H.rowsL
                             . SV.replicate
                             $ (dμ / H.konst m)
                in  gradMean + gradCov x μ dσ
            )
    {-# NOINLINE t #-}
{-# INLINE meanCov #-}

-- | 'meanCov', but if you know you won't use the covariance.
meanL
    :: forall m n s. (KnownNat n, KnownNat m, 1 <= m, Reifies s W)
    => BVar s (H.L m n)
    -> BVar s (H.R n)
meanL = liftOp1 . op1 $ \x ->
    ( fst (H.meanCov x)
    , H.rowsL . SV.replicate . (/ H.konst m)
    )
  where
    m = fromInteger $ natVal (Proxy @m)
{-# INLINE meanL #-}

-- | 'cov', but if you know you won't use the covariance.
cov
    :: forall m n s. (KnownNat n, KnownNat m, 1 <= m, Reifies s W)
    => BVar s (H.L m n)
    -> BVar s (H.Sym n)
cov = liftOp1 . op1 $ \x ->
    let (μ, σ) = H.meanCov x
    in  (σ, gradCov x μ)
{-# INLINE cov #-}

mul :: ( KnownNat m
       , KnownNat k
       , KnownNat n
       , H.Domain field vec mat
       , Backprop (mat m k)
       , Backprop (mat k n)
       , HU.Transposable (mat m k) (mat k m)
       , HU.Transposable (mat k n) (mat n k)
       , Reifies s W
       )
    => BVar s (mat m k)
    -> BVar s (mat k n)
    -> BVar s (mat m n)
mul = liftOp2 . op2 $ \x y ->
    ( x `H.mul` y
    , \d -> (d `H.mul` H.tr y, H.tr x `H.mul` d)
    )
{-# INLINE mul #-}

app :: ( KnownNat m
       , KnownNat n
       , H.Domain field vec mat
       , HU.Transposable (mat m n) (mat n m)
       , Backprop (mat m n)
       , Backprop (vec n)
       , Reifies s W
       )
    => BVar s (mat m n)
    -> BVar s (vec n)
    -> BVar s (vec m)
app = liftOp2 . op2 $ \xs y ->
    ( xs `H.app` y
    , \d -> (d `H.outer` y, H.tr xs `H.app` d)
    )
{-# INLINE app #-}

dot :: ( KnownNat n
       , H.Domain field vec mat
       , H.Sized field (vec n) d
       , Num (vec n)
       , Backprop (vec n)
       , Reifies s W
       )
    => BVar s (vec n)
    -> BVar s (vec n)
    -> BVar s field
dot = liftOp2 . op2 $ \x y ->
    ( x `H.dot` y
    , \d -> let d' = H.konst d
            in  (d' * y, x * d')
    )
{-# INLINE dot #-}

cross
    :: ( H.Domain field vec mat
       , Reifies s W
       , Backprop (vec 3)
       )
    => BVar s (vec 3)
    -> BVar s (vec 3)
    -> BVar s (vec 3)
cross = liftOp2 . op2 $ \x y ->
    ( x `H.cross` y
    , \d -> (y `H.cross` d, d `H.cross` x)
    )
{-# INLINE cross #-}

-- | Create matrix with diagonal, and fill with default entries
diagR
    :: forall m n k field vec mat s.
       ( H.Domain field vec mat
       , Num (vec k)
       , Num (mat m n)
       , KnownNat m
       , KnownNat n
       , KnownNat k
       , HU.Container HU.Vector field
       , H.Sized field (mat m n) HU.Matrix
       , H.Sized field (vec k) HU.Vector
       , Backprop field
       , Backprop (vec k)
       , Reifies s W
       )
    => BVar s field             -- ^ default value
    -> BVar s (vec k)           -- ^ diagonal
    -> BVar s (mat m n)
diagR = liftOp2 . op2 $ \c x ->
    ( H.diagR c x
    , \d -> ( HU.sumElements . H.extract $ H.diagR 1 (0 :: vec k) * d
            , fromJust . H.create . HU.takeDiag . H.extract $ d
            )
    )
{-# INLINE diagR #-}

-- | Note: if possible, use the potentially much more performant 'vmap''.
vmap
    :: (KnownNat n, Reifies s W)
    => (BVar s H.ℝ -> BVar s H.ℝ)
    -> BVar s (H.R n)
    -> BVar s (H.R n)
vmap f = isoVar (H.vecR . SVG.convert @V.Vector . runABP)
                (ABP . SVG.convert . H.rVec)
       . B.fmap f
       . isoVar (ABP . SVG.convert . H.rVec) (H.vecR . SVG.convert . runABP)
{-# INLINE vmap #-}

-- | 'vmap', but potentially more performant.  Only usable if the mapped
-- function does not depend on any external 'BVar's.
vmap'
    :: ( Num (vec n)
       , Storable field
       , H.Sized field (vec n) HU.Vector
       , Backprop (vec n)
       , Backprop field
       , Reifies s W
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
vmap' f = liftOp1 . op1 $ bimap (fromJust . H.create . VG.convert)
                                ((*) . fromJust . H.create . VG.convert)
                        . V.unzip
                        . V.map (backprop f)
                        . VG.convert
                        . H.extract
{-# INLINE vmap' #-}

-- TODO: Can be made more efficient if backprop exports
-- a custom-total-derivative version

-- | Note: Potentially less performant than 'vmap''.
dvmap
    :: ( KnownNat n
       , H.Domain field vec mat
       , Num (vec n)
       , Backprop (vec n)
       , Backprop field
       , Reifies s W
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
dvmap f = liftOp1 . op1 $ \x ->
    ( H.dvmap (evalBP f) x
    , (H.dvmap (gradBP f) x *)
    )
{-# INLINE dvmap #-}

-- | Note: if possible, use the potentially much more performant 'mmap''.
mmap
    :: (KnownNat n, KnownNat m, Reifies s W)
    => (BVar s H.ℝ -> BVar s H.ℝ)
    -> BVar s (H.L n m)
    -> BVar s (H.L n m)
mmap f = isoVar (H.vecL . SVG.convert @V.Vector . runABP)
                (ABP . SVG.convert . H.lVec)
       . B.fmap f
       . isoVar (ABP . SVG.convert . H.lVec) (H.vecL . SVG.convert . runABP)
{-# INLINE mmap #-}

-- | 'mmap', but potentially more performant.  Only usable if the mapped
-- function does not depend on any external 'BVar's.
mmap'
    :: forall n m mat field s.
       ( KnownNat m
       , Num (mat n m)
       , Backprop (mat n m)
       , Backprop field
       , H.Sized field (mat n m) HU.Matrix
       , HU.Element field
       , Reifies s W
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (mat n m)
    -> BVar s (mat n m)
mmap' f = liftOp1 . op1 $ bimap (fromJust . H.create . HU.reshape m . VG.convert)
                                ((*) . fromJust . H.create . HU.reshape m . VG.convert)
                        . V.unzip
                        . V.map (backprop f)
                        . VG.convert
                        . HU.flatten
                        . H.extract
  where
    m :: Int
    m = fromInteger $ natVal (Proxy @m)
{-# INLINE mmap' #-}

-- | Note: Potentially less performant than 'mmap''.
dmmap
    :: ( KnownNat n
       , KnownNat m
       , H.Domain field vec mat
       , Num (mat n m)
       , Backprop (mat n m)
       , Backprop field
       , Reifies s W
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field)
    -> BVar s (mat n m)
    -> BVar s (mat n m)
dmmap f = liftOp1 . op1 $ \x ->
    ( H.dmmap (evalBP f) x
    , (H.dmmap (gradBP f) x *)
    )
{-# INLINE dmmap #-}

outer
    :: ( KnownNat m
       , KnownNat n
       , H.Domain field vec mat
       , HU.Transposable (mat n m) (mat m n)
       , Backprop (vec n)
       , Backprop (vec m)
       , Reifies s W
       )
    => BVar s (vec n)
    -> BVar s (vec m)
    -> BVar s (mat n m)
outer = liftOp2 . op2 $ \x y ->
    ( x `H.outer` y
    , \d -> ( d `H.app` y
            , H.tr d `H.app` x)
    )
{-# INLINE outer #-}

-- | Note: if possible, use the potentially much more performant
-- 'zipWithVector''.
zipWithVector
    :: (KnownNat n, Reifies s W)
    => (BVar s H.ℝ -> BVar s H.ℝ -> BVar s H.ℝ)
    -> BVar s (H.R n)
    -> BVar s (H.R n)
    -> BVar s (H.R n)
zipWithVector f x y = isoVar (H.vecR . SVG.convert . runABP)
                             (ABP . SVG.convert . H.rVec)
                    $ B.liftA2 @(ABP (SV.Vector _)) f (iv x) (iv y)
  where
    iv = isoVar (ABP . SVG.convert . H.rVec) (H.vecR . SVG.convert . runABP)
{-# INLINE zipWithVector #-}

zipWithVector'
    :: ( Num (vec n)
       , Backprop (vec n)
       , Storable field
       , Backprop field
       , H.Sized field (vec n) HU.Vector
       , Reifies s W
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
    -> BVar s (vec n)
zipWithVector' f = liftOp2 . op2 $ \(VG.convert.H.extract->x) (VG.convert.H.extract->y) ->
    let (z, dx, dy) = V.unzip3 $ V.zipWith (\x' -> retup . backprop2 f x') x y
    in  ( fromJust (H.create (VG.convert z))
        , \d -> ( d * fromJust (H.create (VG.convert dx))
                , d * fromJust (H.create (VG.convert dy))
                )
        )
  where
    retup (x, (y, z)) = (x, y, z)
{-# INLINE zipWithVector' #-}

-- | A version of 'zipWithVector'' that is potentially less performant but
-- is based on 'H.zipWithVector' from 'H.Domain'.
dzipWithVector
    :: ( KnownNat n
       , H.Domain field vec mat
       , Num (vec n)
       , Backprop (vec n)
       , Backprop field
       , Reifies s W
       )
    => (forall s'. Reifies s' W => BVar s' field -> BVar s' field -> BVar s' field)
    -> BVar s (vec n)
    -> BVar s (vec n)
    -> BVar s (vec n)
dzipWithVector f = liftOp2 . op2 $ \x y ->
    ( H.zipWithVector (evalBP2 f) x y
    , \d -> let dx = H.zipWithVector (\x' -> fst . gradBP2 f x') x y
                dy = H.zipWithVector (\x' -> snd . gradBP2 f x') x y
            in  (d * dx, d * dy)
    )
{-# INLINE dzipWithVector #-}

det :: ( KnownNat n
       , Num (mat n n)
       , Backprop (mat n n)
       , H.Domain field vec mat
       , H.Sized field (mat n n) d
       , HU.Transposable (mat n n) (mat n n)
       , Reifies s W
       )
    => BVar s (mat n n)
    -> BVar s field
det = liftOp1 . op1 $ \x ->
    let xDet = H.det x
        xInv = H.inv x
    in  ( xDet, \d -> H.konst (d * xDet) * H.tr xInv )
{-# INLINE det #-}

-- | The inverse and the natural log of the determinant together.  If you
-- know you don't need the inverse, it is best to use 'lndet'.
invlndet
    :: forall n mat field vec d s.
       ( KnownNat n
       , Num (mat n n)
       , H.Domain field vec mat
       , H.Sized field (mat n n) d
       , HU.Transposable (mat n n) (mat n n)
       , Backprop field
       , Backprop (mat n n)
       , Reifies s W
       )
    => BVar s (mat n n)
    -> (BVar s (mat n n), (BVar s field, BVar s field))
invlndet v = (t ^^. _1, (t ^^. _2, t ^^. _3))
  where
    o :: Op '[mat n n] (mat n n, field, field)
    o = op1 $ \x ->
      let (i,(ldet, s)) = H.invlndet x
          iTr           = H.tr i
      in  ( (i, ldet, s)
          , \(dI, dLDet, _) ->
                let gradI    = - iTr `H.mul` dI `H.mul` iTr
                    gradLDet = H.konst dLDet * H.tr i
                in  gradI + gradLDet
          )
    {-# INLINE o #-}
    t = liftOp1 o v
    {-# NOINLINE t #-}
{-# INLINE invlndet #-}

-- | The natural log of the determinant.
lndet
    :: forall n mat field vec d s.
       ( KnownNat n
       , Num (mat n n)
       , Backprop (mat n n)
       , H.Domain field vec mat
       , H.Sized field (mat n n) d
       , HU.Transposable (mat n n) (mat n n)
       , Reifies s W
       )
    => BVar s (mat n n)
    -> BVar s field
lndet = liftOp1 . op1 $ \x ->
          let (i,(ldet,_)) = H.invlndet x
          in  (ldet, (* H.tr i) . H.konst)
{-# INLINE lndet #-}

inv :: ( KnownNat n
       , Num (mat n n)
       , Backprop (mat n n)
       , H.Domain field vec mat
       , HU.Transposable (mat n n) (mat n n)
       , Reifies s W
       )
    => BVar s (mat n n)
    -> BVar s (mat n n)
inv = liftOp1 . op1 $ \x ->
    let xInv   = H.inv x
        xInvTr = H.tr xInv
    in  ( xInv, \d -> - xInvTr `H.mul` d `H.mul` xInvTr )
{-# INLINE inv #-}

toRows
    :: forall m n s. (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (H.L m n)
    -> SV.Vector m (BVar s (H.R n))
toRows = runABP . sequenceVar . isoVar (coerce H.lRows) (coerce H.rowsL)
{-# INLINE toRows #-}

toColumns
    :: forall m n s. (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (H.L m n)
    -> SV.Vector n (BVar s (H.R m))
toColumns = runABP . sequenceVar . isoVar (coerce H.lCols) (coerce H.colsL)
{-# INLINE toColumns #-}

fromRows
    :: forall m n s. (KnownNat m, Reifies s W)
    => SV.Vector m (BVar s (H.R n))
    -> BVar s (H.L m n)
fromRows = isoVar (coerce H.rowsL) (coerce H.lRows) . collectVar . ABP
{-# INLINE fromRows #-}

fromColumns
    :: forall m n s. (KnownNat n, Reifies s W)
    => SV.Vector n (BVar s (H.R m))
    -> BVar s (H.L m n)
fromColumns = isoVar (coerce H.colsL) (coerce H.lCols) . collectVar . ABP
{-# INLINE fromColumns #-}

konst
    :: forall t s d q.
     ( H.Sized t s d
     , HU.Container d t
     , Backprop t
     , Reifies q W
     )
    => BVar q t
    -> BVar q s
konst = liftOp1 . op1 $ \x ->
    ( H.konst x
    , HU.sumElements . H.extract
    )
{-# INLINE konst #-}

sumElements
    :: forall t s d q.
     ( H.Sized t s d
     , HU.Container d t
     , Backprop s
     , Reifies q W
     )
    => BVar q s
    -> BVar q t
sumElements = liftOp1 . op1 $ \x ->
    ( HU.sumElements . H.extract $ x
    , H.konst
    )
{-# INLINE sumElements #-}

-- | If there are extra items in the total derivative, they are dropped.
-- If there are missing items, they are treated as zero.
extractV
    :: forall t s q.
       ( H.Sized t s HU.Vector
       , HU.Konst t Int HU.Vector
       , HU.Container HU.Vector t
       , Backprop s
       , Reifies q W
       )
    => BVar q s
    -> BVar q (HU.Vector t)
extractV = liftOp1 . op1 $ \x ->
    let n = H.size x
    in  ( H.extract x
        , \d -> let m  = HU.size d
                    m' = case compare n m of
                            LT -> HU.subVector 0 n d
                            EQ -> d
                            GT -> HU.vjoin [d, HU.konst 0 (n - m)]
                in  fromJust . H.create $ m'
        )
{-# INLINE extractV #-}

-- | If there are extra items in the total derivative, they are dropped.
-- If there are missing items, they are treated as zero.
extractM
    :: forall t s q.
       ( H.Sized t s HU.Matrix
       , Backprop s
       , HU.Konst t (Int, Int) HU.Matrix
       , HU.Container HU.Matrix t
       , Num (HU.Matrix t)
       , Reifies q W
       )
    => BVar q s
    -> BVar q (HU.Matrix t)
extractM = liftOp1 . op1 $ \x ->
    let (xI,xJ) = H.size x
    in  ( H.extract x
        , \d -> let (dI,dJ) = HU.size d
                    m' = case (compare xI dI, compare xJ dJ) of
                           (LT, LT) -> d HU.?? (HU.Take xI, HU.Take xJ)
                           (LT, EQ) -> d HU.?? (HU.Take xI, HU.All)
                           (LT, GT) -> d HU.?? (HU.Take xI, HU.All)
                                HU.||| HU.konst 0 (xI, xJ - dJ)
                           (EQ, LT) -> d HU.?? (HU.All    , HU.Take xJ)
                           (EQ, EQ) -> d
                           (EQ, GT) -> d HU.?? (HU.All, HU.All)
                                HU.||| HU.konst 0 (xI, xJ - dJ)
                           (GT, LT) -> d HU.?? (HU.All, HU.Take xJ)
                                HU.=== HU.konst 0 (xI - dI, xJ)
                           (GT, EQ) -> d HU.?? (HU.All, HU.All)
                                HU.=== HU.konst 0 (xI - dI, xJ)
                           (GT, GT) -> HU.fromBlocks
                              [[d,0                            ]
                              ,[0,HU.konst 0 (xI - dI, xJ - dJ)]
                              ]
                in  fromJust . H.create $ m'
        )
{-# INLINE extractM #-}

create
    :: (H.Sized t s d, Backprop s, Num (d t), Backprop (d t), Reifies q W)
    => BVar q (d t)
    -> Maybe (BVar q s)
create = sequenceVar . isoVar H.create (maybe 0 H.extract)
{-# INLINE create #-}


takeDiag
    :: ( KnownNat n
       , H.Diag (mat n n) (vec n)
       , H.Domain field vec mat
       , Num field
       , Backprop (mat n n)
       , Reifies s W
       )
    => BVar s (mat n n)
    -> BVar s (vec n)
takeDiag = liftOp1 . op1 $ \x ->
    ( H.takeDiag x
    , H.diagR 0
    )
{-# INLINE takeDiag #-}

-- |
-- \[
-- \frac{1}{2} (M + M^T)
-- \]
sym :: (KnownNat n, Reifies s W)
    => BVar s (H.Sq n)
    -> BVar s (H.Sym n)
sym = liftOp1 . op1 $ \x ->
    ( H.sym x
    , H.unSym . H.sym . H.unSym
    )
{-# INLINE sym #-}

-- |
-- \[
-- M^T M
-- \]
mTm :: (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (H.L m n)
    -> BVar s (H.Sym n)
mTm = liftOp1 . op1 $ \x ->
    ( H.mTm x
    , \d -> 2 * (x H.<> H.unSym d)
    )
{-# INLINE mTm #-}

-- | Warning: the gradient is going necessarily symmetric, and so is /not/
-- meant to be used directly.  Rather, it is meant to be used in the middle
-- (or at the end) of a longer computation.
unSym
    :: (KnownNat n, Reifies s W)
    => BVar s (H.Sym n)
    -> BVar s (H.Sq n)
unSym = isoVar H.unSym unsafeCoerce
{-# INLINE unSym #-}

-- | Unicode synonym for '<.>>'
(<·>)
    :: (KnownNat n, Reifies s W)
    => BVar s (H.R n)
    -> BVar s (H.R n)
    -> BVar s H.ℝ
(<·>) = dot
infixr 8 <·>
{-# INLINE (<·>) #-}

afSV :: Backprop a => BE.AddFunc (SV.Vector n a)
afSV = BE.AF (SV.zipWith add)
{-# INLINE afSV #-}
