{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Nudge where

import           Control.Monad
import           Data.Bifunctor
import           Data.Finite
import           Data.Kind
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           Hedgehog
import           Lens.Micro
import           Lens.Micro.Platform                   ()
import           Numeric.Backprop
import qualified Data.Ix                               as Ix
import qualified Data.Vector.Sized                     as SV
import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range
import qualified Numeric.LinearAlgebra                 as HU
import qualified Numeric.LinearAlgebra.Static          as H
import qualified Numeric.LinearAlgebra.Static.Backprop as B

nudge :: Double
nudge = 1e-6

eps :: Double
eps = 1e-11

class (Backprop c, Show c, Show (TIx c)) => Testing c where
    type TIx c :: Type
    allIx  :: c -> [TIx c]
    ixLens :: TIx c -> Lens' c Double
    scalarize :: Reifies s W => BVar s c -> BVar s Double
    genTest :: Gen c

sized
    :: forall s t d. H.Sized t s d
    => Lens' s (d t)
sized f = fmap (fromJust . H.create) . f . H.extract

ixContainer
    :: forall t d. HU.Container d t
    => HU.IndexOf d
    -> Lens' (d t) t
ixContainer i = lens (`HU.atIndex` i)
                     (\xs x -> HU.accum xs (\_ _ -> x) [(i, x)])

instance Testing Double where
    type TIx Double = ()
    allIx _ = [()]
    ixLens _ = id
    scalarize = abs
    genTest = Gen.filter ((> eps) . (**2)) $
         Gen.double (Range.linearFracFrom 0 (-5) 5)

instance KnownNat n => Testing (H.R n) where
    type TIx (H.R n) = Int
    allIx v = [0 .. H.size v - 1]
    ixLens i = sized . ixContainer i
    scalarize = B.norm_2V
    genTest = H.vector <$> replicateM n genTest
      where
        n = fromInteger $ natVal (Proxy @n)

instance (KnownNat n, KnownNat m) => Testing (H.L n m) where
    type TIx (H.L n m) = (Int, Int)
    allIx m = Ix.range ((0,0), bimap pred pred (H.size m))
    ixLens i = sized . ixContainer i
    scalarize = sqrt . B.sumElements . (**2)
    genTest = H.matrix <$> replicateM nm genTest
      where
        nm = fromInteger $ natVal (Proxy @n) * natVal (Proxy @m)

instance Testing (HU.Vector Double) where
    type TIx (HU.Vector Double) = Int
    allIx v = [0 .. HU.size v - 1]
    ixLens = ixContainer
    scalarize = liftOp1 . op1 $ \xs -> (HU.sumElements xs, (`HU.konst` HU.size xs))
    genTest = HU.fromList <$> replicateM 3 genTest

instance Testing (HU.Matrix Double) where
    type TIx (HU.Matrix Double) = (Int, Int)
    allIx m = Ix.range ((0,0), bimap pred pred (HU.size m))
    ixLens = ixContainer
    scalarize = liftOp1 . op1 $ \xs -> (HU.sumElements xs, (`HU.konst` HU.size xs))
    genTest = HU.fromLists <$> (replicateM 3 . replicateM 2) genTest

instance (KnownNat n, Testing a, Num a) => Testing (SV.Vector n a) where
    type TIx (SV.Vector n a) = (Finite n, TIx a)
    allIx = fst . SV.imapM (\i x -> ((fromIntegral i,) <$> allIx x , x))
    ixLens (i,j) = SV.ix i . ixLens j
    scalarize = scalarize . liftOp1 o . (^ (2 :: Int))
      where
        o :: Op '[SV.Vector n a] a
        o = op1 $ \xs -> (SV.sum xs, SV.replicate)
    genTest = SV.replicateM genTest

instance (Testing a, Testing b) => Testing (a, b) where
    type TIx (a, b) = Either (TIx a) (TIx b)
    allIx (x, y) = (Left  <$> allIx x)
                ++ (Right <$> allIx y)
    ixLens (Left  i) = _1 . ixLens i
    ixLens (Right j) = _2 . ixLens j
    scalarize t = B.norm_2V (B.vec2 (scalarize (t ^^. _1))
                                    (scalarize (t ^^. _2))
                            )
    genTest = (,) <$> genTest <*> genTest

instance (Testing a, Testing b, Testing c) => Testing (a, b, c) where
    type TIx (a, b, c) = Either (TIx a) (Either (TIx b) (TIx c))
    allIx (x, y, z) = (Left          <$> allIx x)
                   ++ (Right . Left  <$> allIx y)
                   ++ (Right . Right <$> allIx z)
    ixLens (Left         i ) = _1 . ixLens i
    ixLens (Right (Left  j)) = _2 . ixLens j
    ixLens (Right (Right k)) = _3 . ixLens k
    scalarize t = B.norm_2V (B.vec3 (scalarize (t ^^. _1))
                                    (scalarize (t ^^. _2))
                                    (scalarize (t ^^. _3))
                            )
    genTest = (,,) <$> genTest <*> genTest <*> genTest

validGrad
    :: Monad m
    => Lens' c Double
    -> c
    -> c
    -> (c -> Double)
    -> PropertyT m (Double, Double)
validGrad l x0 g f = forAll $ Gen.double (Range.constantFrom 0 (-nudge) nudge) <&> \d ->
    let x   = x0 & l %~ (+d)
        old = f x0 + (g ^. l) * d
        new = f x
    in  (old, new)

nudgeProp
    :: forall c d. (Testing c, Testing d)
    => (forall s. Reifies s W => BVar s c -> BVar s d)
    -> Property
nudgeProp f = property $ do
    (inp, i) <- forAll $ do
      inp <- genTest
      i   <- Gen.element (allIx inp)
      return (inp, i)
    let (r,gr) = backprop (scalarize . f) inp
    when (r**2 < eps) discard
    (old, new) <- validGrad (ixLens i) inp gr (evalBP (scalarize . f))
    footnoteShow (r, gr, old, new, (old - new)**2, ((old - new)/old)**2)
    assert $ ((old - new)/old)**2 < eps

nudgeProp2
    :: forall c d e. (Testing c, Testing d, Testing e)
    => (forall s. Reifies s W => BVar s c -> BVar s d -> BVar s e)
    -> Property
nudgeProp2 f = property $ do
    (inpC, inpD, i) <- forAll $ do
      inpC <- genTest
      inpD <- genTest
      i    <- Gen.element (allIx (inpC, inpD))
      return (inpC, inpD, i)
    let (r, gr) = backprop2 (\x -> scalarize . f x) inpC inpD
    when (r**2 < eps) discard
    (old, new) <- validGrad (ixLens i) (inpC, inpD) gr
          (evalBP (\t -> scalarize $ f (t ^^. _1) (t ^^. _2)))
    footnoteShow (r, gr, old, new, (old - new)**2, ((old - new)/old)**2)
    assert $ ((old - new)/old)**2 < eps

instance (HU.Container HU.Vector a, Num a) => Backprop (HU.Matrix a) where
    zero = HU.cmap (const 0)
    add  = HU.add
    one  = HU.cmap (const 1)

instance (KnownNat n, Num a) => Backprop (SV.Vector n a) where
    zero = (0 <$)
    add  = (+)
    one  = (1 <$)
