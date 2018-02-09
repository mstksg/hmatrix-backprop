{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}

module Nudge where

import           Control.Monad
import           Data.Bifunctor
import           Data.Kind
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeNats
import           Hedgehog
import           Lens.Micro
import           Lens.Micro.Platform                   ()
import           Numeric.Backprop
import           Numeric.Backprop.Tuple
import qualified Data.Ix                               as Ix
import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range
import qualified Numeric.LinearAlgebra                 as HU
import qualified Numeric.LinearAlgebra.Static          as H
import qualified Numeric.LinearAlgebra.Static.Backprop as B

nudge :: Double
nudge = 1e-6

eps :: Double
eps = 1e-8

class Testing c where
    type TIx c :: Type
    allIx  :: c -> [TIx c]
    ixLens :: TIx c -> Lens' c Double
    scalarize :: Reifies s W => BVar s c -> BVar s Double

ixContainer
    :: forall s t d. (H.Sized t s d, HU.Container d t)
    => HU.IndexOf d
    -> Lens' s t
ixContainer i f = fmap (fromJust . H.create) . go f . H.extract
  where
    go :: Lens' (d t) t
    go = lens (`HU.atIndex` i)
              (\xs x -> HU.accum xs (\_ _ -> x) [(i, 0)])

instance Testing Double where
    type TIx Double = ()
    allIx _ = [()]
    ixLens _ = id
    scalarize = abs

instance KnownNat n => Testing (H.R n) where
    type TIx (H.R n) = Int
    allIx v = [0 .. H.size v - 1]
    ixLens = ixContainer
    scalarize = B.norm_2V

instance (KnownNat n, KnownNat m) => Testing (H.L n m) where
    type TIx (H.L n m) = (Int, Int)
    allIx m = Ix.range ((0,0), bimap pred pred (H.size m))
    ixLens = ixContainer
    scalarize = sqrt . B.sumElements . (**2)

instance (Testing a, Testing b, Num a, Num b) => Testing (T2 a b) where
    type TIx (T2 a b) = Either (TIx a) (TIx b)
    allIx (T2 x y) = (Left  <$> allIx x)
                  ++ (Right <$> allIx y)
    ixLens (Left  i) = _1 . ixLens i
    ixLens (Right j) = _2 . ixLens j
    scalarize t = B.norm_2V (B.vec2 (scalarize (t ^^. _1))
                                    (scalarize (t ^^. _2))
                            )

instance (Testing a, Testing b, Testing c, Num a, Num b, Num c) => Testing (T3 a b c) where
    type TIx (T3 a b c) = Either (TIx a) (Either (TIx b) (TIx c))
    allIx (T3 x y z) = (Left          <$> allIx x)
                    ++ (Right . Left  <$> allIx y)
                    ++ (Right . Right <$> allIx z)
    ixLens (Left         i ) = _1 . ixLens i
    ixLens (Right (Left  j)) = _2 . ixLens j
    ixLens (Right (Right k)) = _3 . ixLens k
    scalarize t = B.norm_2V (B.vec3 (scalarize (t ^^. _1))
                                    (scalarize (t ^^. _2))
                                    (scalarize (t ^^. _3))
                            )

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
    :: (Show c, Num c, Testing c, Show (TIx c), Testing d)
    => Gen c
    -> (forall s. Reifies s W => BVar s c -> BVar s d)
    -> Property
nudgeProp gn f = property $ do
    inp <- forAll gn
    let (r,gr) = backprop (scalarize . f) inp
    when (r**2 < eps) discard
    i <- forAll $ Gen.element (allIx inp)
    (old, new) <- validGrad (ixLens i) inp gr (evalBP (scalarize . f))
    footnoteShow (r, gr, old, new, (old - new)**2, ((old - new)/old)**2)
    assert $ ((old - new)/old)**2 < eps

nudgeProp2
    :: (Show c, Num c, Testing c, Show (TIx c), Show d, Num d, Testing d, Show (TIx d), Testing e)
    => Gen c
    -> Gen d
    -> (forall s. Reifies s W => BVar s c -> BVar s d -> BVar s e)
    -> Property
nudgeProp2 gnc gnd f = property $ do
    inpC <- forAll gnc
    inpD <- forAll gnd
    let (r, gr) = second tupT2 $ backprop2 (\x -> scalarize . f x) inpC inpD
    when (r**2 < eps) discard
    i <- forAll $ Gen.element (allIx (T2 inpC inpD))
    (old, new) <- validGrad (ixLens i) (T2 inpC inpD) gr
          (evalBP (\t -> scalarize $ f (t ^^. _1) (t ^^. _2)))
    footnoteShow (r, gr, old, new, (old - new)**2, ((old - new)/old)**2)
    assert $ ((old - new)/old)**2 < eps

genDouble :: Gen Double
genDouble = Gen.filter ((> eps) . (**2)) $ Gen.double (Range.linearFracFrom 0 (-10) 10)

genVec :: forall n. KnownNat n => Gen (H.R n)
genVec = H.vector <$> replicateM n genDouble
  where
    n = fromIntegral $ natVal (Proxy @n)

genMat :: forall n m. (KnownNat n, KnownNat m) => Gen (H.L n m)
genMat = H.matrix <$> replicateM nm genDouble
  where
    nm = fromIntegral $ natVal (Proxy @n) * natVal (Proxy @m)

