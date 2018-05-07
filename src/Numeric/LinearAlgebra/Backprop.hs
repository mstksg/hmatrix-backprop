{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: WARNING WARNING: NUM INSTANCE OF VECTOR DOES WRONG THING!!

module Numeric.LinearAlgebra.Backprop (
    dot
  , (<.>)
  , (#>)
  , (<#)
  , (<>)
  , outer
  , kronecker
  , cross
  , sumElements
  , flatten
  ) where

import           Data.Function
import           Numeric.Backprop
import           Numeric.Backprop.Class
import qualified Numeric.LinearAlgebra  as H

instance (H.Container H.Vector a, Num a) => Backprop (H.Matrix a) where
    zero = H.cmap (const 0)
    add  = H.add
    one  = H.cmap (const 1)

dot :: (H.Numeric t, Backprop t, Reifies s W)
    => BVar s (H.Vector t)
    -> BVar s (H.Vector t)
    -> BVar s t
dot = liftOp2 . op2 $ \x y ->
    ( x `H.dot` y
    , \d -> (H.scale d y, H.scale d x)
    )
{-# INLINE dot #-}

-- | Infix synonym for `dot`
(<.>)
    :: (H.Numeric t, Backprop t, Reifies s W)
    => BVar s (H.Vector t)
    -> BVar s (H.Vector t)
    -> BVar s t
(<.>) = dot
infixr 8 <.>
{-# INLINE (<.>) #-}

(#>)
    :: ( Reifies s W
       , H.Product t
       , H.Numeric t
       , Backprop t
       )
    => BVar s (H.Matrix t)
    -> BVar s (H.Vector t)
    -> BVar s (H.Vector t)
(#>) = liftOp2 . op2 $ \xs y ->
    ( xs H.#> y
    , \d -> (d `H.outer` y, d H.<# xs)
    )
infixr 8 #>
{-# INLINE (#>) #-}

(<#)
    :: ( Reifies s W
       , H.Product t
       , H.Numeric t
       , Backprop t
       )
    => BVar s (H.Vector t)
    -> BVar s (H.Matrix t)
    -> BVar s (H.Vector t)
(<#) = liftOp2 . op2 $ \y xs ->
    ( y H.<# xs
    , \d -> (xs H.#> d, y `H.outer` d)
    )
infixr 8 <#
{-# INLINE (<#) #-}

(<>)
    :: ( Reifies s W
       , H.Product t
       , H.Numeric t
       )
    => BVar s (H.Matrix t)
    -> BVar s (H.Matrix t)
    -> BVar s (H.Matrix t)
(<>) = liftOp2 . op2 $ \x y ->
    ( x H.<> y
    , \d -> (d H.<> H.tr y, H.tr x H.<> d)
    )
infixr 8 <>
{-# INLINE (<>) #-}

outer
    :: ( Reifies s W
       , H.Product t
       , H.Numeric t
       , Backprop t
       )
    => BVar s (H.Vector t)
    -> BVar s (H.Vector t)
    -> BVar s (H.Matrix t)
outer = liftOp2 . op2 $ \x y ->
    ( x `H.outer` y
    , \d -> ( d H.#> y
            , H.tr d H.#> x
            )
    )
{-# INLINE outer #-}

kronecker
    :: ( Reifies s W
       , H.Product t
       , H.Numeric t
       )
    => BVar s (H.Matrix t)
    -> BVar s (H.Matrix t)
    -> BVar s (H.Matrix t)
kronecker = liftOp2 . op2 $ \x y ->
    ( x `H.kronecker` y
    , \d -> let (xR, xC) = H.size x
                (yR, yC) = H.size y
                bsX = H.toBlocksEvery xR xC d
                bsY = H.toBlocksEvery yR yC d
            in  ( H.fromLists . (map . map) (matDot y) $ bsY
                , H.fromLists . (map . map) (matDot x) $ bsX
                )
    )
  where
    matDot = H.dot `on` H.flatten

flatten
    :: ( H.Element t
       , H.Container H.Vector t
       , Num t
       , Backprop t
       , Reifies s W
       )
    => BVar s (H.Matrix t)
    -> BVar s (H.Vector t)
flatten = liftOp1 . op1 $ \x ->
    ( H.flatten x
    , H.reshape (H.rows x)
    )

-- | Only defined for inputs that are three-element vectors
cross
    :: (H.Product t, Backprop t, Reifies s W)
    => BVar s (H.Vector t)
    -> BVar s (H.Vector t)
    -> BVar s (H.Vector t)
cross = liftOp2 . op2 $ \x y ->
    ( x `H.cross` y
    , \d -> (y `H.cross` d, d `H.cross` x)
    )
{-# INLINE cross #-}

sumElements
    :: ( Backprop e
       , Backprop (c e)
       , H.Container c e
       , H.Konst e (H.IndexOf c) c
       , Reifies s W
       )
    => BVar s (c e)
    -> BVar s e
sumElements = liftOp1 . op1 $ \xs ->
    ( H.sumElements xs
    , flip H.konst (H.size xs)
    )
