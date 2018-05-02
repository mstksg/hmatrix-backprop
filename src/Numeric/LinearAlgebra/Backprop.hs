{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.LinearAlgebra.Backprop (
  ) where

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

