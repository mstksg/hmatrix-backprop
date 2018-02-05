{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Numeric.LinearAlgebra.Static.Backprop (
    H.R
  , H.ℝ
  , vec2
  , vec3
  ) where

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


