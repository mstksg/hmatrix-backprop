{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

import           Control.Monad
import           Data.Bifunctor
import           Data.Maybe
import           Hedgehog
import           Lens.Micro
import           Nudge
import           Numeric.Backprop
import           Numeric.Backprop.Tuple
import           Numeric.LinearAlgebra.Static          (L, R)
import           System.Exit
import           System.IO
import qualified Numeric.LinearAlgebra.Static.Backprop as B

prop_vec2 :: Property
prop_vec2 = nudgeProp2 B.vec2

prop_vec3 :: Property
prop_vec3 = nudgeProp @(T3 Double Double Double)
                (\t -> B.vec3 (t ^^. _1) (t ^^. _2) (t ^^. _3))

prop_vec4 :: Property
prop_vec4 = nudgeProp2 @(T2 Double Double) @(T2 Double Double)
                (\x y -> B.vec4 (x ^^. _1) (x ^^. _2) (y ^^. _1) (y ^^. _2))

prop_snoc :: Property
prop_snoc = nudgeProp2 @(R 3) (B.&)

prop_append :: Property
prop_append = nudgeProp2 @(R 3) @(R 2) (B.#)

prop_split1 :: Property
prop_split1 = nudgeProp @(R 3) (fst . B.split @2)

prop_split2 :: Property
prop_split2 = nudgeProp @(R 3) (snd . B.split @2)

prop_headTail1 :: Property
prop_headTail1 = nudgeProp @(R 3) (fst . B.headTail)

prop_headTail2 :: Property
prop_headTail2 = nudgeProp @(R 3) (snd . B.headTail)

prop_vector :: Property
prop_vector = nudgeProp (B.vector @3 . sequenceVar)

prop_linspace :: Property
prop_linspace = nudgeProp2 (B.linspace @3)

prop_row :: Property
prop_row = nudgeProp @(R 3) B.row

prop_col :: Property
prop_col = nudgeProp @(R 3) B.col

prop_horzcat :: Property
prop_horzcat = nudgeProp2 @(L 3 2) @(L 3 1) (B.|||)

prop_vertcat :: Property
prop_vertcat = nudgeProp2 @(L 2 3) @(L 1 3) (B.===)

prop_splitRows1 :: Property
prop_splitRows1 = nudgeProp @(L 2 3) (fst . B.splitRows @1)

prop_splitRows2 :: Property
prop_splitRows2 = nudgeProp @(L 2 3) (snd . B.splitRows @1)

prop_splitCols1 :: Property
prop_splitCols1 = nudgeProp @(L 3 2) (fst . B.splitCols @1)

prop_splitCols2 :: Property
prop_splitCols2 = nudgeProp @(L 3 2) (snd . B.splitCols @1)

prop_unrow :: Property
prop_unrow = nudgeProp @(L 1 3) B.unrow

prop_uncol :: Property
prop_uncol = nudgeProp @(L 3 1) B.uncol

prop_tr :: Property
prop_tr = nudgeProp @(L 3 2) B.tr

prop_diag :: Property
prop_diag = nudgeProp @(R 3) B.diag

prop_svd :: Property
prop_svd = nudgeProp @(L 3 2) B.svd

prop_svd_ :: Property
prop_svd_ = nudgeProp @(L 3 2) ((\(_,x,_) -> x) . B.svd_)

prop_eigensystem1 :: Property
prop_eigensystem1 = nudgeProp @(L 3 2) (fst . B.eigensystem . B.mTm)

prop_eigensystem2 :: Property
prop_eigensystem2 = nudgeProp @(L 3 2) (snd . B.eigensystem . B.mTm)

prop_eigenvalues :: Property
prop_eigenvalues = nudgeProp @(L 3 2) (B.eigenvalues . B.mTm)

prop_chol :: Property
prop_chol = nudgeProp @(L 3 2) (B.chol . B.mTm)

prop_norm_0V :: Property
prop_norm_0V = nudgeProp @(R 3) B.norm_0

prop_norm_0M :: Property
prop_norm_0M = nudgeProp @(L 3 2) B.norm_0

prop_norm_1V :: Property
prop_norm_1V = nudgeProp @(R 3) B.norm_1V

prop_norm_1M :: Property
prop_norm_1M = nudgeProp @(L 3 2) B.norm_1M

prop_norm_2V :: Property
prop_norm_2V = nudgeProp @(R 3) B.norm_2V

prop_norm_2M :: Property
prop_norm_2M = nudgeProp @(L 3 2) B.norm_2M

prop_norm_InfV :: Property
prop_norm_InfV = nudgeProp @(R 3) B.norm_InfV

prop_norm_InfM :: Property
prop_norm_InfM = nudgeProp @(L 3 2) B.norm_InfM

prop_mean :: Property
prop_mean = nudgeProp @(R 3) B.mean

prop_meanCov1 :: Property
prop_meanCov1 = nudgeProp @(L 3 2) (fst . B.meanCov)

prop_meanCov2 :: Property
prop_meanCov2 = nudgeProp @(L 3 2) (B.unSym . snd . B.meanCov)

prop_meanL :: Property
prop_meanL = nudgeProp @(L 3 2) B.meanL

prop_cov :: Property
prop_cov = nudgeProp @(L 3 2) (B.unSym . B.cov)

prop_mul :: Property
prop_mul = nudgeProp2 @(L 3 2) @(L 2 3) B.mul

prop_app :: Property
prop_app = nudgeProp2 @(L 3 2) @(R 2) B.app

prop_dot :: Property
prop_dot = nudgeProp2 @(R 3) @(R 3) B.dot

prop_cross :: Property
prop_cross = nudgeProp2 @(R 3) B.cross

-- TODO: bug in diagR?
-- prop_diagR :: Property
-- prop_diagR = nudgeProp2 genDouble (genVec @3) (B.diagR @5 @4)

-- TODO: Mappers
-- , dvmap
-- , dvmap'
-- , dmmap
-- , dmmap'

prop_outer :: Property
prop_outer = nudgeProp2 @(R 3) @(R 2) B.outer

-- TODO: Zippers
-- , zipWithVector
-- , zipWithVector'

prop_det :: Property
prop_det = nudgeProp @(L 3 3) B.det

prop_invlndet1 :: Property
prop_invlndet1 = nudgeProp @(L 3 3) (fst . B.invlndet)

prop_invlndet2 :: Property
prop_invlndet2 = nudgeProp @(L 3 3) (fst . snd . B.invlndet)

prop_invlndet3 :: Property
prop_invlndet3 = nudgeProp @(L 3 3) (snd . snd . B.invlndet)

prop_lndet :: Property
prop_lndet = nudgeProp @(L 3 3) B.lndet

-- TODO: more general invertible matrix
prop_inv :: Property
prop_inv = nudgeProp @(L 3 2) (B.inv . B.unSym . B.mTm)

prop_toRows :: Property
prop_toRows = nudgeProp @(L 3 2) (collectVar . B.toRows)

prop_toColumns :: Property
prop_toColumns = nudgeProp @(L 2 3) (collectVar . B.toColumns)

prop_fromRows :: Property
prop_fromRows = nudgeProp (B.fromRows @3 @2 . sequenceVar)

prop_fromColumns :: Property
prop_fromColumns = nudgeProp (B.fromColumns @2 @3 . sequenceVar)

prop_konstV :: Property
prop_konstV = nudgeProp (B.konst @_ @(B.R 3))

prop_konstM :: Property
prop_konstM = nudgeProp (B.konst @_ @(B.L 3 2))

prop_sumElementsV :: Property
prop_sumElementsV = nudgeProp @(R 3) B.sumElements

prop_sumElementsM :: Property
prop_sumElementsM = nudgeProp @(L 3 2) B.sumElements

prop_extractV :: Property
prop_extractV = nudgeProp (B.extractV @_ @(R 3))

prop_extractM :: Property
prop_extractM = nudgeProp (B.extractM @_ @(L 3 2))

prop_createV :: Property
prop_createV = nudgeProp (fromMaybe 0 . B.create @_ @(R 3))

prop_createM :: Property
prop_createM = nudgeProp (fromMaybe 0 . B.create @_ @(L 3 2))

prop_takeDiag :: Property
prop_takeDiag = nudgeProp @(L 3 3) B.takeDiag

prop_sym :: Property
prop_sym = nudgeProp @(L 3 3) (B.unSym . B.sym)

prop_mTm :: Property
prop_mTm = nudgeProp @(L 3 2) (B.unSym . B.mTm)

prop_unSym :: Property
prop_unSym = nudgeProp @(L 3 3) (B.unSym . B.sym)

tryGroup :: (forall a. Num a => a) -> Group -> Group
tryGroup n Group{..} =
    Group groupName
          ((map . second) (withDiscards n . withTests n)
                          groupProperties
          )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- checkParallel (tryGroup 100 $$(discover))

  unless results exitFailure

