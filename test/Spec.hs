{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- import           Data.Bifunctor
-- import           Data.Kind
-- import           Data.Maybe
-- import           GHC.TypeNats
-- import           Lens.Micro.Platform                ()
-- import qualified Data.Ix                            as Ix
-- import qualified Hedgehog.Gen                       as Gen
-- import qualified Hedgehog.Range                     as Range
-- import qualified Numeric.LinearAlgebra              as HU
-- import qualified Numeric.LinearAlgebra.Static       as H
import           Control.Monad
import           Hedgehog
import           Lens.Micro
import           Nudge
import           Numeric.Backprop
import           Numeric.Backprop.Tuple
import           System.Exit
import           System.IO
import qualified Numeric.LinearAlgebra.Static.Backprop as B

prop_vec2 :: Property
prop_vec2 = nudgeProp2 genDouble genDouble B.vec2

prop_vec3 :: Property
prop_vec3 = nudgeProp (T3 <$> genDouble <*> genDouble <*> genDouble)
                      (\t -> B.vec3 (t ^^. _1) (t ^^. _2) (t ^^. _3))

-- , vec4

prop_snoc :: Property
prop_snoc = nudgeProp2 (genVec @10) genDouble (B.&)

prop_append :: Property
prop_append = nudgeProp2 (genVec @10) (genVec @10) (B.#)

prop_split1 :: Property
prop_split1 = nudgeProp (genVec @10) (fst . B.split @5)

prop_split2 :: Property
prop_split2 = nudgeProp (genVec @10) (snd . B.split @5)

prop_headTail1 :: Property
prop_headTail1 = nudgeProp (genVec @10) (fst . B.headTail)

prop_headTail2 :: Property
prop_headTail2 = nudgeProp (genVec @10) (snd . B.headTail)

-- , vector

prop_linspace :: Property
prop_linspace = nudgeProp2 genDouble genDouble (B.linspace @10)

prop_row :: Property
prop_row = nudgeProp (genVec @10) B.row

prop_col :: Property
prop_col = nudgeProp (genVec @10) B.col

prop_horzcat :: Property
prop_horzcat = nudgeProp2 (genMat @10 @8) (genMat @10 @7) (B.|||)

prop_vertcat :: Property
prop_vertcat = nudgeProp2 (genMat @8 @10) (genMat @7 @10) (B.===)

prop_splitRows1 :: Property
prop_splitRows1 = nudgeProp (genMat @9 @10) (fst . B.splitRows @5)

prop_splitRows2 :: Property
prop_splitRows2 = nudgeProp (genMat @9 @10) (snd . B.splitRows @5)

prop_splitCols1 :: Property
prop_splitCols1 = nudgeProp (genMat @10 @9) (fst . B.splitCols @5)

prop_splitCols2 :: Property
prop_splitCols2 = nudgeProp (genMat @10 @9) (snd . B.splitCols @5)

prop_unrow :: Property
prop_unrow = nudgeProp (genMat @_ @10) B.unrow

prop_uncol :: Property
prop_uncol = nudgeProp (genMat @10) B.uncol

prop_tr :: Property
prop_tr = nudgeProp (genMat @10 @9) B.tr

prop_diag :: Property
prop_diag = nudgeProp (genVec @10) B.diag

-- TODO: bug in diagR
-- prop_svd :: Property
-- prop_svd = nudgeProp (genMat @10 @9) B.svd

-- TODO: bug in diagR
-- prop_svd_ :: Property
-- prop_svd_ = nudgeProp (genMat @10 @9) ((\(_,x,_) -> x) . B.svd_)

prop_eigensystem1 :: Property
prop_eigensystem1 = nudgeProp (genMat @10 @9) (fst . B.eigensystem . B.mTm)

prop_eigensystem2 :: Property
prop_eigensystem2 = nudgeProp (genMat @10 @9) (snd . B.eigensystem . B.mTm)

prop_eigenvalues :: Property
prop_eigenvalues = nudgeProp (genMat @10 @9) (B.eigenvalues . B.mTm)

prop_chol :: Property
prop_chol = nudgeProp (genMat @10 @9) (B.chol . B.mTm)

prop_norm_0M :: Property
prop_norm_0M = nudgeProp (genMat @10 @9) B.norm_0

prop_norm_0V :: Property
prop_norm_0V = nudgeProp (genVec @10) B.norm_0

prop_norm_1V :: Property
prop_norm_1V = nudgeProp (genVec @10) B.norm_1V

prop_norm_1M :: Property
prop_norm_1M = nudgeProp (genMat @10 @9) B.norm_1M

prop_norm_2V :: Property
prop_norm_2V = nudgeProp (genVec @10) B.norm_2V

prop_norm_2M :: Property
prop_norm_2M = nudgeProp (genMat @10 @9) B.norm_2M

prop_norm_InfV :: Property
prop_norm_InfV = nudgeProp (genVec @10) B.norm_InfV

prop_norm_InfM :: Property
prop_norm_InfM = nudgeProp (genMat @10 @9) B.norm_InfM

prop_mean :: Property
prop_mean = nudgeProp (genVec @10) B.mean

-- prop_meanCov1 :: Property
-- prop_meanCov1 = nudgeProp (genMat @10 @9) (fst . B.meanCov)

-- prop_meanCov2 :: Property
-- prop_meanCov2 = nudgeProp (genMat @10 @9) (B.unSym . snd . B.meanCov)

prop_meanL :: Property
prop_meanL = nudgeProp (genMat @10 @9) B.meanL

-- prop_cov :: Property
-- prop_cov = nudgeProp (genMat @10 @9) (B.unSym . B.cov)

prop_mul :: Property
prop_mul = nudgeProp2 (genMat @10 @8) (genMat @8 @9) B.mul

prop_app :: Property
prop_app = nudgeProp2 (genMat @10 @8) (genVec @8) B.app

prop_dot :: Property
prop_dot = nudgeProp2 (genVec @10) (genVec @10) B.dot

prop_cross :: Property
prop_cross = nudgeProp2 genVec genVec B.cross

-- TODO: bug in diagR?
-- prop_diagR :: Property
-- prop_diagR = nudgeProp2 genDouble (genVec @8) (B.diagR @10 @9)

  -- , dvmap
  -- , dvmap'
  -- , dmmap
  -- , dmmap'

prop_outer :: Property
prop_outer = nudgeProp2 (genVec @10) (genVec @9) B.outer

  -- , zipWithVector
  -- , zipWithVector'

prop_det :: Property
prop_det = nudgeProp (genMat @10 @10) B.det

prop_invlndet1 :: Property
prop_invlndet1 = nudgeProp (genMat @10 @10) (fst . B.invlndet)

prop_invlndet2 :: Property
prop_invlndet2 = nudgeProp (genMat @10 @10) (fst . snd . B.invlndet)

prop_invlndet3 :: Property
prop_invlndet3 = nudgeProp (genMat @10 @10) (snd . snd . B.invlndet)

prop_lndet :: Property
prop_lndet = nudgeProp (genMat @10 @10) B.lndet

-- TODO: more general invertible matrix
--
-- also, bug in inv?
prop_inv :: Property
prop_inv = nudgeProp (genMat @10 @9) (B.inv . B.unSym . B.mTm)

-- , toRows
-- , toColumns
-- , fromRows
-- , fromColumns

prop_konstV :: Property
prop_konstV = nudgeProp genDouble (B.konst @_ @(B.R 10))

prop_konstM :: Property
prop_konstM = nudgeProp genDouble (B.konst @_ @(B.L 10 9))

-- , extractV
-- , extractM
-- , create

prop_takeDiag :: Property
prop_takeDiag = nudgeProp (genMat @10 @10) B.takeDiag

prop_sym :: Property
prop_sym = nudgeProp (genMat @10 @10) (B.unSym . B.sym)

prop_mTm :: Property
prop_mTm = nudgeProp (genMat @10 @9) (B.unSym . B.mTm)

prop_unSym :: Property
prop_unSym = nudgeProp (genMat @10 @10) (B.unSym . B.sym)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- checkSequential $$(discover)

  unless results exitFailure

