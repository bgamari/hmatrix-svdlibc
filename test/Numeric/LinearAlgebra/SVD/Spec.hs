{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((><))
import qualified Data.Vector.Storable as Vec
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel
import Numeric.LinearAlgebra.SVD.SVDLIBC as SVD
import Debug.Trace
import System.IO

newtype RandHMat = RandHMat (Matrix Double) deriving Show

instance Arbitrary (RandHMat) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    vector <- Vec.replicateM (x*y) arbitrary
    return $ RandHMat $ reshape y vector

main :: IO ()
main = hspec $ do
  let summat = Vec.foldr (+) 0 . flatten
      validate u s v ref = summat (mconcat [tr u, diag s, v] - ref) / summat ref < 0.01

  describe "Dense matrices" $ do
    it "Factorizes a random dense matrix" $ do
      let (width, height) = (20, 10)
      mat <- randn width height
      short <- return $! min width height
      (u,s',v) <- return $! SVD.svd short mat
      s <- return $! vjoin [s', konst 0 (short - size s') ]
      validate u s v mat `shouldBe` True


    prop "Factorizes a random dense matrix (quickcheck)" $ \(RandHMat mat) ->
      let (width, height) = size mat
          short = min width height
          (u,s',v) = SVD.svd short mat
          s = vjoin [s', konst 0 (short - size s') ]
      in  (width > 1 && height > 1) ==> validate u s v mat

  describe "Sparse matrices" $ do
    it "Factorizes a random sparse matrix" $ do
      let (width, height) = (20, 10)
      m <- randn width height
      csr <- return $! SVD.sparsify m
      short <- return $! min width height
      (u,s',v) <- return $! SVD.sparseSvd short csr
      s <- return $! vjoin [s', konst 0 (short - size s') ]
      validate u s v m `shouldBe` True

    prop "Factorizes a random sparse matrix (quickcheck)" $ \(RandHMat mat) ->
      let (width, height) = size mat
          short = min width height
          csr = SVD.sparsify mat
          (u,s',v) = SVD.sparseSvd short csr
          s = vjoin [s', konst 0 (short - size s') ]
      in  (width > 1 && height > 1) ==> validate u s v mat
