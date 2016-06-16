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

main :: IO ()
main = hspec $ do
  describe "Dense matrices" $ do
    prop "Factorizes a random matrix" $ \(width, items) ->
      let height = length items `div` width
          m = (width >< height) items
          short = min width height
          (u,s',v) = SVD.svd short m
          s = vjoin [s', konst 0 (short - size s') ]
      in  width <= 3
          || height <= 3
          || find (\x -> abs x > 0.001) (mconcat [tr u, diag s, v] - m) == []

    prop "Factorizes a random sparse matrix" $ \(width, items) ->
      let height = length items `div` width
          m = (width >< height) items
          csr = SVD.sparsify m
          short = min width height
          (u,s',v) = SVD.sparseSvd short csr
          s = vjoin [s', konst 0 (short - size s') ]
      in  width <= 3
          || height <= 3
          || find (\x -> abs x > 0.001) (mconcat [tr u, diag s, v] - m) == []
