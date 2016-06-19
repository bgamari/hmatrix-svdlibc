{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import qualified Data.Vector.Storable as Vec
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel
import Numeric.LinearAlgebra.SVD.SVDLIBC as SVD
import System.IO


main :: IO ()
main = defaultMain
  [ bench "Factorizes a random dense matrix" $ nfIO $ do
      let (width, height) = (20, 10)
      SVD.svd (min width height) <$> randn width height
  , bench "Factorizes a random sparse matrix" $ nfIO $ do
      let (width, height) = (20, 10)
      SVD.sparseSvd (min width height) <$> SVD.sparsify <$> randn width height
  ]
