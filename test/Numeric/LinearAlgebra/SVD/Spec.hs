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
          --m = fromBlocks [[mb, mb], [mb, mb]] -- tile it four times
          short = min width height
          (u,s',v) = SVD.svd short m
          sp = vjoin [s', konst 0 (short - size s') ]
          s = traceShow (tr u, sp, v, mconcat [tr u, diag sp, v] - m) sp
      in  width <= 3
          || height <= 3
          || find (\x -> abs x > 0.001) (mconcat [tr u, diag s, v] - m) == []

    --prop "Factorizes a random sparse matrix" $ \(width, items) ->
      --let height = length items `div` width
      --    mb = (width >< height) items
      --    m = fromBlocks [[mb, mb], [mb, mb]]
      --    csr = SVD.sparsify m
      --    (u,s,v) = SVD.sparseSvd (min width height) csr
      --in  width < 1
      --    || height < 1
      --    || find (\x -> abs x > 0.01) ((tr u <> diag s <> v) - m) == []
