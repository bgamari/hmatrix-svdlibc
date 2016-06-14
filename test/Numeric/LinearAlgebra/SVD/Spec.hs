{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Data.Vector.Unboxed as Vec
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.SVD.SVDLIBC as SVD


main :: IO ()
main = hspec $ do
  describe "Dense matrices" $ do
    it "Factorizes an identity matrix" $ do
      mb <- randn 2 3
      let m = fromBlocks [[mb, mb], [mb, mb]]
      let (u,s,v) = SVD.svd 2 m
      print $ (size u, size s, size v)
      find (\x -> abs x > 0.01) ((tr u <> diag s <> v) - m) `shouldBe` []
