import Data.Packed as P
import Numeric.LinearAlgebra.SVD.SVDLIBC as SVD
import Numeric.LinearAlgebra

main = do
    let m = ident 500
    print $ SVD.svd 10 m
