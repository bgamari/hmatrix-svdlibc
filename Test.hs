import Data.Packed as P
import Numeric.LinearAlgebra.SVD.SVDLIBC as SVD
import Numeric.LinearAlgebra

main = do
    let m = ident 100
        (u,s,v) = SVD.svd 50 m
    print $ s
    print $ u `mXm` diag s `mXm` trans v
