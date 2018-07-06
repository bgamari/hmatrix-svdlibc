{-# LANGUAGE ForeignFunctionInterface #-}

module Numeric.LinearAlgebra.SVD.SVDLIBC
    ( -- * Singular value decomposition
      svd, svd'
      -- * Sparse SVD
    , sparsify
    , sparseSvd, sparseSvd'
      -- * Parameters
    , SVDParams(..), defaultSVDParams
    ) where

import Control.Applicative
import qualified Numeric.LinearAlgebra.Data as P
import qualified Numeric.LinearAlgebra.Devel as I
import qualified Data.Vector.Storable as Vec
import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import System.IO.Unsafe
import Foreign.Marshal.Alloc
import System.IO

newtype DenseMatrix = DMat (ForeignPtr DenseMatrix)
                    deriving (Eq, Ord, Show)
foreign import ccall unsafe "svdNewDMatFromArray" _newDMatFromArray :: CInt -> CInt -> Ptr Double -> IO (Ptr DenseMatrix)
foreign import ccall unsafe "&free_dmat" p_freeDMat :: FunPtr (Ptr DenseMatrix -> IO ())

newtype SparseMatrix = SMat (ForeignPtr SparseMatrix)
                     deriving (Eq, Ord, Show)

foreign import ccall unsafe "svdNewSMat" _newSMat :: CInt -> CInt -> IO (Ptr SparseMatrix)
foreign import ccall unsafe "svd_new_smat_from_csr" _newSMatFromCSRT :: CInt -> CInt -> CInt -> Ptr CLong -> Ptr CLong -> Ptr Double -> IO (Ptr SparseMatrix)
foreign import ccall unsafe "&svdFreeSMat" p_freeSMat :: FunPtr (Ptr SparseMatrix -> IO ())
foreign import ccall unsafe "svdTransposeS" _transposeSMat :: Ptr SparseMatrix -> IO (Ptr SparseMatrix)

foreign import ccall unsafe "svdConvertDtoS" _convertDToS :: Ptr DenseMatrix -> IO (Ptr SparseMatrix)
foreign import ccall unsafe "svdConvertStoD" _convertSToD :: Ptr SparseMatrix -> IO (Ptr DenseMatrix)

newtype SVDRec = SVDRec (ForeignPtr SVDRec)
foreign import ccall unsafe "svdLAS2" _svdLAS2 :: Ptr SparseMatrix -> CLong
                                               -> CLong -> Ptr CDouble -> CDouble -> IO (Ptr SVDRec)

foreign import ccall unsafe "get_svdrec_ut" getUt :: Ptr SVDRec -> IO (Ptr DenseMatrix)
foreign import ccall unsafe "get_svdrec_s" getS :: Ptr SVDRec -> IO (Ptr Double)
foreign import ccall unsafe "get_svdrec_vt" getVt :: Ptr SVDRec -> IO (Ptr DenseMatrix)
foreign import ccall unsafe "get_svdrec_rank" getRank :: Ptr SVDRec -> IO CLong

foreign import ccall unsafe "get_dmat_rows" getRows :: Ptr DenseMatrix -> IO CLong
foreign import ccall unsafe "get_dmat_cols" getCols :: Ptr DenseMatrix -> IO CLong
foreign import ccall unsafe "get_dmat_buffer" getBuffer :: Ptr DenseMatrix -> IO (Ptr Double)

foreign import ccall unsafe "get_smat_rows" getSRows :: Ptr SparseMatrix -> IO CLong
foreign import ccall unsafe "get_smat_cols" getSCols :: Ptr SparseMatrix -> IO CLong
foreign import ccall unsafe "get_smat_pointr" getSPointr :: Ptr SparseMatrix -> IO (Ptr CLong)
foreign import ccall unsafe "get_smat_rowind" getSRowind :: Ptr SparseMatrix -> IO (Ptr CLong) --long?
foreign import ccall unsafe "get_smat_buffer" getSBuffer :: Ptr SparseMatrix -> IO (Ptr Double)

foreign import ccall unsafe "set_verbosity" setVerbosity :: CLong -> IO ()

-- Our approach to memory management for dmats isn't entirely future-proof as we currently
-- free the library's data structures directly, keeping the underlying
-- buffers around for our own purposes
asDMat :: Ptr DenseMatrix -> IO DenseMatrix
asDMat ptr = DMat <$> newForeignPtr p_freeDMat ptr

asSMat :: Ptr SparseMatrix -> IO SparseMatrix
asSMat ptr = SMat <$> newForeignPtr p_freeSMat ptr

wrapSMatrix :: I.CSR -> IO SparseMatrix
wrapSMatrix csr = do
  let tfst (x,_,_) = x
  Vec.unsafeWith (Vec.map fromIntegral $ I.csrRows csr) $ \rowvec ->
    Vec.unsafeWith (Vec.map fromIntegral $ I.csrCols csr) $ \colvec ->
      Vec.unsafeWith (I.csrVals csr) $ \valvec -> do
        smatptr <- _newSMatFromCSRT
          (fromIntegral $ I.csrNRows csr)
          (fromIntegral $ I.csrNCols csr)
          (fromIntegral $ P.size $ I.csrVals csr)
          rowvec
          colvec
          valvec
        asSMat smatptr

transposeSMatrix :: SparseMatrix -> IO SparseMatrix
transposeSMatrix (SMat fptr) = withForeignPtr fptr $ \ptr->
    _transposeSMat ptr >>= asSMat

matrixToDMatrix :: P.Matrix Double -> IO DenseMatrix
matrixToDMatrix m = do
    let m' = P.flatten m
        (fptr, offset, length) = I.unsafeToForeignPtr m'
    dmat <- withForeignPtr fptr $ _newDMatFromArray (fromIntegral $ P.rows m) (fromIntegral $ P.cols m)
    asDMat dmat

dMatrixToMatrix :: DenseMatrix -> IO (P.Matrix Double)
dMatrixToMatrix (DMat fptr) = withForeignPtr fptr $ \ptr->do
    rows <- fromIntegral <$> getRows ptr
    cols <- fromIntegral <$> getCols ptr
    value <- getBuffer ptr >>= newForeignPtr_
    return $ I.matrixFromVector I.RowMajor rows cols
           $ I.unsafeFromForeignPtr value 0 (rows*cols)

dMatrixToSMatrix :: DenseMatrix -> IO SparseMatrix
dMatrixToSMatrix (DMat fptr) = withForeignPtr fptr $ \ptr->
    _convertDToS ptr >>= asSMat

sMatrixToDMatrix :: SparseMatrix -> IO DenseMatrix
sMatrixToDMatrix (SMat fptr) = withForeignPtr fptr $ \ptr->
    _convertSToD ptr >>= asDMat

data SVDParams = SVDParams { maxIters :: Maybe Int
                             -- ^ Maximum iteration count
                           , minEigenvalRange :: (Double, Double)
                             -- ^ Eigenvalues in this range are considered uninteresting
                           , kappa :: Double
                           }

-- | No iteration limit, exclude eigenvalues in range \((-10^{-30}, +10^{-30})@,
-- kappa of @10^{-6}\).
defaultSVDParams :: SVDParams
defaultSVDParams = SVDParams { maxIters = Nothing
                             , minEigenvalRange = (-1e-30, 1e-30)
                             , kappa = 1e-6
                             }

runSvd :: SVDParams -> Int -> SparseMatrix -> IO SVDRec
runSvd params rank (SMat fptr) = withForeignPtr fptr $ \ptr->do
    let iterLimit = maybe 0 fromIntegral (maxIters params)
        (a,b) = minEigenvalRange params
    res <- withArray [realToFrac a, realToFrac b] $ \eigenvalRangePtr ->
        _svdLAS2 ptr (fromIntegral rank) iterLimit eigenvalRangePtr (realToFrac $ kappa params)
    SVDRec <$> newForeignPtr finalizerFree res

unpackSvdRec :: SVDRec -> IO (P.Matrix Double, P.Vector Double, P.Matrix Double)
unpackSvdRec (SVDRec fptr) = withForeignPtr fptr $ \ptr->do
    rank <- fromIntegral <$> getRank ptr
    ut <- getUt ptr >>= asDMat >>= dMatrixToMatrix
    ptrS <- getS ptr >>= newForeignPtr finalizerFree
    let s = I.unsafeFromForeignPtr ptrS 0 rank
    vt <- getVt ptr >>= asDMat >>= dMatrixToMatrix
    return (ut, s, vt)

-- | @svd rank a@ is the sparse SVD of matrix @a@ with the given rank
-- This function handles the conversion to svdlibc's sparse representation.
svd :: Int -> P.Matrix Double -> (P.Matrix Double, P.Vector Double, P.Matrix Double)
svd = svd' defaultSVDParams

-- | Similar to 'svd' but allowing control over the 'SVDParams'.
svd' :: SVDParams -> Int -> P.Matrix Double
     -> (P.Matrix Double, P.Vector Double, P.Matrix Double)
svd' params rank m = unsafePerformIO $ do
    setVerbosity 0
    >> matrixToDMatrix m
    >>= dMatrixToSMatrix
    >>= runSvd params rank
    >>= unpackSvdRec

sparsify :: P.Matrix Double -> I.CSR
sparsify mat = I.CSR {
    I.csrVals = P.flatten mat,
    -- The following are indexed from 1! This will be undone in sparseSvd(shiftCSR)
    -- but it needs to match HMatrix's standard until then
    I.csrCols = Vec.iterateN (rows*cols) (\x -> (x `mod` i32cols) + 1) 1,
    I.csrRows = Vec.iterateN (rows+1) (+i32cols) 1,
    I.csrNRows = rows,
    I.csrNCols = cols
  } where
    (rows, cols) = P.size mat
    (i32rows, i32cols) = (fromIntegral rows, fromIntegral cols)

shiftCSR :: I.CSR -> I.CSR
shiftCSR csr = csr {
    I.csrRows = Vec.map (subtract 1) $ I.csrRows csr,
    I.csrCols = Vec.map (subtract 1) $ I.csrCols csr
  }

-- | @sparseSvd rank a@ is the sparse SVD of matrix @a@ with the given rank
-- This function handles the conversion to svdlibc's sparse representation,
-- but does not require making the whole matrix dense first
sparseSvd :: Int -> I.CSR -> (P.Matrix Double, P.Vector Double, P.Matrix Double)
sparseSvd = sparseSvd' defaultSVDParams

-- | Like 'sparseSvd' but providing greater control over the 'SVDParams' used
-- for the computation.
sparseSvd' :: SVDParams -> Int -> I.CSR
           -> (P.Matrix Double, P.Vector Double, P.Matrix Double)
sparseSvd' params rank m = unsafePerformIO $
    setVerbosity 0
    >>  (wrapSMatrix $ shiftCSR m)
    >>= runSvd params rank
    >>= unpackSvdRec
