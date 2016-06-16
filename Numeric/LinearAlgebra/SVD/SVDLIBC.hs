{-# LANGUAGE ForeignFunctionInterface #-}

module Numeric.LinearAlgebra.SVD.SVDLIBC
    (svd, sparsify, sparseSvd) where

import Control.Applicative
import qualified Numeric.LinearAlgebra.Data as P
import qualified Numeric.LinearAlgebra.Devel as I
import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import System.IO.Unsafe
import Foreign.Marshal.Alloc

newtype DenseMatrix = DMat (ForeignPtr DenseMatrix)
                    deriving (Eq, Ord, Show)
foreign import ccall unsafe "svdNewDMatFromArray" _newDMatFromArray :: CInt -> CInt -> Ptr Double -> IO (Ptr DenseMatrix)
foreign import ccall unsafe "&free_dmat" p_freeDMat :: FunPtr (Ptr DenseMatrix -> IO ())

newtype SparseMatrix = SMat (ForeignPtr SparseMatrix)
                     deriving (Eq, Ord, Show)

foreign import ccall unsafe "svdNewSMat" _newSMat :: CInt -> CInt -> IO (Ptr SparseMatrix)
--svd_new_smat_from_csr(int rows, int cols, int vals, long *pointr, long *rowind, double *value)
foreign import ccall unsafe "svd_new_smat_from_csr" _newSMatFromCSR :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO (Ptr SparseMatrix)
foreign import ccall unsafe "&svdFreeSMat" p_freeSMat :: FunPtr (Ptr SparseMatrix -> IO ())
foreign import ccall unsafe "svdTransposeS" _transposeSMat :: Ptr SparseMatrix -> IO (Ptr SparseMatrix)

foreign import ccall unsafe "svdConvertDtoS" _convertDToS :: Ptr DenseMatrix -> IO (Ptr SparseMatrix)
foreign import ccall unsafe "svdConvertStoD" _convertSToD :: Ptr SparseMatrix -> IO (Ptr DenseMatrix)

newtype SVDRec = SVDRec (ForeignPtr SVDRec)
foreign import ccall unsafe "svdLAS2A" _svdLAS2 :: Ptr SparseMatrix -> CLong -> IO (Ptr SVDRec)

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
  withForeignPtr (tfst $ I.unsafeToForeignPtr $ I.csrRows csr) $ \rowvec ->
    withForeignPtr (tfst $ I.unsafeToForeignPtr $ I.csrCols csr) $ \colvec ->
      withForeignPtr (tfst $ I.unsafeToForeignPtr $ I.csrVals csr) $ \valvec ->
        _newSMatFromCSR
          (fromIntegral $ I.csrNRows csr)
          (fromIntegral $ I.csrNCols csr)
          (fromIntegral $ P.size $ I.csrVals csr)
          rowvec
          colvec
          valvec
          >>= asSMat

createSMatrix :: Int -> Int -> IO SparseMatrix
createSMatrix rows cols = do
    _newSMat (fromIntegral rows) (fromIntegral cols) >>= asSMat

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

runSvd :: Int -> SparseMatrix -> IO SVDRec
runSvd rank (SMat fptr) = withForeignPtr fptr $ \ptr->do
    res <- _svdLAS2 ptr (fromIntegral rank)
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
svd rank m = unsafePerformIO $ do
    setVerbosity 0 >> matrixToDMatrix m >>= dMatrixToSMatrix >>= runSvd rank >>= unpackSvdRec

sparsify :: P.Matrix Double -> I.CSR
sparsify mat = I.CSR {
    I.csrVals = P.flatten mat,
    I.csrCols = P.fromList $ mconcat $ replicate rows [1..fromIntegral cols],
    I.csrRows = P.fromList $ CInt <$> [1, i32cols+1 .. i32rows*i32cols+1],
    I.csrNRows = rows,
    I.csrNCols = cols
  } where
    (rows, cols) = P.size mat
    (i32rows, i32cols) = (fromIntegral rows, fromIntegral cols)

-- | @svd rank a@ is the sparse SVD of matrix @a@ with the given rank
-- This function handles the conversion to svdlibc's sparse representation,
-- but does not require making the whole matrix dense first
sparseSvd :: Int -> I.CSR -> (P.Matrix Double, P.Vector Double, P.Matrix Double)
sparseSvd rank m = unsafePerformIO $ do
    setVerbosity 0
    print m
    wrapSMatrix m >>= runSvd rank >>= unpackSvdRec
