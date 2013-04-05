module Numeric.LinearAlgebra.SVD.SVDLIBC
    () where

import Control.Applicative
import qualified Data.Packed as P
import qualified Data.Packed.Development as I
import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import System.IO.Unsafe

newtype DenseMatrix = DMat (ForeignPtr DenseMatrix)
                    deriving (Eq, Ord, Show)
foreign import ccall unsafe "svdNewDMatFromArray" _newDMatFromArray :: CInt -> CInt -> Ptr Double -> IO (Ptr DenseMatrix)
foreign import ccall unsafe "&svdFreeDMat" p_freeDMat :: FunPtr (Ptr DenseMatrix -> IO ())

newtype SparseMatrix = SMat (ForeignPtr SparseMatrix)
                     deriving (Eq, Ord, Show)

foreign import ccall unsafe "svdNewSMat" _newSMat :: CInt -> CInt -> IO (Ptr SparseMatrix)
foreign import ccall unsafe "&svdFreeSMat" p_freeSMat :: FunPtr (Ptr SparseMatrix -> IO ())
foreign import ccall unsafe "svdTransposeS" _transposeSMat :: Ptr SparseMatrix -> IO (Ptr SparseMatrix)

foreign import ccall unsafe "svdConvertDToS" _convertDToS :: Ptr DenseMatrix -> IO (Ptr SparseMatrix)
foreign import ccall unsafe "svdConvertSToD" _convertSToD :: Ptr SparseMatrix -> IO (Ptr DenseMatrix)

newtype SVDRec = SVDRec (ForeignPtr SVDRec)
foreign import ccall unsafe "&svdFreeSVDRec" p_freeSVDRec :: FunPtr (Ptr SVDRec -> IO ())
foreign import ccall unsafe "svdLAS2A" svdLAS2 :: Ptr SparseMatrix -> CLong -> IO (Ptr SVDRec)

createSMatrix :: Int -> Int -> SparseMatrix
createSMatrix rows cols = unsafePerformIO $ do
    ptr <- _newSMat (fromIntegral rows) (fromIntegral cols)
    SMat <$> newForeignPtr p_freeSMat ptr

transposeSMatrix :: SparseMatrix -> SparseMatrix
transposeSMatrix (SMat fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr->do
    ptr' <- _transposeSMat ptr
    SMat <$> newForeignPtr p_freeSMat ptr'

runSvd :: Int -> SparseMatrix -> SVDRec
runSvd rank (SMat fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr->do
    res <- svdLAS2 ptr (fromIntegral rank)
    SVDRec <$> newForeignPtr p_freeSVDRec res

matrixToDMatrix :: P.Matrix Double -> DenseMatrix
matrixToDMatrix m = unsafePerformIO $ do
    let m' = P.flatten m
        (fptr, offset, length) = I.unsafeToForeignPtr m'
    dmat <- withForeignPtr fptr $ _newDMatFromArray (fromIntegral $ P.rows m) (fromIntegral $ P.cols m)
    DMat <$> newForeignPtr p_freeDMat dmat

dMatrixToSMatrix :: DenseMatrix -> SparseMatrix
dMatrixToSMatrix (DMat fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr->do
    smat <- _convertDToS ptr
    SMat <$> newForeignPtr p_freeSMat smat

sMatrixToDMatrix :: SparseMatrix -> DenseMatrix
sMatrixToDMatrix (SMat fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr->do
    dmat <- _convertSToD ptr
    DMat <$> newForeignPtr p_freeDMat dmat
