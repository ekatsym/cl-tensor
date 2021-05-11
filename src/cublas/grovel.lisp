(in-package :cl-tensor.cublas)

(include "cublas_v2.h")

(cstruct cu-complex "cuComplex")
(cstruct cu-double-complex "cuDoubleComplex")


(constant (+cublas-version+ "CUBLAS_VERSION"))

(ctype cublas-handle         "cublasHandle_t")
(ctype cublas-status         "cublasStatus_t")
(ctype cublas-operation      "cublasOperation_t")
(ctype cublas-fill-mode      "cublasFillMode_t")
(ctype cublas-diag-type      "cublasDiagType_t")
(ctype cublas-side-mode      "cublasSideMode_t")
(ctype cublas-pointer-mode   "cublasPointerMode_t")
(ctype cublas-atomics-mode   "cublasAtomicsMode_t")
(ctype cublas-gemm-algo      "cublasGemmAlgo_t")
(ctype cublas-math           "cublasMath_t")
(ctype cublas-compute-type   "cublasComputeType_t")

(ctype cuda-data-type "cudaDataType_t")
(ctype library-property-type "libraryPropertyType_t")

(ctype cuda-stream "cudaStream_t")
