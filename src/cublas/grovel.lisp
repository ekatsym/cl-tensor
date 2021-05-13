(in-package :cl-tensor.cublas)

(include "cublas_v2.h")

(cstruct cu-complex "cuComplex")
(cstruct cu-double-complex "cuDoubleComplex")


(constant (+version+ "CUBLAS_VERSION"))

(ctype handle         "cublasHandle_t")
(ctype status         "cublasStatus_t")
(ctype operation      "cublasOperation_t")
(ctype fill-mode      "cublasFillMode_t")
(ctype diag-type      "cublasDiagType_t")
(ctype side-mode      "cublasSideMode_t")
(ctype pointer-mode   "cublasPointerMode_t")
(ctype atomics-mode   "cublasAtomicsMode_t")
(ctype gemm-algo      "cublasGemmAlgo_t")
(ctype math           "cublasMath_t")
(ctype compute-type   "cublasComputeType_t")

(ctype log-callback   "cublasLogCallback")

(ctype cuda-data-type "cudaDataType_t")
(ctype library-property-type "libraryPropertyType_t")

(ctype cuda-stream "cudaStream_t")
