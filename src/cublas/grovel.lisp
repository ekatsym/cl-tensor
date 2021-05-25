(in-package :cl-tensor.cublas)

(include "cublas_v2.h")

(cstruct cu-complex "cuComplex")
(cstruct cu-double-complex "cuDoubleComplex")


(constant (+version+ "CUBLAS_VERSION"))

(ctype handle "cublasHandle_t")

(cenum status
  ((:cublas-status-success          "CUBLAS_STATUS_SUCCESS"))
  ((:cublas-status-not-initialized  "CUBLAS_STATUS_NOT_INITIALIZED"))
  ((:cublas-status-alloc-failed     "CUBLAS_STATUS_ALLOC_FAILED"))
  ((:cublas-status-invalid-value    "CUBLAS_STATUS_INVALID_VALUE"))
  ((:cublas-status-arch-mismatch    "CUBLAS_STATUS_ARCH_MISMATCH"))
  ((:cublas-status-mapping-error    "CUBLAS_STATUS_MAPPING_ERROR"))
  ((:cublas-status-execution-failed "CUBLAS_STATUS_EXECUTION_FAILED"))
  ((:cublas-status-internal-error   "CUBLAS_STATUS_INTERNAL_ERROR"))
  ((:cublas-status-not-supported    "CUBLAS_STATUS_NOT_SUPPORTED"))
  ((:cublas-status-license-error    "CUBLAS_STATUS_LICENSE_ERROR")))

(cenum operation
  ((:cublas-op-n "CUBLAS_OP_N"))
  ((:cublas-op-t "CUBLAS_OP_T"))
  ((:cublas-op-c "CUBLAS_OP_C")))

(cenum fill-mode
  ((:cublas-fill-mode-lower "CUBLAS_FILL_MODE_LOWER"))
  ((:cublas-fill-mode-upper "CUBLAS_FILL_MODE_UPPER"))
  ((:cublas-fill-mode-full  "CUBLAS_FILL_MODE_FULL")))

(cenum diag-type
  ((:cublas-diag-non-unit "CUBLAS_DIAG_NON_UNIT"))
  ((:cublas-diag-unit     "CUBLAS_DIAG_UNIT")))

(cenum side-mode
  ((:cublas-side-left  "CUBLAS_SIDE_LEFT"))
  ((:cublas-side-right "CUBLAS_SIDE_RIGHT")))

(cenum pointer-mode
  ((:cublas-pointer-mode-host   "CUBLAS_POINTER_MODE_HOST"))
  ((:cublas-pointer-mode-device "CUBLAS_POINTER_MODE_DEVICE")))

(cenum atomics-mode
  ((:cublas-atomics-not-allowed "CUBLAS_ATOMICS_NOT_ALLOWED"))
  ((:cublas-atomics-allowed     "CUBLAS_ATOMICS_ALLOWED")))

(cenum gemm-algo
  ((:cublas-gemm-default "CUBLAS_GEMM_DEFAULT"))
  ((:cublas-gemm-algo0   "CUBLAS_GEMM_ALGO0"))  ((:cublas-gemm-algo1   "CUBLAS_GEMM_ALGO1"))
  ((:cublas-gemm-algo2   "CUBLAS_GEMM_ALGO2"))  ((:cublas-gemm-algo3   "CUBLAS_GEMM_ALGO3"))
  ((:cublas-gemm-algo4   "CUBLAS_GEMM_ALGO4"))  ((:cublas-gemm-algo5   "CUBLAS_GEMM_ALGO5"))
  ((:cublas-gemm-algo6   "CUBLAS_GEMM_ALGO6"))  ((:cublas-gemm-algo7   "CUBLAS_GEMM_ALGO7"))
  ((:cublas-gemm-algo8   "CUBLAS_GEMM_ALGO8"))  ((:cublas-gemm-algo9   "CUBLAS_GEMM_ALGO9"))
  ((:cublas-gemm-algo10  "CUBLAS_GEMM_ALGO10")) ((:cublas-gemm-algo11   "CUBLAS_GEMM_ALGO11"))
  ((:cublas-gemm-algo12  "CUBLAS_GEMM_ALGO12")) ((:cublas-gemm-algo13   "CUBLAS_GEMM_ALGO13"))
  ((:cublas-gemm-algo14  "CUBLAS_GEMM_ALGO14")) ((:cublas-gemm-algo15   "CUBLAS_GEMM_ALGO15"))
  ((:cublas-gemm-algo16  "CUBLAS_GEMM_ALGO16")) ((:cublas-gemm-algo17   "CUBLAS_GEMM_ALGO17"))
  ((:cublas-gemm-algo18  "CUBLAS_GEMM_ALGO18")) ((:cublas-gemm-algo19   "CUBLAS_GEMM_ALGO19"))
  ((:cublas-gemm-algo20  "CUBLAS_GEMM_ALGO20")) ((:cublas-gemm-algo21   "CUBLAS_GEMM_ALGO21"))
  ((:cublas-gemm-algo22  "CUBLAS_GEMM_ALGO22")) ((:cublas-gemm-algo23   "CUBLAS_GEMM_ALGO23")))

(cenum math
   ((:cublas-default-math                               "CUBLAS_DEFAULT_MATH"))
   ((:cublas-pedantic_math                              "CUBLAS_PEDANTIC_MATH"))
   ((:cublas-tf32-tensor-op-math                        "CUBLAS_TF32_TENSOR_OP_MATH"))
   ((:cublas-math-disallowe-reduced-precision-reduction "CUBLAS_MATH_DISALLOW_REDUCED_PRECISION_REDUCTION")))

(cenum compute-type
  ((:cublas-compute-16f           "CUBLAS_COMPUTE_16F"))
  ((:cublas-compute-16f-pedantic  "CUBLAS_COMPUTE_16F_PEDANTIC"))
  ((:cublas-compute-32f           "CUBLAS_COMPUTE_32F"))
  ((:cublas-compute-32f-pedantic  "CUBLAS_COMPUTE_32F_PEDANTIC"))
  ((:cublas-compute-32f-fast-16f  "CUBLAS_COMPUTE_32F_FAST_16F"))
  ((:cublas-compute-32f-fast-16bf "CUBLAS_COMPUTE_32F_FAST_16BF"))
  ((:cublas-compute-32f-fast-tf32 "CUBLAS_COMPUTE_32F_FAST_TF32"))
  ((:cublas-compute-64f           "CUBLAS_COMPUTE_64F"))
  ((:cublas-compute-64f-pedantic  "CUBLAS_COMPUTE_64F_PEDANTIC"))
  ((:cublas-compute-32i           "CUBLAS_COMPUTE_32I"))
  ((:cublas-compute-32i-pedantic  "CUBLAS_COMPUTE_32I_PEDANTIC")))

(ctype log-callback "cublasLogCallback")

(cenum cuda-data-type
  ((:cuda-r-16f  "CUDA_R_16F"))
  ((:cuda-c-16f  "CUDA_C_16F"))
  ((:cuda-r-16bf "CUDA_R_16BF"))
  ((:cuda-c-16bf "CUDA_C_16BF"))
  ((:cuda-r-32f  "CUDA_R_32F"))
  ((:cuda-c-32f  "CUDA_C_32F"))
  ((:cuda-r-64f  "CUDA_R_64F"))
  ((:cuda-c-64f  "CUDA_C_64F"))
  ((:cuda-r-8i   "CUDA_R_8I"))
  ((:cuda-c-8i   "CUDA_C_8I"))
  ((:cuda-r-8u   "CUDA_R_8U"))
  ((:cuda-c-8u   "CUDA_C_8U"))
  ((:cuda-r-32i  "CUDA_R_32I"))
  ((:cuda-c-32i  "CUDA_C_32I")))

(cenum library-property-type
  ((:major-version "MAJOR_VERSION"))
  ((:minor-version "MINOR_VERSION"))
  ((:patch-level   "PATCH_LEVEL")))

(ctype cuda-stream "cudaStream_t")
