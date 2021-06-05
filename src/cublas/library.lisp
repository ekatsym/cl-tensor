(in-package :cl-tensor.cublas)


;;; libcudart
(define-foreign-library libcudart
  (:unix "libcudart.so")
  (t (:default "libcudart")))
(use-foreign-library libcudart)

;;; libcublas
(define-foreign-library libcublas
  (:unix "libcublas.so")
  (t (:default "libcublas")))
(use-foreign-library libcublas)


;;;;; CUDA
;;; Define CUDA functions
(defcufun ("cudaMalloc" cuda-malloc)
  (=>=>x/d :pointer)
  (size :size))

(defcufun ("cudaFree" cuda-free)
  (=>x/d :pointer))

(defcufun ("cudaMemcpy" cuda-memcpy)
  (=>dst :pointer)
  (=>src :pointer)
  (count :size)
  (kind cuda-memcpy-kind))

(defcufun ("cudaDeviceSynchronize" cuda-device-synchronize))


;;;;; cuBLAS
;;; Define cuBLAS helper functions
(defcublasfun_v2 ("cublasCreate" create)
  (=>handle :pointer))

(defcublasfun_v2 ("cublasDestroy" destroy)
  (handle handle))

(defcublasfun_v2 ("cublasGetVersion" get-version)
  (handle handle)
  (=>version :pointer))

(defcfun ("cublasGetProperty" get-property) handle
  (type library-property-type)
  (=>value :pointer))

(defcublasfun_v2 ("cublasSetStream" set-stream)
  (handle handle)
  (stream-id cuda-stream))

(defcublasfun_v2 ("cublasSetWorkspace" set-workspace)
  (handle handle)
  (=>workspace :pointer)
  (workspace-size-in-bytes :size))

(defcublasfun_v2 ("cublasGetStream" get-stream)
  (handle handle)
  (=>stream-id :pointer))

(defcublasfun_v2 ("cublasGetPointerMode" get-pointer-mode)
  (handle handle)
  (=>mode :pointer))

(defcublasfun_v2 ("cublasSetPointerMode" set-pointer-mode)
  (handle handle)
  (=>mode :pointer))

(defcublasfun_v2 ("cublasSetVector" set-vector)
  (n :int)
  (elem-size :int)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2 ("cublasGetVector" get-vector)
  (n :int)
  (elem-size :int)
  (=>x :pointer)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2 ("cublasSetMatrix" set-matrix)
  (rows :int)
  (cols :int)
  (elem-size :int)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int))

(defcublasfun_v2 ("cublasGetMatrix" get-matrix)
  (rows :int)
  (cols :int)
  (elem-size :int)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int))

(defcublasfun_v2 ("cublasSetVectorAsync" set-vector-async)
  (n :int)
  (elem-size :int)
  (=>host :pointer)
  (incx :int)
  (=>device :pointer)
  (incy :int)
  (stream cuda-stream))

(defcublasfun_v2 ("cublasGetVectorAsync" get-vector-async)
  (n :int)
  (elem-size :int)
  (=>device :pointer)
  (incx :int)
  (=>host :pointer)
  (incy :int)
  (stream cuda-stream))

(defcublasfun_v2 ("cublasSetMatrixAsync" set-matrix-async)
  (rows :int)
  (cols :int)
  (elem-size :int)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (stream cuda-stream))

(defcublasfun_v2 ("cublasGetMatrixAsync" get-matrix-async)
  (rows :int)
  (cols :int)
  (element-size :int)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (stream cuda-stream))

(defcublasfun_v2 ("cublasSetAtomicsMode" set-atomics-mode)
  (handle handle)
  (mode atomics-mode))

(defcublasfun_v2 ("cublasGetAtomicsMode" get-atomics-mode)
  (handle handle)
  (=>mode :pointer))

(defcublasfun_v2 ("cublasSetMathMode" set-math-mode)
  (handle handle)
  (mode math))

(defcublasfun_v2 ("cublasGetMathMode" get-math-mode)
  (handle handle)
  (=>mode :pointer))

(defcublasfun_v2 ("cublasLoggerConfigure" logger-configure)
  (log-is-on :int)
  (log-to-stdout :int)
  (log-to-stderr :int)
  (log-file-name (:pointer :char)))

(defcublasfun_v2 ("cublasGetLoggerCallback" get-logger-callback)
  (user-callback :pointer))

(defcublasfun_v2 ("cublasSetLoggerCallback" set-logger-callback)
  (user-callback log-callback))


;;; Define cuBLAS level-1 functions
(defcublasfun_v2* ("cublasI?amax" i?amax :downcase? t)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>result :pointer))

(defcublasfun_v2* ("cublasI?amin" i?amin :downcase? t)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>result :pointer))

(defcublasfun_v2* ("cublas?asum" ?asum :downcase? t)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>result :pointer))

(defcublasfun_v2* ("cublas?axpy" ?axpy)
  (handle handle)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?copy" ?copy)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?dot" ?dot)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int)
  (=>result :pointer))

(defcublasfun_v2* ("cublas?nrm2" ?nrm2)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>result :pointer))

(defcublasfun_v2* ("cublas?rot" ?rot)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int)
  (=>c :pointer)
  (=>s :pointer))

(defcublasfun_v2* ("cublas?rotg" ?rotg)
  (handle handle)
  (=>a :pointer)
  (=>b :pointer)
  (=>c :pointer))

(defcublasfun_v2* ("cublas?rotm" ?rotm :real-only? t)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int)
  (=>param :pointer))

(defcublasfun_v2* ("cublas?rotmg" ?rotmg :real-only? t)
  (handle handle)
  (=>d1 :pointer)
  (=>d2 :pointer)
  (=>x1 :pointer)
  (=>y1 :pointer)
  (=>param :pointer))

(defcublasfun_v2* ("cublas?scal" ?scal)
  (handle handle)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int))

(defcublasfun_v2* ("cublas?swap" ?swap)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int))

;;; Define cuBLAS level-2 functions
(defcublasfun_v2* ("cublas?gdmv" ?gdmv)
  (handle handle)
  (trans operation)
  (m :int)
  (n :int)
  (kl :int)
  (ku :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int)
  (=>beta :pointer)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?gemv" ?gemv)
  (handle handle)
  (trans operation)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int)
  (=>beta :pointer)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?ger" ?ger)
  (handle handle)
  (m :int)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int)
  (=>a :pointer)
  (lda :int))

(defcublasfun_v2* ("cublas?sbmv" ?sbmv :real-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int)
  (=>beta :pointer)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?spmv" ?spmv :real-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>ap :pointer)
  (=>x :pointer)
  (incx :int)
  (=>beta :pointer)
  (=>y :float)
  (incy :int))

(defcublasfun_v2* ("cublas?spr" ?spr :real-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>ap :pointer))

(defcublasfun_v2* ("cublas?spr2" ?spr2 :real-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int)
  (=>ap :pointer))

(defcublasfun_v2* ("cublas?symv" ?symv)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int)
  (=>beta :pointer)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?syr" ?syr)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>a :int)
  (lda :int))

(defcublasfun_v2* ("cublas?syr2" ?syr2)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int)
  (=>a :pointer)
  (lda :int))

(defcublasfun_v2* ("cublas?tbmv" ?tbmv)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (n :int)
  (k :int)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int))

(defcublasfun_v2* ("cublas?tbvs" ?tbvs)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (n :int)
  (k :int)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int))

(defcublasfun_v2* ("cublas?tpmv" ?tpmv)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (n :int)
  (=>ap :pointer)
  (=>x :pointer)
  (incx :int))

(defcublasfun_v2* ("cublas?tpsv" ?tpsv)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (n :int)
  (=>ap :pointer)
  (=>x :pointer)
  (incx :int))

(defcublasfun_v2* ("cublas?trmv" ?trmv)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (n :int)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int))

(defcublasfun_v2* ("cublas?trsv" ?trsv)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (n :int)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int))

(defcublasfun_v2* ("cublas?hemv" ?hemv :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int)
  (=>beta :pointer)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?hbmv" ?hbmv :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int)
  (=>beta :pointer)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?hpmv" ?hpmv :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>ap :pointer)
  (=>x :pointer)
  (incx :int)
  (=>beta :pointer)
  (=>y :pointer)
  (incy :int))

(defcublasfun_v2* ("cublas?her" ?her :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>a :pointer)
  (lda :int))

(defcublasfun_v2* ("cublas?her2" ?her2 :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>a :pointer)
  (lda :int))

(defcublasfun_v2* ("cublas?hpr" ?hpr :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>ap :pointer))

(defcublasfun_v2* ("cublas?hpr2" ?hpr2 :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>alpha :pointer)
  (=>x :pointer)
  (incx :int)
  (=>y :pointer)
  (incy :int)
  (=>ap :pointer))


;;; Define cuBLAS level3 functions
(defcublasfun_v2* ("cublas?gemm" ?gemm :add-half? t)
  (handle handle)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?gemm3m" ?gemm3m :complex-only? t)
  (handle handle)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?gemmBatched" ?gemm-batched :add-half? t)
  (handle handle)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a-array :pointer)
  (lda :int)
  (=>b-array :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c-array :pointer)
  (ldc :int)
  (batch-count :int))

(defcublasfun_v2* ("cublas?gemmStridedBatched" ?gemm-strided-batched)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (stride-a :long-long)
  (=>b :pointer)
  (ldb :int)
  (stride-b :long-long)
  (=>c :pointer)
  (ldc :int)
  (stride-c :long-long)
  (batch-count :int))

(defcublasfun_v2 ("cublasCgemm3mStridedBatched" cgemm3m-strided-batched)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (stride-a :long-long)
  (=>b :pointer)
  (ldb :int)
  (stride-b :long-long)
  (=>c :pointer)
  (ldc :int)
  (stride-c :long-long)
  (batch-count :int))

(defcublasfun_v2* ("cublas?symm" ?symm)
  (handle handle)
  (side side-mode)
  (uplo fill-mode)
  (m :int)
  (n :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?syrk" ?syrk)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?syr2k" ?syr2k)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?syrkx" ?syrkx)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?trmm" ?trmm)
  (handle handle)
  (side side-mode)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (m :int)
  (n :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?trsm" ?trsm)
  (handle handle)
  (side side-mode)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (m :int)
  (n :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int))

(defcublasfun_v2* ("cublas?trsmBatched" ?trsm-batched)
  (handle handle)
  (side side-mode)
  (uplo fill-mode)
  (trans operation)
  (diag diag-type)
  (m :int)
  (n :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (batch-count :int))

(defcublasfun_v2* ("cublas?hemm" ?hemm :complex-only? t)
  (handle handle)
  (side side-mode)
  (uplo fill-mode)
  (m :int)
  (n :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?herk" ?herk :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?her2k" ?her2k :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?herkx" ?herkx :complex-only? t)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>b :pointer)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (ldc :int))


;;; Define BLAS-like functions
(defcublasfun_v2* ("cublas?geam" ?geam)
  (handle handle)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (lda :int)
  (=>beta :pointer)
  (=>b :pointer)
  (ldb :int)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?dgmm" ?dgmm)
  (handle handle)
  (mode side-mode)
  (m :int)
  (n :int)
  (=>a :pointer)
  (lda :int)
  (=>x :pointer)
  (incx :int)
  (=>c :pointer)
  (ldc :int))

(defcublasfun_v2* ("cublas?getrfBatched" ?getrf-batched)
  (handle handle)
  (n :int)
  (=>a-array :pointer)
  (lda :int)
  (=>pivot-array :pointer)
  (=>info-array :pointer)
  (batch-size :int))

(defcublasfun_v2* ("cublas?getrsBatched" ?getrs-batched)
  (handle handle)
  (trans operation)
  (n :int)
  (nrhs :int)
  (=>a-array :pointer)
  (lda :int)
  (=>dev-ipiv :pointer)
  (=>b-array :pointer)
  (ldb :int)
  (=>info :pointer)
  (batch-size :int))

(defcublasfun_v2* ("cublas?riBatched" ?ri-batched)
  (handle handle)
  (n :int)
  (=>a-array :pointer)
  (lda :int)
  (=>pivot-array :pointer)
  (=>c-array :pointer)
  (ldc :int)
  (=>info-array :int)
  (batch-size :pointer))

(defcublasfun_v2* ("cublas?matinvBatched" ?matinv-batched)
  (handle handle)
  (=>a :pointer)
  (lda :int)
  (=>a-inv :pointer)
  (lda_inv :int)
  (=>info :pointer)
  (batch-size :int))

(defcublasfun_v2* ("cublas?geqrfBatched" ?geqrf-batched)
  (handle handle)
  (m :int)
  (n :int)
  (=>a-array :pointer)
  (lda :int)
  (=>tau-array :pointer)
  (=>info :pointer)
  (batch-size :int))

(defcublasfun_v2* ("cublas?gelsBatched" ?gels-batched)
  (handle handle)
  (trans operation)
  (m :int)
  (n :int)
  (nrhs :int)
  (=>a-array :pointer)
  (lda :int)
  (=>c-array :pointer)
  (ldc :int)
  (=>info :pointer)
  (=>dev-info-array :pointer)
  (batch-size :int))

(defcublasfun_v2* ("cublas?tpttr" ?tpttr)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>ap :pointer)
  (=>a :pointer)
  (lda :int))

(defcublasfun_v2* ("cublas?trttp" ?trttp)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (=>a :pointer)
  (lda :int)
  (=>ap :pointer))

(defcublasfun_v2* ("cublas?gemmEx" ?gemm* :single-only? t)
  (handle handle)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (a-type cuda-data-type)
  (lda :int)
  (=>b :pointer)
  (b-type cuda-data-type)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (c-type cuda-data-type)
  (ldc :int))

(defcublasfun_v2 ("cublasGemmEx" gemm*)
  (handle handle)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (a-type cuda-data-type)
  (lda :int)
  (=>b :pointer)
  (b-type cuda-data-type)
  (ldb :int)
  (=>beta :pointer)
  (=>c :pointer)
  (c-type cuda-data-type)
  (ldc :int)
  (compute-type compute-type)
  (algo gemm-algo))

(defcublasfun_v2 ("cublasGemmStridedBatchedEx" gemm-strided-batched*)
  (handle handle)
  (transa operation)
  (transb operation)
  (m :int)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (a-type cuda-data-type)
  (lda :int)
  (stride-a :long-long)
  (=>b :pointer)
  (b-type cuda-data-type)
  (ldb :int)
  (stride-b :long-long)
  (=>beta :pointer)
  (=>c :pointer)
  (c-type cuda-data-type)
  (ldc :int)
  (stride-c :long-long)
  (batch-count :int)
  (compute-type compute-type)
  (algo gemm-algo))

(defcublasfun_v2 ("cublasCsyrkEx" csyrk*)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (a-type cuda-data-type)
  (lda :int)
  (=>beta :pointer)
  (=>c :pointer)
  (c-type cuda-data-type)
  (ldc :int))

(defcublasfun_v2 ("cublasCsyrk3mEx" csyrk3m*)
  (handle handle)
  (uplo fill-mode)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (a-type cuda-data-type)
  (lda :int)
  (=>beta :pointer)
  (=>c :pointer)
  (c-type cuda-data-type)
  (ldc :int))

(defcublasfun_v2 ("cublasCherkEx" cherk*)
  (handle handle)
  (uplo operation)
  (trans operation)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (a-type cuda-data-type)
  (lda :int)
  (=>beta :pointer)
  (=>c :pointer)
  (c-type cuda-data-type)
  (ldc :int))

(defcublasfun_v2 ("cublasCherk3mEx" cherk3m*)
  (handle handle)
  (uplo fill-mode)
  (trans operation)
  (n :int)
  (k :int)
  (=>alpha :pointer)
  (=>a :pointer)
  (a-type cuda-data-type)
  (lda :int)
  (=>beta :pointer)
  (=>c :pointer)
  (c-type cuda-data-type)
  (ldc :int))

(defcublasfun_v2 ("cublasNrm2Ex" nrm2*)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (x-type cuda-data-type)
  (incx :int)
  (=>result :pointer)
  (result-type cuda-data-type)
  (execution-type cuda-data-type))

(defcublasfun_v2 ("cublasAxpyEx" axpy*)
  (handle handle)
  (n :int)
  (=>alpha :pointer)
  (alpha-type cuda-data-type)
  (=>x :pointer)
  (x-type cuda-data-type)
  (incx :int)
  (=>y :pointer)
  (y-type cuda-data-type)
  (incy :int)
  (execution-type cuda-data-type))

(defcublasfun_v2 ("cublasDotEx" dot*)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (x-type cuda-data-type)
  (incx :int)
  (=>y :pointer)
  (y-type cuda-data-type)
  (incy :int)
  (=>result :pointer)
  (result-type cuda-data-type)
  (execution-type cuda-data-type))

(defcublasfun_v2 ("cublasDotcEx" dotc*)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (x-type cuda-data-type)
  (incx :int)
  (=>y :pointer)
  (y-type cuda-data-type)
  (incy :int)
  (=>result :pointer)
  (result-type cuda-data-type)
  (execution-type cuda-data-type))

(defcublasfun_v2 ("cublasRotEx" rot*)
  (handle handle)
  (n :int)
  (=>x :pointer)
  (x-type cuda-data-type)
  (incx :int)
  (=>y :pointer)
  (y-type cuda-data-type)
  (incy :int)
  (=>c :pointer)
  (=>s :pointer)
  (cs-type cuda-data-type)
  (execution-type cuda-data-type))

(defcublasfun_v2 ("cudaScalEx" scal*)
  (handle handle)
  (n :int)
  (=>alpha :pointer)
  (alpha-type cuda-data-type)
  (=>x :pointer)
  (x-type cuda-data-type)
  (incx :int)
  (execution-type cuda-data-type))
