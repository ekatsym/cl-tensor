(defpackage cl-tensor.cublas
  (:nicknames :clts.cublas)
  (:use :common-lisp :cffi)
  (:export
    ;; grovel types
    #:+version+
    #:cu-complex
    #:cu-double-complex
    #:handle
    #:status
    #:operation
    #:fill-mode
    #:diag-type
    #:side-mode
    #:pointer-mode
    #:atomics-mode
    #:gemm-algo
    #:math
    #:compute-type
    #:log-callback
    #:cuda-stream

    ;; library
    #:libcublas

    ;; cuBLAS helper functions
    #:create
    #:destroy
    #:get-version
    #:set-property
    #:set-stream
    #:get-stream
    #:get-pointer-mode
    #:set-pointer-mode
    #:set-vector
    #:get-vector
    #:set-matrix
    #:get-matrix
    #:set-vector-async
    #:get-vector-async
    #:set-matrix-async
    #:get-matrix-async
    #:set-atomics-mode
    #:get-atomics-mode
    #:set-math-mode
    #:get-math-mode
    #:logger-configure
    #:get-logger-callback
    #:set-logger-callback

    ;; cuBLAS level-1 functions
    #:isamax #:idamax #:icamax #:izamax
    #:isamin #:idamin #:icamin #:izamin
    #:saxpy #:daxpy #:caxpy #:zaxpy
    #:scopy #:dcopy #:ccopy #:zcopy
    #:sdot #:ddot #:cdot #:zdot
    #:snrm2 #:dnrm2 #:cnrm2 #:znrm2
    #:srot #:drot #:crot #:zrot
    #:srotg #:drotg #:crotg #:zrotg
    #:srotm #:drotm #:crotm #:zrotm
    #:srotmg #:drotmg #:crotmg #:zrotmg
    #:sscal #:dscal #:cscal #:zscal
    #:sswap #:dswap #:cswap #:zswap

    ;; cuBLAS level-2 functions
    #:sgdmv #:dgdmv #:cgdmv #:zgdmv
    #:sgemv #:dgemv #:cgemv #:zgemv
    #:sger #:dger #:cger #:zger
    #:ssbmv #:dsbmv
    #:sspmv #:dspmv
    #:sspr #:dspr
    #:sspr2 #:dspr2
    #:sspmv #:dspmv
    #:ssymv #:dsymv #:csymv #:zsymv
    #:ssyr #:dsyr #:csyr #:zsyr
    #:ssyr2 #:dsyr2 #:csyr2 #:zsyr2
    #:stbmv #:dtbmv #:ctbmv #:ztbmv
    #:stbvs #:dtbvs #:ctbvs #:ztbvs
    #:stpmv #:dtpmv #:ctpmv #:ztpmv
    #:stpsv #:dtpsv #:ctpsv #:ztpsv
    #:strmv #:dtrmv #:ctrmv #:ztrmv
    #:strsv #:dtrsv #:ctrsv #:ztrsv
    #:chemv #:zhemv
    #:chbmv #:zhbmv
    #:chpmv #:zhpmv
    #:cher #:zher
    #:cher2 #:zher2
    #:chpr #:zhpr
    #:chpr2 #:zhpr2

    ;; cuBLAS level-3 functions
    #:sgemm #:dgemm #:cgemm #:zgemm #:hgemm
    #:sgemm3m #:dgemm3m #:cgemm3m #:zgemm3m #:hgemm3m
    #:cgemm-batched #:zgemm-batched
    #:sgemm-strided-batched #:dgemm-strided-batched #:cgemm-strided-batched #:zgemm-strided-batched
    #:cgemm3m-strided-batched
    #:ssymm #:dsymm #:csymm #:zsymm
    #:ssyrk #:dsyrk #:csyrk #:zsyrk
    #:ssyr2k #:dsyr2k #:csyr2k #:zsyr2k
    #:ssyrkx #:dsyrkx #:csyrkx #:zsyrkx
    #:strsm #:dtrsm #:ctrsm #:ztrsm
    #:strsm-batched #:dtrsm-batched #:ctrsm-batched #:ztrsm-batched
    #:chemm #:zhemm
    #:cherk #:zherk
    #:cher2k #:zher2k
    #:cherkx #:zherkx

    ;; cuBLAS-like functions
    #:sgeam #:dgeam #:cgeam #:zgeam
    #:sdgmm #:ddgmm #:cdgmm #:zdgmm
    #:sgetrf-batched #:dgetrf-batched #:cgetrf-batched #:zgetrf-batched
    #:sgetrs-batched #:dgetrs-batched #:cgetrs-batched #:zgetrs-batched
    #:sri-batched #:dri-batched #:cri-batched #:zri-batched
    #:smatinv-batched #:dmatinv-batched #:cmatinv-batched #:zmatinv-batched
    #:sgeqrf-batched #:dgeqrf-batched #:cgeqrf-batched #:zgeqrf-batched
    #:sgels-batched #:dgels-batched #:cgels-batched #:zgels-batched
    #:stpttr #:dtpttr #:ctpttr #:ztpttr
    #:strttp #:dtrttp #:ctrttp #:ztrttp
    #:sgemm* #:cgemm*
    #:gemm*
    #:gemm-strided-batched
    #:csyrk*
    #:csyrk3m*
    #:cherk*
    #:cherk3m*
    #:nrm2*
    #:axpy*
    #:dot*
    #:dotc*
    #:rot*
    #:scal*))
