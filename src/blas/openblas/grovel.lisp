(in-package :cl-tensor.blas.cblas)


(include "openblas_config.h")

(ctype blasint "blasint")


(include "cblas.h")

(ctype cblas-index "CBLAS_INDEX")

(cenum cblas-order
  ((:cblas-row-major      "CblasRowMajor"))
  ((:cblas-col-major      "CblasColMajor")))

(cenum cblas-transpose
  ((:cblas-no-trans       "CblasNoTrans"))
  ((:cblas-trans      "CblasTrans"))
  ((:cblas-conj-trans     "CblasConjTrans"))
  ((:cblas-conj-no-trans  "CblasConjNoTrans")))

(cenum cblas-uplo
  ((:cblas-upper          "CblasUpper"))
  ((:cblas-lower          "CblasLower")))

(cenum cblas-diag
  ((:cblas-non-unit       "CblasNonUnit"))
  ((:cblas-unit           "CblasUnit")))

(cenum cblas-side
  ((:cblas-left           "CblasLeft"))
  ((:cblas-right          "CblasRight")))
