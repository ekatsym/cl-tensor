(defpackage cl-tensor.tensor
  (:use :common-lisp)
  (:nicknames :clt.tensor)
  (:export
    ;; core
    #:tensor

    #:transpose
    #:diag
    ))
(in-package :cl-tensor.tensor)
