(defpackage cl-tensor.core
  (:nicknames :clt.core :clt.cr)
  (:use :common-lisp :cl-tensor.util)
  (:export
    #:element-type))
(in-package :cl-tensor.core)


(deftype element-type ()
  '(or single-float double-float (complex single-float) (complex double-float)))
