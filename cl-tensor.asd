(defsystem "cl-tensor"
  :version "0.1.0"
  :author "ekatsym"
  :license "LLGPL"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:module "blas"
                  :depends-on ("util")
                  :components
                  ((:file "main")
                   (:module "cublas"
                    :serial t
                    :components
                    ((:file "package")
                     (:cffi-grovel-file "grovel")
                     (:file "util")
                     (:file "library")))
                   (:module "openblas"
                    :serial t
                    :components
                    ((:file "package")
                     (:cffi-grovel-file "grovel")
                     (:file "util")
                     (:file "library")))
                   (:file "assert"  :depends-on ("main"))
                   (:file "array"   :depends-on (           "main" "assert"))
                   (:file "cuarray" :depends-on ("cublas"   "main" "assert"))
                   (:file "oparray" :depends-on ("openblas" "main" "assert"))
                   (:file "convert" :depends-on ("main" "array" "cuarray" "oparray"))))
                 (:file "util"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-tensor/tests"))))

(defsystem "cl-tensor/tests"
  :author "ekatsym"
  :license "LLGPL"
  :depends-on ("cl-tensor"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-tensor"
  :perform (test-op (op c) (symbol-call :rove :run c)))
