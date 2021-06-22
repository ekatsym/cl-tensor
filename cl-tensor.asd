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
                  ((:module "cublas"
                    :serial t
                    :components
                    ((:file "package")
                     (:cffi-grovel-file "grovel")
                     (:file "util")
                     (:file "library")))
                   (:module "openblas")
                   (:file "package")
                   (:file "cuarray" :depends-on ("cublas" "package"))
                   (:file "oparray" :depends-on ("openblas" "package"))
                   (:file "main" :depends-on ("cuarray" "oparray"))))
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
