(defsystem "cl-tensor"
  :version "0.1.0"
  :author "ekatsym"
  :license "LLGPL"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:module "cublas"
                  :serial t
                  :components
                  ((:file "package")
                   (:cffi-grovel-file "grovel")
                   (:file "library")
                   (:file "util"))))))
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
