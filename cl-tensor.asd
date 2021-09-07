(defsystem "cl-tensor"
  :version "0.1.0"
  :author "ekatsym"
  :license "LLGPL"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "core"   :depends-on ("util"))
                 (:module "blas" :depends-on ("util" "core")
                  :components
                  ((:file "main")
                   (:file "util"      :depends-on ("main"))
                   (:module "array"   :depends-on ("main" "util")
                    :serial t
                    :components
                    ((:file             "package")
                     (:file             "util")
                     (:file             "main")))
                   (:module "carray"  :depends-on ("main" "util")
                    :serial t
                    :components
                    ((:file             "package")
                     (:cffi-grovel-file "grovel")
                     (:file             "util")
                     (:file             "library")
                     (:file             "main")))
                   (:module "cuarray" :depends-on ("main" "util")
                    :serial t
                    :components
                    ((:file             "package")
                     (:cffi-grovel-file "grovel")
                     (:file             "util")
                     (:file             "library")
                     (:file             "main")))))
                 ;(:file "tensor" :depends-on ("util" "blas"))
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
                ((:file "blas"))))
  :description "Test system for cl-tensor"
  :perform (test-op (op c) (unless (symbol-call :rove :run c)
                             #+sbcl (sb-ext:exit :code 1)
                             #+ccl  (ccl:quit 1)
                             #+abcl (extensions:exit :status 1)
                             #+ecl  (si:quit 1))))
