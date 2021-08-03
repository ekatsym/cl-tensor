(in-package :cl-tensor.blas.cblas)


(defmacro defcfun* ((cname lisp-name &key real-only?) return-type &body args)
  (let ((types (cond (real-only?  (list #\s #\d))
                     (t           (list #\s #\d #\c #\z)))))
    (flet ((ensure-ctype (tp ctype)
             (if (eq ctype '?type)
                 (ecase tp
                   (#\s :float)
                   (#\d :double)
                   (#\c :pointer)
                   (#\z :pointer))
                 ctype)))
      `(progn
         ,@(mapcar
             (lambda (tp)
               `(defcfun (,(substitute tp #\? cname)
                           ,(intern
                              (string-upcase
                                (substitute
                                  tp #\?
                                  (string lisp-name))))) ,(ensure-ctype tp return-type)
                         ,@(mapcar
                             (lambda (arg)
                               (destructuring-bind (lisp-arg ctype) arg
                                 `(,lisp-arg ,(ensure-ctype tp ctype))))
                             args)))
             types)))))
