(in-package :cl-tensor.cublas)


;;; CUDA
(define-condition cuda-error (error)
  ((datum :initarg :datum :reader cuda-error-datum))
  (:report (lambda (o s)
             (format s "The cudaERROR_t ~S is invoked."
                     (cuda-error-datum o)))))

(defun check-cuda-error (cuda-error)
  (if (eq cuda-error :cuda-success)
      cuda-error
      (error 'cuda-error :datum cuda-error)))

(defmacro defcufun ((cname lisp-name) &body args)
  (let ((%lisp-name (intern (concatenate 'string "%" (string lisp-name)))))
    `(progn
       (defun ,lisp-name (,@(mapcar #'first args))
         (check-cuda-error (,%lisp-name ,@(mapcar #'first args))))
       (defcfun (,cname ,%lisp-name) cuda-error
         ,@args))))

(defun create-cuda-pointer (size)
  (cffi:with-foreign-object (=>=>x/d :pointer)
    (check-cuda-error
      (cuda-malloc =>=>x/d (* (cffi:foreign-type-size :float) size)))
    (cffi:mem-ref =>=>x/d :pointer)))


;;; Define macros and utils to define cublas_v2.h function
;;; status
(define-condition cublas-status-error (error)
  ((status :initarg :status :reader cublas-status-error-status))
  (:report (lambda (o s)
             (format s "The CUBLAS_STATUS_t ~S is invoked."
                     (cublas-status-error-status o)))))

(defun check-status (status)
  (if (eq status :cublas-status-success)
      status
      (error 'cublas-status-error :status status)))

;;; handle
(defun create-handle ()
  (cffi:with-foreign-object (=>handle 'handle)
    (check-status (create =>handle))
    (cffi:mem-ref =>handle 'handle)))

(defmacro with-handle (var &body body)
  `(let ((,var (create-handle)))
     (unwind-protect (multiple-value-prog1 (progn ,@body))
       (check-status (destroy ,var)))))

(defmacro with-handles (vars &body body)
  `(let ,(mapcar (lambda (var) `(,var (create-handle))) vars)
     (unwind-protect (multiple-value-prog1 (progn ,@body))
       ,@(mapcar (lambda (var) `(check-status (destroy ,var))) vars))))

;;; macros for definition
(defmacro defcublasfun_v2 ((cname lisp-name) &body args)
  (let ((cname_v2 (concatenate 'string cname "_v2"))
        (%lisp-name (intern (concatenate 'string "%"(string lisp-name)))))
    `(progn
       (defun ,lisp-name (,@(mapcar #'first args))
         (check-status (,%lisp-name ,@(mapcar #'first args))))
       (defcfun (,cname_v2 ,%lisp-name) status
         ,@args))))

(defmacro defcublasfun_v2* ((cname lisp-name &key downcase? real-only? complex-only? single-only? add-half?) &body args)
  (assert (not (and real-only? complex-only?)) (real-only? complex-only?)
          ":REAL-ONLY? and :COMPLEX-ONLY? were both supplied.")
  (let ((types (mapcar (if downcase? #'char-downcase #'identity)
                       (cond ((and real-only? add-half?)    (list #\S #\D #\H))
                             (real-only?                    (list #\S #\D))
                             ((and complex-only? add-half?) (list #\C #\Z #\H))
                             (complex-only?                 (list #\C #\Z))
                             ((and single-only? add-half?)  (list #\S #\C #\H))
                             (single-only?                  (list #\S #\C))
                             (add-half?                     (list #\S #\D #\C #\Z #\H))
                             (t                             (list #\S #\D #\C #\Z))))))
    `(progn
       ,@(mapcar
           (lambda (tp)
             `(defcublasfun_v2 (,(substitute tp #\? cname)
                                ,(intern
                                   (string-upcase
                                     (substitute tp #\? (string lisp-name)))))
                ,@args))
           types))))



#|
(let ((=>dev (create-cuda-pointer 6))
      (=>host (cffi:foreign-aref))
      )
  (cuda-free =>dev))

(cffi:with-foreign-object (=>handle 'handle)
  (check-status (create =>handle))
  (check-status (destroy (cffi:mem-ref =>handle 'handle))))

(with-handle handle)

|#
