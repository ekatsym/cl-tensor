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

(defun make-device-pointer (type count)
  (with-foreign-object (=>=>x/d :pointer)
    (check-cuda-error
      (cuda-malloc =>=>x/d (* (foreign-type-size type) count)))
    (mem-ref =>=>x/d :pointer)))

(defun call-with-device-pointer (type count function)
  (let ((=>x/d (make-device-pointer type count)))
    (unwind-protect (funcall function =>x/d)
      (cuda-free =>x/d))))

(defmacro with-device-pointer ((var type count) &body body)
  `(let ((,var (make-device-pointer ,type ,count)))
     (unwind-protect
       (multiple-value-prog1 ,@body)
       (cuda-free ,var))))

(defun call-with-device-pointers (type-count-s function)
  (let ((=>xs/d (mapcar (lambda (type-count)
                          (destructuring-bind (type count) type-count
                            (make-device-pointer type count))) type-count-s)))
    (unwind-protect (apply function =>xs/d)
      (mapc #'cuda-free =>xs/d))))

(defmacro with-device-pointers ((&rest var-type-count-s) &body body)
  `(let (,@(mapcar (lambda (var-type-count)
                     (destructuring-bind (var type count) var-type-count
                       `(,var (make-device-pointer ,type ,count))))
                   var-type-count-s))
     (unwind-protect
       (multiple-value-prog1 ,@body)
       ,@(mapcar (lambda (var-type-count)
                   (destructuring-bind (var _1 _2) var-type-count
                     (declare (ignore _1 _2))
                     `(cuda-free ,var)))
                 var-type-count-s))))


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
  (with-foreign-object (=>handle 'handle)
    (check-status (create =>handle))
    (mem-ref =>handle 'handle)))

(defun call-with-handle (function)
  (let ((handle (create-handle)))
    (unwind-protect (funcall function handle)
      (cuda-device-synchronize)
      (destroy handle))))

(defmacro with-handle (var &body body)
  `(let ((,var (create-handle)))
     (unwind-protect (multiple-value-prog1 (progn ,@body))
       (cuda-device-synchronize)
       (check-status (destroy ,var)))))

(defun call-with-handles (n function)
  (if (zerop n)
      function
      (let ((handle (create-handle)))
        (call-with-handles (1- n) (funcall function handle)))))

(defmacro with-handles (vars &body body)
  `(with-handle ,(first vars)
     (with-handles ,(rest vars) ,@body)))

;;; macros for definition
(defmacro defcublasfun ((cname lisp-name) &body args)
  (let ((%lisp-name (intern (concatenate 'string "%"(string lisp-name)))))
    `(progn
       (defun ,lisp-name (,@(mapcar #'first args))
         (check-status (,%lisp-name ,@(mapcar #'first args))))
       (defcfun (,cname ,%lisp-name) status
         ,@args))))

(defmacro defcublasfun* ((cname lisp-name &key downcase? real-only? complex-only? single-only? add-half?) &body args)
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
             `(defcublasfun (,(substitute tp #\? cname)
                              ,(intern
                                 (string-upcase
                                   (substitute tp #\? (string lisp-name)))))
                ,@args))
           types))))

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
