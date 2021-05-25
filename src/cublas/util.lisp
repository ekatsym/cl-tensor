(in-package :cl-tensor.cublas)


;;; common
(define-condition cublas-error (error)
  ()
  (:documentation "The condition consists of errors invoked by cuBLAS."))


;;; status
(define-condition cublas-status-error (cublas-error)
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
  (cffi:with-foreign-object (*handle 'clt.cublas:handle)
    (check-status (clt.cublas:create *handle))
    (cffi:mem-ref *handle 'clt.cublas:handle)))

(defmacro with-handle (var &body body)
  `(let ((,var (create-handle)))
     (unwind-protect (multiple-value-prog1 (progn ,@body))
       (check-status (clt.cublas:destroy ,var)))))

(defmacro with-handles (vars &body body)
  `(let ,(mapcar (lambda (var) `(,var (create-handle))) vars)
     (unwind-protect (multiple-value-prog1 (progn ,@body))
       ,@(mapcar (lambda (var) `(check-status (clt.cublas:destroy ,var))) vars))))
