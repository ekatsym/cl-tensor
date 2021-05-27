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

(defun create-device-pointer (type count)
  (with-foreign-object (=>=>x/d :pointer)
    (check-cuda-error
      (cuda-malloc =>=>x/d (* (foreign-type-size type) count)))
    (mem-ref =>=>x/d :pointer)))

(defun call-with-device-pointer (type count function)
  (let ((=>x/d (create-device-pointer type count)))
    (unwind-protect (funcall function =>x/d)
      (cuda-free =>x/d))))

(defmacro with-device-pointer ((var type count) &body body)
  `(let ((,var (create-device-pointer ,type ,count)))
     (unwind-protect
       (multiple-value-prog1 ,@body)
       (cuda-free ,var))))

(defun call-with-device-pointers (type-count-s function)
  (let ((=>xs/d (mapcar (lambda (type-count)
                          (destructuring-bind (type count) type-count
                            (create-device-pointer type count))) type-count-s)))
    (unwind-protect (apply function =>xs/d)
      (mapc #'cuda-free =>xs/d))))

(defmacro with-device-pointers ((&rest var-type-count-s) &body body)
  `(let (,@(mapcar (lambda (var-type-count)
                     (destructuring-bind (var type count) var-type-count
                       `(,var (create-device-pointer ,type ,count))))
                   var-type-count-s))
     (unwind-protect
       (multiple-value-prog1 ,@body)
       ,@(mapcar #'first var-type-count-s))))


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
      (destroy handle))))

(defmacro with-handle (var &body body)
  `(let ((,var (create-handle)))
     (unwind-protect (multiple-value-prog1 (progn ,@body))
       (check-status (destroy ,var)))))

(defun call-with-handles (n function)
  (let ((handles (loop :repeat n :collect (create-handle))))
    (unwind-protect (apply function handles)
      (mapc #'destroy handles))))

(defmacro with-handles (vars &body body)
  `(let (,@(mapcar (lambda (var) `(,var (create-handle))) vars))
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
(defun matrix-multiply ()
  (declare (optimize (debug 3)))
  (let* ((block-size 32)
         (isize-multiple 5)
         (m (* 4 block-size isize-multiple))
         (n (* 2 block-size isize-multiple))
         (k (* 3 block-size isize-multiple)))
    (format t "starting...~%")
    (with-foreign-objects ((=>a/h :float (* m k))
                           (=>b/h :float (* k n))
                           (=>c/h :float (* m n))
                           (=>c/h* :float (* m n))
                           (=>alpha :float)
                           (=>beta :float))
      (dotimes (i (* m k)) (setf (mem-aref =>a/h :float i) (random 10000.0f1)))
      (dotimes (i (* k n)) (setf (mem-aref =>b/h :float i) (random 10000.0f1)))
      (setf (mem-ref =>alpha :float) 1.0f1
            (mem-ref =>beta  :float) 0.0f1)
      (with-device-pointers ((=>a/d :float (* m k))
                             (=>b/d :float (* k n))
                             (=>c/d :float (* m n)))
        (cuda-memcpy =>a/d =>a/h (* m k) :cuda-memcpy-host-to-device)
        (cuda-memcpy =>b/d =>b/h (* k n) :cuda-memcpy-host-to-device)
        (with-handle h
          (format t "warming up...~%")
          (sgemm h :cublas-op-n :cublas-op-n m n k
                 =>alpha
                 =>a/d m
                 =>b/d k
                 =>beta
                 =>c/d m)
          (format t "done!~%")
          (format t "computing...~%")
          (time (dotimes (i 300)
                  (sgemm h :cublas-op-n :cublas-op-n m n k
                         =>alpha
                         =>a/d m
                         =>b/d k
                         =>beta
                         =>c/d m)))
          (format t "done!~%")
          (format t "copying...~%")
          (cuda-memcpy =>c/h =>c/d (* m n) :cuda-memcpy-default)
          (format t "done!~%"))
        (time
          (dotimes (mi m)
            (dotimes (ni n)
              (setf (mem-aref =>c/h* :float (+ (* ni m) mi)) 0.0f1)
              (dotimes (ki k)
                (incf (mem-aref =>c/h* :float (+ (* ni m) mi))
                      (* (mem-aref =>a/h :float (+ (* ki m) mi))
                         (mem-aref =>b/h :float (+ (* ni k) ki))))))))
        (format t "done!~%")
        ;(dotimes (i (* m k))
        ;  (format t "~&~10F ~10F~%"
        ;          (mem-aref =>c/h  :float i)
        ;          (mem-aref =>c/h* :float i)))
        ))))

(matrix-multiply)

10f1
10d1

(defun main ()
  (with-foreign-object (device-prop ) (foreign-funcall "cudaGetDeviceProperties "))
  )



(defun vector-add (h n =>v1/d =>v2/d =>v3/d)
  (cuda-memcpy =>v3/d =>v1/d 10 :cuda-memcpy-device-to-device)
  (with-foreign-object (=>alpha :float)
    (setf (mem-ref =>alpha :float) 1.0)
    (saxpy h n =>alpha =>v2/d 1 =>v3/d 1))
  =>v3/d)

(defun main ()
  (with-device-pointers ((=>v1/d :float 10)
                         (=>v2/d :float 10)
                         (=>v3/d :float 10))
    (with-foreign-objects ((=>v1/h :float 10)
                           (=>v2/h :float 10)
                           (=>v3/h :float 10))
      (with-handle h
        (dotimes (i 10)
          (setf (mem-aref =>v1/h :float i) (random 1000.0)
                (mem-aref =>v2/h :float i) (random 1000.0)))
        (cuda-memcpy =>v1/d =>v1/h 10 :cuda-memcpy-host-to-device)
        (cuda-memcpy =>v2/d =>v2/h 10 :cuda-memcpy-host-to-device)
        (time (progn (vector-add h 10 =>v1/d =>v2/d =>v3/d)))
        (cuda-memcpy =>v3/h =>v3/d 10 :cuda-memcpy-default)
        (let ((array-type (make-instance 'cffi::foreign-array-type
                                         :dimensions '(10)
                                         :element-type :float)))
          (values (foreign-array-to-lisp =>v1/h array-type)
                  (foreign-array-to-lisp =>v2/h array-type)
                  (foreign-array-to-lisp =>v3/h array-type)))))))
(main)

(with-device-pointer (=>a/d :float (expt 2 16))
  (with-foreign-objects ((=>b :float (expt 2 16))
                         (=>c :float (expt 2 16)))
    (print "check1")
    (dotimes (i (expt 2 16)) (setf (mem-aref =>b :float i) (float i)))
    (print "check2")
    (print (cuda-memcpy =>a/d =>b (expt 2 16) :cuda-memcpy-default))
    (print "check3")
    (print (cuda-memcpy =>c =>a/d (expt 2 16) :cuda-memcpy-default))
    (print "check4")
    (foreign-array-to-lisp =>c (make-instance 'cffi::foreign-array-type
                                              :dimensions '((expt 2 16))
                                              :element-type :float))))

(with-device-pointer (=>a/d :float 99999)
  (error))

(locally
  (declare (optimize (debug 3)))
  (with-foreign-objects ((=>a :float 10)
                         (=>b :float 10))
    (dotimes (i 10)
      (setf (mem-aref =>a :float i) (float i)))
    (dotimes (i 10)
      (setf (mem-aref =>b :float i)
            (* 2.0 (mem-aref =>a :float i))))
    (foreign-array-to-lisp =>b (make-instance 'cffi::foreign-array-type
                                              :dimensions '(10)
                                              :element-type :float))))

(make-load-form)
(foreign-array-alloc )
:foreign-dataref
(lisp-array-to-foreign )
cffi::foreign-array-type
cffi::dimensions

(foreign-aref)
(dotimes (i 10) (print i))

(let ((=>dev (create-device-pointer 6))
      (=>host (foreign-array-alloc (make-array '(2 3)
                                               :initial-contents '((0 1 2) (3 4 5)))
                                   :float)))
  ()
  (cuda-free =>dev))

(with-foreign-object (=>handle 'handle)
  (check-status (create =>handle))
  (check-status (destroy (mem-ref =>handle 'handle))))

(with-handle handle)

|#
