(in-package :cl-string-generator)

(defvar *registers*)

(define-condition value (condition)
  ((value :initarg :value :reader value-of))
  (:report (lambda (c s)
             (format s "value: ~S" (value-of c)))))

(defun value (value)
  (check-type value string)
  (error 'value :value value))

(defmacro select ((value form) &body body)
  (with-unique-names (c)
    `(handler-bind ((value (lambda (,c)
                             (let ((,value (value-of ,c)))
                               ,@body)
                             (when (find-restart 'next)
                               (invoke-restart 'next)))))
       ,form)))

(defgeneric generate-aux (regex rest-length))

(defmethod generate-aux ((regex null) rest-length)
  (value ""))

(defmethod generate-aux ((regex void) rest-length)
  (value ""))

(defmethod generate-aux ((regex str) rest-length)
  (let ((string (.str regex)))
    (when (<= (length string) rest-length)
      (value string))))

(defmethod generate-aux ((regex seq) rest-length)
  (labels ((f (seq acc rest-length)
             (cond ((null seq)
                    (value acc))
                   (t
                    (select (string (generate-aux (first seq) rest-length))
                      (f (rest seq)
                         (string-append acc string)
                         (- rest-length (length string))))))))
    (f (.seq regex) "" rest-length)))

(defmethod generate-aux ((regex alternation) rest-length)
  (let ((choices (.choices regex)))
    (dolist (choice (shuffle (copy-list choices)))
      (select (string (generate-aux choice rest-length))
        (value string)))))

(defmethod generate-aux ((regex repetition) rest-length)
  (let ((inner-regex (.regex regex)))
    (labels ((f (n acc rest-length)
               (cond ((zerop n)
                      (value acc))
                     (t
                      (select (string (generate-aux inner-regex rest-length))
                        (f (1- n)
                           (string-append acc string)
                           (- rest-length (length string))))))))
      (let ((maximum (min (.maximum regex) rest-length))
            (minimum (.minimum regex)))
        (dolist (n (shuffle (loop :for n :from minimum :to maximum :collect n)))
          (restart-case
              (f n "" rest-length)
            (next ())))))))

(defmethod generate-aux ((regex register) rest-length)
  (handler-bind ((value (lambda (value)
                          (let ((string (value-of value)))
                            (setf (gethash (.number regex) *registers*)
                                  string)))))
    (generate-aux (.regex regex) rest-length)))

(defmethod generate-aux ((regex back-reference) rest-length)
  (let ((string (gethash (.number regex) *registers*)))
    (when (<= (length string) rest-length)
      (value string))))

(defmethod generate-aux ((regex random-char) rest-length)
  (when (<= 1 rest-length)
    (value (string (funcall (.function regex))))))

(defun %generate (regex max-length)
  (let (#+sbcl
        (sb-kernel:*maximum-error-depth* 1000000)
        (*registers* (make-hash-table)))
    (restart-case
        (generate-aux (convert regex) (or max-length +string-length-limit+))
      (next ()))))

(defun generate-using-handler (regex max-length function)
  (handler-bind ((value (lambda (value)
                          (cond ((find-restart 'next)
                                 (funcall function (value-of value))
                                 (invoke-restart 'next))
                                (t
                                 (return-from generate-using-handler))))))
    (%generate regex max-length)))

(defun generate (regex &key (min-length 0) max-length)
  (generate-using-handler regex
                          max-length
                          (lambda (string)
                            (when (<= min-length (length string))
                              (return-from generate string)))))

#+(or)
(defun generate* (regex &optional (max-length 10))
  (generate-using-handler regex
                          max-length
                          (lambda (string)
                            (print string))))
