(in-package :cl-string-generator)

(defvar *max-length*)
(defvar *registers*)

(defun minimum (range)
  (if (null range)
      0
      (loop :for elt :in range
            :minimize (if (consp elt)
                          (car elt)
                          elt))))

(defun maximum (range)
  (if (null range)
      *max-length*
      (loop :for elt :in range
            :maximize (if (consp elt)
                          (cdr elt)
                          elt))))

(defgeneric add-range (x y)
  (:method ((x integer) (y integer))
    (+ x y))
  (:method ((x integer) (y cons))
    (cons (+ x (car y)) (+ x (cdr y))))
  (:method ((x cons) (y integer))
    (cons (+ (car x) y) (+ (cdr x) y)))
  (:method ((x cons) (y cons))
    (cons (+ (car x) (car y)) (+ (cdr x) (cdr y)))))

(defun all-length-candidates (range)
  (let ((acc '()))
    (labels ((f (range sum)
               (if (null range)
                   (pushnew sum acc :test #'equal)
                   (dolist (n (first range))
                     (f (rest range) (add-range n sum))))))
      (f range 0)
      (nreverse acc))))

(defgeneric compute-range-of-length-aux (regex))

(defmethod compute-range-of-length-aux ((regex void))
  (list 0))

(defmethod compute-range-of-length-aux ((regex str))
  (list (length (.str regex))))

(defmethod compute-range-of-length-aux ((regex seq))
  (all-length-candidates
   (loop :for regex-1 :in (.seq regex)
         :for range := (compute-range-of-length-aux regex-1)
         :collect range)))

(defmethod compute-range-of-length-aux ((regex alternation))
  (loop :for regex-1 :in (.choices regex)
        :for range := (compute-range-of-length-aux regex-1)
        :append range))

(defmethod compute-range-of-length-aux ((regex repetition))
  (let ((range (compute-range-of-length-aux (.regex regex))))
    (list (cons (* (minimum range)
                   (.minimum regex))
                (* (maximum range)
                   (.maximum regex))))))

(defmethod compute-range-of-length-aux ((regex register))
  (let ((range (compute-range-of-length-aux (.regex regex))))
    (setf (gethash (.number regex) *registers*) range)
    range))

(defmethod compute-range-of-length-aux ((regex back-reference))
  (gethash (.number regex) *registers*))

(defmethod compute-range-of-length-aux ((regex random-char))
  (list 1))

(defun compute-range-of-length (regex)
  (let ((*registers* (make-hash-table)))
    (compute-range-of-length-aux regex)))

(defun compute-fix-length (regex *max-length*)
  (let ((range (compute-range-of-length regex)))
    (mean (loop :for elt :in range
                :collect (if (consp elt)
                             (cdr elt)
                             elt)))))
