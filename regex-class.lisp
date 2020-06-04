(in-package :regex-generator)

(defclass regex ()
  ())

(defclass void (regex)
  ())

(defclass str (regex)
  ((str
    :initarg :str
    :reader .str)))

(defclass seq (regex)
  ((seq
    :initarg :seq
    :reader .seq)))

(defclass alternation (regex)
  ((choices
    :initarg :choices
    :reader .choices)))

(defclass repetition (regex)
  ((regex
    :initarg :regex
    :reader .regex)
   (greedyp
    :initarg :greedyp
    :reader .greedyp)
   (minimum
    :initarg :minimum
    :reader .minimum)
   (maximum
    :initarg :maximum
    :reader .maximum)))

(defclass register (regex)
  ((regex
    :initarg :regex
    :reader .regex)
   (number
    :initarg :number
    :reader .number)))

(defclass back-reference (regex)
  ((number
    :initarg :number
    :reader .number)))

(defclass random-char (regex)
  ((function
    :initarg :function
    :reader .function)))
