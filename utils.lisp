(in-package :regex-generator)

(defun word-char-p (c)
  (or (alphanumericp c)
      (char= c #\_)))

(defun random-choice (sequence)
  (elt sequence (random (length sequence))))

(defun random-integer (min max)
  (+ min (random (1+ (- max min)))))

(defun random-expected-char (test-function)
  (loop :for char := (code-char (random 128))
        :until (funcall test-function char)
        :finally (return char)))

(defun random-char ()
  (random-expected-char #'graphic-char-p))

(defun random-digit-char ()
  (digit-char (random-integer 0 9)))

(defun random-non-digit-char ()
  (random-expected-char (complement #'digit-char-p)))

(defun random-word-char ()
  (random-expected-char #'word-char-p))

(defun random-non-word-char ()
  (random-expected-char (complement #'word-char-p)))

(defun random-whitespace-char ()
  (random-choice ppcre::+whitespace-char-string+))

(defun random-non-whitespace-char ()
  (random-expected-char (complement #'ppcre::whitespacep)))
