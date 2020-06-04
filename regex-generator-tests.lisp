(defpackage #:regex-generator-tests
  (:use :cl :regex-generator :rove :alexandria))
(in-package #:regex-generator-tests)

(defun ensure-char (string)
  (assert (and (stringp string)
               (= 1 (length string))))
  (char string 0))

(defun simple-test (regex test expected-num)
  (loop :with table := (make-array 128 :initial-element nil)
        :repeat 1000
        :for string := (generate regex)
        :always (and (stringp string) (= 1 (length string)) (funcall test (char string 0)))
        :do (setf (aref table (char-code (char string 0))) t)
        :finally (return (= expected-num (count t table)))))

(deftest simple-regex-tests
  (ok (string= "" (generate "")))
  (ok (string= "a" (generate #\a)))
  (ok (string= "a" (generate "a")))
  (ok (string= "abc" (generate "abc")))
  (ok (simple-test "." #'graphic-char-p 95))
  (ok (simple-test "\\d" #'digit-char-p 10))
  (ok (simple-test "\\w" #'ppcre::word-char-p 63))
  (ok (simple-test "\\s" #'ppcre::whitespacep 5))
  (ok (simple-test "\\D" (complement #'digit-char-p) 118))
  (ok (simple-test "\\S" (complement #'ppcre::whitespacep) 123))
  (ok (simple-test "\\W" (complement #'ppcre::word-char-p) 65)))

(deftest complex-regex-tests
  (ok (string= "ab" (generate '(:sequence "a" "b"))))
  (ok (loop :with chars := '()
            :repeat 100
            :for c := (ensure-char (generate "a|b"))
            :do (pushnew c chars)
            :finally (return (length= chars 2))))
  (ok (loop :with chars := '()
            :repeat 100
            :for c := (ensure-char (generate "a|b|c"))
            :do (pushnew c chars)
            :finally (return (length= chars 3))))
  (ok (string= "ab" (generate "(?:ab)")))
  (ok (string= "abcd" (generate '(:group "ab" "cd"))))
  (ok (loop :with chars := '()
            :repeat 100
            :for c := (ensure-char (generate "[a-c]"))
            :do (pushnew c chars)
            :finally (return (length= chars 3))))
  (ok (loop :with chars := '()
            :repeat 100
            :for c := (ensure-char (generate "[a-c0-2_]"))
            :do (pushnew c chars)
            :finally (return (length= chars 7))))
  (ok (loop :with chars := '()
            :repeat 100
            :for c := (ensure-char (generate "[abc]"))
            :do (pushnew c chars)
            :finally (return (length= chars 3))))
  (ok (loop :repeat 10 :always (ppcre:scan "^a*$" (generate "a*"))))
  (ok (loop :repeat 100 :always (ppcre:scan "^([0-9]*)\\1$" (generate "([0-9]*)\\1")))))

(deftest max-length-tests
  (ok (loop :repeat 100
            :always (<= (length (generate "a*b*" :max-length 3)) 3))))
