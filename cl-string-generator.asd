(defsystem "cl-string-generator"
  :depends-on ("alexandria" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "utils")
               (:file "regex-class")
               (:file "convert")
               (:file "generate"))
  :in-order-to ((test-op (test-op "cl-string-generator/tests"))))

(defsystem "cl-string-generator/tests"
  :depends-on ("cl-string-generator" "rove")
  :components ((:file "cl-string-generator-tests"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
