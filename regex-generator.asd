(defsystem "regex-generator"
  :depends-on ("alexandria" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "utils")
               (:file "specials")
               (:file "regex-class")
               (:file "convert")
               (:file "generate"))
  :in-order-to ((test-op (test-op "regex-generator/tests"))))

(defsystem "regex-generator/tests"
  :depends-on ("regex-generator" "rove")
  :components ((:file "regex-generator-tests"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
