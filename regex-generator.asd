(defsystem "regex-generator"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "utils")
               (:file "regex-generator")))
