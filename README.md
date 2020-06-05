# cl-string-generator

## Usage
```common-lisp
(ql:quickload :cl-string-generator)
(use-package :cl-string-generator)

(generate "0[6-9]0-\\d{4}-\\d{4}")
;; => "080-9844-1389"

(generate "(foo|bar)\\1")
;; => "foofoo"

(generate "a*b*" :max-length 3)
;; => "aab"

(generate "a*b*" :min-length 3 :max-length 4)
;; => "aaab"
```
