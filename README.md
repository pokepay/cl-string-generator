# cl-string-generator

## Usage
```
(ql:quickload :cl-string-generator)
(use-package :cl-string-generator)

(generate "0[6-9]0-\\d{4}-\\d{4}")
;; => "080-9844-1389"

(generate "(foo|bar)\\1")
;; => "foofoo"
```
