# regex-generator

## Usage
```
(ql:quickload :regex-generator)
(use-package :regex-generator)

(generate "0[6-9]0-\\d{4}-\\d{4}")
;; => "080-9844-1389"

(generate "(foo|bar)\\1")
;; => "foofoo"
```
