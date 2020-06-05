# cl-string-generator

Generate string from regular expression

## Usage
```common-lisp
(ql:quickload :cl-string-generator)
(use-package :cl-string-generator)

(generate "[0-9A-F]{8}-(?:[0-9A-F]{4}-){3}[0-9A-F]{8}")
;; => "A64BE7F3-1041-6C90-D8EB-2A0F46A8"

(generate "(foo|bar)\\1")
;; => "foofoo"

(generate "a*b*" :max-length 3)
;; => "aab"

(generate "a*b*" :min-length 3 :max-length 4)
;; => "aaab"
```

## License
MIT
