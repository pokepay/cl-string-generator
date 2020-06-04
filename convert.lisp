(in-package :regex-generator)

(defvar *register-number*)

(defgeneric convert-simple-parse-tree (parse-tree)
  (:method ((parse-tree character))
    (make-instance 'str :str (string parse-tree)))
  (:method ((parse-tree string))
    (make-instance 'str :str parse-tree))
  (:method ((parse-tree (eql :void)))
    (make-instance 'str :str ""))
  (:method ((parse-tree (eql :word-boundary))))
  (:method ((parse-tree (eql :non-word-boundary))))
  (:method ((parse-tree (eql :everything)))
    (make-instance 'random-char :function #'random-char))
  (:method ((parse-tree (eql :digit-class)))
    (make-instance 'random-char :function #'random-digit-char))
  (:method ((parse-tree (eql :word-char-class)))
    (make-instance 'random-char :function #'random-word-char))
  (:method ((parse-tree (eql :whitespace-char-class)))
    (make-instance 'random-char :function #'random-whitespace-char))
  (:method ((parse-tree (eql :non-digit-class)))
    (make-instance 'random-char :function #'random-non-digit-char))
  (:method ((parse-tree (eql :non-word-char-class)))
    (make-instance 'random-char :function #'random-non-word-char))
  (:method ((parse-tree (eql :non-whitespace-char-class)))
    (make-instance 'random-char :function #'random-non-whitespace-char))
  (:method ((parse-tree (eql :start-anchor))))
  (:method ((parse-tree (eql :end-anchor))))
  (:method ((parse-tree (eql :modeless-start-anchor))))
  (:method ((parse-tree (eql :modeless-end-anchor))))
  (:method ((parse-tree (eql :modeless-end-anchor-no-newline))))
  (:method ((parse-tree (eql :case-insensitive-p))))
  (:method ((parse-tree (eql :case-sensitive-p))))
  (:method ((parse-tree (eql :multi-line-mode-p))))
  (:method ((parse-tree (eql :not-multi-line-mode-p))))
  (:method ((parse-tree (eql :single-line-mode-p))))
  (:method ((parse-tree (eql :not-single-line-mode-p)))))

(defun convert-sequence (arguments)
  (make-instance 'seq
                 :seq (loop :for parse-tree :in arguments
                            :collect (convert-aux parse-tree))))

(defun random-char-function (item)
  (cond ((characterp item)
         (make-instance 'str :str (string item)))
        ((symbolp item)
         (ecase item
           ((:digit-class)
            (make-instance 'random-char :function #'random-digit-char))
           ((:non-digit-class)
            (make-instance 'random-char :function #'random-non-digit-char))
           ((:whitespace-char-class)
            (make-instance 'random-char :function #'random-whitespace-char))
           ((:non-whitespace-char-class)
            (make-instance 'random-char :function #'random-non-whitespace-char))
           ((:word-char-class)
            (make-instance 'random-char :function #'random-word-char))
           ((:non-word-char-class)
            (make-instance 'random-char :function #'random-non-word-char))))
        ((and (consp item)
              (eq (first item) :property))
         (error "unsupported inverted-property"))
        ((and (consp item)
              (eq (first item) :inverted-property))
         (error "unsupported inverted-property"))
        ((and (consp item)
              (eq (first item) :range))
         (destructuring-bind (min max) (rest item)
           (lambda ()
             (code-char (random-integer (char-code min) (char-code max))))))
        (t (error "Unknown item ~A in char-class list." item))))

(defgeneric convert-compound-parse-tree (token parse-tree)
  (:method ((token (eql :sequence)) parse-tree)
    (convert-sequence (rest parse-tree)))
  (:method ((token (eql :alternation)) parse-tree)
    (make-instance 'alternation
                   :choices (loop :for parse-tree :in (rest parse-tree)
                                  :collect (convert-aux parse-tree))))
  (:method ((token (eql :group)) parse-tree)
    (if (cddr parse-tree)
        (convert-sequence (rest parse-tree))
        (convert-aux (second parse-tree))))
  (:method ((token (eql :char-class)) parse-tree)
    (let* ((item (random-choice (rest parse-tree)))
           (function (random-char-function item)))
      (make-instance 'random-char :function function)))
  (:method ((token (eql :greedy-repetition)) parse-tree)
    (destructuring-bind (min max regex) (rest parse-tree)
      (make-instance 'repetition
                     :regex (convert-aux regex)
                     :greedyp t
                     :minimum min
                     :maximum (or max +max-repetition+))))
  (:method ((token (eql :register)) parse-tree)
    (destructuring-bind (regex) (rest parse-tree)
      (make-instance 'register
                     :regex (convert-aux regex)
                     :number (prog1 *register-number*
                               (incf *register-number*)))))
  (:method ((token (eql :back-reference)) parse-tree)
    (destructuring-bind (number) (rest parse-tree)
      (make-instance 'back-reference :number number))))

(defun convert-aux (parse-tree)
  (let ((result
          (if (consp parse-tree)
              (convert-compound-parse-tree (first parse-tree) parse-tree)
              (convert-simple-parse-tree parse-tree))))
    result))

(defun convert (regex)
  (let ((parse-tree (typecase regex
                      (string
                       (ppcre:parse-string regex))
                      (otherwise
                       regex)))
        (*register-number* 1))
    (convert-aux parse-tree)))