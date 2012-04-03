;; lang/python.lisp

(in-package :colorize)

(defvar *python-reserved-words*
  '("and"       "assert"        "break"         "class"         "continue"
    "def"       "del"           "elif"          "else"          "except"
    "exec"      "finally"       "for"           "from"          "global"
    "if"        "import"        "in"            "is"            "lambda"
    "not"       "or"            "pass"          "print"         "raise"
    "return"    "try"           "while"         "yield"))

(define-coloring-type :python "Python"
  :default-mode :normal
  :transitions
  ((:normal
    ((scan-any *c-begin-word*)
     (set-mode :word-ish
               :until (scan-any *c-terminators*)
               :advancing nil))
    ((or
      (scan-any *c-open-parens*)
      (scan-any *c-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil)) 
    ((scan #\#)
     (set-mode :comment
	       :until (scan-any '(#\return #\newline))))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((scan "\"\"\"")
     (set-mode :string
	       :until (scan "\"\"\"")))
    ((scan "'''")
     (set-mode :string
	       :until (scan "'''")))
    ((scan #\')
     (set-mode :string
	       :until (scan #\')))
    ((scan "@")
     (set-mode :decorator
	       :until (scan-any *non-constituent*)
	       :advancing nil))
    ((scan "def")
     (set-mode :def
	       :until (scan-any '(#\: #\())
	       :advancing nil))
    ((scan "class")
     (set-mode :def
	       :until (scan-any '(#\: #\())
	       :advancing nil)))
   (:string
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))))
  :formatter-variables ((paren-counter 0))
  :formatters
  ((:normal
    (lambda (type s)
      (declare (ignore type))
      s))
   (:comment
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"comment\">~A</span>"
              s)))
   (:string
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   (:character
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"character\">~A</span>"
              s)))
   (:single-escape
    (lambda (type s)
      (call-formatter (cdr type) s)))
   (:paren-ish
    (lambda (type s)
      (declare (ignore type))
      (let ((open nil)
            (count 0))
        (if (eql (length s) 1)
            (progn
              (when (member (elt s 0) (coerce *c-open-parens* 'list))
                (setf open t)
                (setf count (mod paren-counter 6))
                (incf paren-counter))
              (when (member (elt s 0) (coerce *c-close-parens* 'list))
                (setf open nil)
                (decf paren-counter)
                (setf count (mod paren-counter 6)))
              (if open
                  (format nil "<span class=\"paren~A\">~A<span class=\"~A\">"
                          (1+ count) s *css-background-class*)
                  (format nil "</span>~A</span>"
                          s)))
            s))))
   (:def
       (lambda (type s)
	 (declare (ignore type))
	 (format nil "<span class=\"special\">~A</span><span
class=\"keyword\">~A</span>"
		 (subseq s 0 (position #\Space s))
		 (subseq s (position #\Space s)))))
   (:decorator
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"symbol\">~A</span>" s)))
   (:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *python-reserved-words* :test #'string=)
	  (format nil "<span class=\"symbol\">~A</span>"
		  s)
	  s)))))
