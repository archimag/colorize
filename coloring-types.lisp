;; coloring-types.lisp

(in-package :colorize)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *version-token* (gensym)))

(defparameter *symbol-characters*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ*!%$&+-1234567890<=>")

(defparameter *non-constituent*
  '(#\space #\tab #\newline #\linefeed #\page #\return
    #\" #\' #\( #\) #\, #\; #\` #\[ #\]))

(defparameter *special-forms*
  '("let" "load-time-value" "quote" "macrolet" "progn" "progv" "go" "flet" "the"
    "if" "throw" "eval-when" "multiple-value-prog1" "unwind-protect" "let*"
    "labels" "function" "symbol-macrolet" "block" "tagbody" "catch" "locally"
    "return-from" "setq" "multiple-value-call"))

(defparameter *common-macros*
  '("loop" "cond" "lambda"))

(defparameter *open-parens* '(#\())
(defparameter *close-parens* '(#\)))

(define-coloring-type :lisp "Basic Lisp"
  :default-mode :first-char-on-line
  :invisible t
  :transitions
  (((:in-list)
    ((or
      (scan-any *symbol-characters*)
      (and (scan #\.) (scan-any *symbol-characters*))
      (and (scan #\\) (advance 1)))
     (set-mode :symbol
               :until (scan-any *non-constituent*)
               :advancing nil))
    ((or (scan #\:) (scan "#:"))
     (set-mode :keyword
               :until (scan-any *non-constituent*)
               :advancing nil))
    ((scan "#\\")
     (let ((count 0))
       (set-mode :character
                 :until (progn
                          (incf count)
                          (if (> count 1)
                              (scan-any *non-constituent*)))
                 :advancing nil)))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((scan #\;)
     (set-mode :comment
               :until (scan #\newline)))
    ((scan "#|")
     (set-mode :multiline
               :until (scan "|#")))
    ((scan #\()
     (set-mode :in-list
               :until (scan #\)))))
   ((:normal :first-char-on-line)
    ((scan #\()
     (set-mode :in-list
               :until (scan #\)))))
   (:first-char-on-line
    ((scan #\;)
     (set-mode :comment
               :until (scan #\newline)))
    ((scan "#|")
     (set-mode :multiline
               :until (scan "|#")))
    ((advance 1)
     (set-mode :normal
               :until (scan #\newline))))
   (:multiline
    ((scan "#|")
     (set-mode :multiline
               :until (scan "|#"))))
   ((:symbol :keyword :escaped-symbol :string)
    ((scan #\\)
     (let ((count 0))
       (set-mode :single-escaped
                 :until (progn
                          (incf count)
                          (if (< count 2)
                              (advance 1))))))))
  :formatter-variables ((paren-counter 0))
  :formatter-after-hook (lambda nil
                          (format nil "~{~A~}"
                                  (loop for i from paren-counter downto 1
                                        collect "</span></span>")))
  :formatters
  (((:normal :first-char-on-line)
    (lambda (type s)
      (declare (ignore type))
      s))
   ((:in-list)
    (lambda (type s)
      (declare (ignore type))
      (labels ((color-parens (s)
                 (let ((paren-pos (find-if-not #'null
                                               (mapcar #'(lambda (c)
                                                           (position c s))
                                                       (append *open-parens*
                                                               *close-parens*)))))
                   (if paren-pos
                       (let ((before-paren (subseq s 0 paren-pos))
                             (after-paren (subseq s (1+ paren-pos)))
                             (paren (elt s paren-pos))
                             (open nil)
                             (count 0))
                         (when (member paren *open-parens* :test #'char=)
                           (setf count (mod paren-counter 6))
                           (incf paren-counter)
                           (setf open t))
                         (when (member paren *close-parens* :test #'char=)
                           (decf paren-counter))
                         (if open
                             (format nil "~A<span class=\"paren~A\">~C<span class=\"~A\">~A"
                                     before-paren
                                     (1+ count)
                                     paren *css-background-class*
                                     (color-parens after-paren))
                             (format nil "~A</span>~C</span>~A"
                                     before-paren
                                     paren (color-parens after-paren))))
                       s))))
        (color-parens s))))
   ((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type))
      (let* ((colon (position #\: s :from-end t))
             (new-s (or (and colon (subseq s (1+ colon))) s)))
        (cond
          ((or
            (member new-s *common-macros* :test #'string-equal)
            (member new-s *special-forms* :test #'string-equal)
            (some #'(lambda (e)
                      (and (> (length new-s) (length e))
                           (string-equal e (subseq new-s 0 (length e)))))
                  '("WITH-" "DEF")))
           (format nil "<i><span class=\"symbol\">~A</span></i>" s))
          ((and (> (length new-s) 2)
                (char= (elt new-s 0) #\*)
                (char= (elt new-s (1- (length new-s))) #\*))
           (format nil "<span class=\"special\">~A</span>" s))
          (t s)))))
   (:keyword (lambda (type s)
      (declare (ignore type))
               (format nil "<span class=\"keyword\">~A</span>"
                       s)))
   ((:comment :multiline)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"comment\">~A</span>"
              s)))
   ((:character)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"character\">~A</span>"
              s)))
   ((:string)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   ((:single-escaped)
    (lambda (type s)
      (call-formatter (cdr type) s)))
   ((:syntax-error)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"syntaxerror\">~A</span>"
              s)))))

(define-coloring-type :scheme "Scheme"
  :parent :lisp
  :transitions
  (((:normal :in-list)
    ((scan "...")
     (set-mode :symbol
               :until (scan-any *non-constituent*)
               :advancing nil))
    ((scan #\[)
     (set-mode :in-list
               :until (scan #\])))))
  :formatters
  (((:in-list)
    (lambda (type s)
      (declare (ignore type s))
      (let ((*open-parens* (cons #\[ *open-parens*))
            (*close-parens* (cons #\] *close-parens*)))
        (call-parent-formatter))))
   ((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type))
      (let ((result (if (find-package :r5rs-lookup)
                         (funcall (symbol-function (intern "SYMBOL-LOOKUP" :r5rs-lookup))
                                  s))))
        (if result
            (format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
                    result (call-parent-formatter))
            (call-parent-formatter)))))))

(define-coloring-type :elisp "Emacs Lisp"
  :parent :lisp
  :formatters
  (((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type))
      (let ((result (if (find-package :elisp-lookup)
                         (funcall (symbol-function (intern "SYMBOL-LOOKUP" :elisp-lookup))
                                  s))))
        (if result
            (format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
                    result (call-parent-formatter))
            (call-parent-formatter)))))))

(define-coloring-type :common-lisp "Common Lisp"
  :parent :lisp
  :transitions
  (((:normal :in-list)
    ((scan #\|)
     (set-mode :escaped-symbol
               :until (scan #\|)))))
  :formatters 
  (((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type))
      (let* ((colon (position #\: s :from-end t :test #'char=))
             (to-lookup (if colon (subseq s (1+ colon)) s))
             (result (if (find-package :clhs-lookup)
                         (funcall (symbol-function (intern "SYMBOL-LOOKUP" :clhs-lookup))
                                  to-lookup))))
        (if result
            (format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
                    result (call-parent-formatter))
            (call-parent-formatter)))))))

(define-coloring-type :common-lisp-file "Common Lisp File"
  :parent :common-lisp
  :default-mode :in-list
  :invisible t)

(defvar *c-open-parens* "([{")
(defvar *c-close-parens* ")]}")

(defvar *c-reserved-words*
  '("auto"   "break"  "case"   "char"   "const"
    "continue" "default" "do"     "double" "else"
    "enum"   "extern" "float"  "for"    "goto"
    "if"     "int"    "long"   "register" "return"
    "short"  "signed" "sizeof" "static" "struct"
    "switch" "typedef" "union"  "unsigned" "void"
    "volatile" "while"  "__restrict" "_Bool"))

(defparameter *c-begin-word* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
(defparameter *c-terminators* '(#\space #\return #\tab #\newline #\. #\/ #\- #\* #\+ #\{ #\} #\( #\) #\' #\" #\[ #\] #\< #\> #\#))

(define-coloring-type :basic-c "Basic C"
  :default-mode :normal
  :invisible t
  :transitions
  ((:normal
    ((scan-any *c-begin-word*)
     (set-mode :word-ish
               :until (scan-any *c-terminators*)
               :advancing nil))
    ((scan "/*")
     (set-mode :comment
               :until (scan "*/")))
    
    ((or
      (scan-any *c-open-parens*)
      (scan-any *c-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((or (scan "'\\")
         (scan #\'))
     (set-mode :character
               :until (advance 2))))
   (:string
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))))
  :formatter-variables
  ((paren-counter 0))
  :formatter-after-hook (lambda nil
                          (format nil "~{~A~}"
                                  (loop for i from paren-counter downto 1
                                        collect "</span></span>")))
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
   (:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *c-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>" s)
          s)))
   ))

(define-coloring-type :c "C"
  :parent :basic-c
  :transitions
  ((:normal
    ((scan #\#)
     (set-mode :preprocessor
               :until (scan-any '(#\return #\newline))))))
  :formatters
  ((:preprocessor
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"special\">~A</span>" s)))))

(defvar *c++-reserved-words*
  '("asm"          "auto"      "bool"     "break"            "case"
    "catch"        "char"      "class"   "const"            "const_cast"
    "continue"     "default"   "delete"   "do"               "double"
    "dynamic_cast" "else"      "enum"     "explicit"         "export"
    "extern"       "false"     "float"    "for"              "friend"
    "goto"         "if"        "inline"   "int"              "long"
    "mutable"      "namespace" "new"      "operator"         "private"
    "protected"    "public"    "register" "reinterpret_cast" "return"
    "short"        "signed"    "sizeof"   "static"           "static_cast"
    "struct"       "switch"    "template" "this"             "throw"
    "true"         "try"       "typedef"  "typeid"           "typename"
    "union"        "unsigned"  "using"    "virtual"          "void"
    "volatile"     "wchar_t"   "while"))

(define-coloring-type :c++ "C++"
  :parent :c
  :transitions
  ((:normal
    ((scan "//")
     (set-mode :comment
               :until (scan-any '(#\return #\newline))))))
  :formatters
  ((:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *c++-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>"
                  s)
          s)))))

(defvar *java-reserved-words*
  '("abstract"     "boolean"      "break"    "byte"         "case" 
    "catch"        "char"         "class"   "const"        "continue" 
    "default"      "do"           "double"   "else"         "extends" 
    "final"        "finally"      "float"    "for"          "goto" 
    "if"           "implements"   "import"   "instanceof"   "int" 
    "interface"    "long"         "native"   "new"          "package" 
    "private"      "protected"    "public"   "return"       "short" 
    "static"       "strictfp"     "super"    "switch"       "synchronized" 
    "this"         "throw"        "throws"   "transient"    "try" 
    "void"         "volatile"     "while"))

(define-coloring-type :java "Java"
  :parent :c++
  :formatters
  ((:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *java-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>"
                  s)
          s)))))

(let ((terminate-next nil))
  (define-coloring-type :objective-c "Objective C"
    :transitions
    ((:normal
      ((scan #\[)
       (set-mode :begin-message-send
		 :until (advance 1)
		 :advancing nil))
      ((scan #\])
       (set-mode :end-message-send
		 :until (advance 1)
		 :advancing nil))
      ((scan-any *c-begin-word*)
       (set-mode :word-ish
		 :until (or
			 (and (peek-any '(#\:))
			      (setf terminate-next t))
			 (and terminate-next (progn
					       (setf terminate-next nil)
					       (advance 1)))
			 (scan-any *c-terminators*))
		 :advancing nil)))
     (:word-ish
      #+nil
      ((scan #\:)
       (format t "hi~%")
       (set-mode :word-ish :until (advance 1) :advancing nil)
       (setf terminate-next t))))
  :parent :c++
  :formatter-variables ((is-keyword nil) (in-message-send nil))
  :formatters
  ((:begin-message-send
    (lambda (type s)
      (setf is-keyword nil)
      (setf in-message-send t)
      (call-formatter (cons :paren-ish type) s)))
   (:end-message-send
    (lambda (type s)
      (setf is-keyword nil)
      (setf in-message-send nil)
      (call-formatter (cons :paren-ish type) s)))
   (:word-ish
    (lambda (type s)
      (declare (ignore type))
      (prog1
	  (let ((result (if (find-package :cocoa-lookup)
			    (funcall (symbol-function (intern "SYMBOL-LOOKUP" :cocoa-lookup))
				     s))))
	    (if result
		(format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
			result s)
		(if (member s *c-reserved-words* :test #'string=)
		    (format nil "<span class=\"symbol\">~A</span>" s)
		    (if in-message-send
			(if is-keyword
			    (format nil "<span class=\"keyword\">~A</span>" s)
			    s)
			s))))
	(setf is-keyword (not is-keyword))))))))

#+nil
(defparameter *numbers* "0123456789")
#+nil
(define-coloring-type :webkit "WebKit (text or diff)"
  :parent :diff
  :transitions
  (((:file-from)
    ((scan "(revision ")
     (set-mode :revision
	       :until (scan #\)))))
   ((:revision)
    ((scan-any *numbers*)
     (set-mode :revision-number)))
   ((:revision :revision-number)
    ((scan #\newline)
     (set-mode :first-char-on-line)))
   ((:revision-number)
    ((scan #\))
     (set-mode :revision)))
   ((:diff-added)
    ((scan "Reviewed by NOBODY (OOPS!)")
     (set-mode :oops
	       :until (advance 1)
	       :advancing nil))))
  :formatters
  (((:revision)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>" s)))
   ((:revision-number)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<a href=\"http://trac.webkit.org/changeset/~A\" class=\"diff-link\">~:*~A</a>" s)))
   ((:index-file)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<a href=\"http://trac.webkit.org/browser/trunk/~A\" class=\"diff-link\">~:*~A</a>"
	      s)))
   ((:git-index-file)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<a href=\"http://trac.webkit.org/browser/trunk/~A\" class=\"diff-link\">~A</a>"
	      (if (> (length s) 2)
		  (subseq s 2)
		  s)
	      s)))
   ((:oops)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"syntaxerror\">~A</span>" s)))))
