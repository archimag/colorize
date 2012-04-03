;; lang/php.lisp

(in-package :colorize)

(defvar *php-open-parens* "([{")
(defvar *php-close-parens* ")]}")
(defvar *php-open-tags* '("<?php" "<?"))
(defvar *php-close-tags* '("?>"))

(defvar *php-reserved-words*
  '("abstract" "and" "array" "as" "break" "case" "catch" "cfunction" "class"
    "clone" "const" "continue" "declare" "default" "do" "else" "elseif"
    "enddeclare" "endfor" "endforeach" "endif" "endswitch" "endwhile" "extends"
    "final" "for" "foreach" "function" "global" "goto" "if" "implements"
    "interface" "instanceof" "namespace" "new" "old_function" "or" "private"
    "protected" "public" "static" "switch" "throw" "try" "use" "var" "while" "xor"
    "die" "echo" "empty" "exit" "eval" "include" "include_once" "isset" "list"
    "require" "require_once" "return" "print" "unset" "__halt_compiler"))
(defvar *php-predefined-constants*
  '("PHP_VERSION" "PHP_MAJOR_VERSION" "PHP_MINOR_VERSION" "PHP_RELEASE_VERSION"
     "PHP_VERSION_ID" "PHP_EXTRA_VERSION" "PHP_ZTS" "PHP_DEBUG" "PHP_MAXPATHLEN"
     "PHP_OS" "PHP_SAPI" "PHP_EOL" "PHP_INT_MAX" "PHP_INT_SIZE"
     "DEFAULT_INCLUDE_PATH" "PEAR_INSTALL_DIR" "PEAR_EXTENSION_DIR"
     "PHP_EXTENSION_DIR" "PHP_PREFIX" "PHP_BINDIR" "PHP_BINARY" "PHP_MANDIR"
     "PHP_LIBDIR" "PHP_DATADIR" "PHP_SYSCONFDIR" "PHP_LOCALSTATEDIR"
     "PHP_CONFIG_FILE_PATH" "PHP_CONFIG_FILE_SCAN_DIR" "PHP_SHLIB_SUFFIX"
     "PHP_OUTPUT_HANDLER_START" "PHP_OUTPUT_HANDLER_CONT" "PHP_OUTPUT_HANDLER_END"
     "E_ERROR" "E_WARNING" "E_PARSE" "E_NOTICE" "E_CORE_ERROR" "E_CORE_WARNING"
     "E_COMPILE_ERROR" "E_COMPILE_WARNING" "E_USER_ERROR" "E_USER_WARNING"
     "E_USER_NOTICE" "E_DEPRECATED" "E_USER_DEPRECATED" "E_ALL" "E_STRICT" "__COMPILER_HALT_OFFSET__"
     "__CLASS__" "__DIR__" "__FILE__" "__LINE__" "__FUNCTION__" "__METHOD__" "__NAMESPACE__"))

(defparameter *php-begin-word* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
(defparameter *php-terminators* '(#\space #\return #\tab #\newline #\. #\' #\" #\# #\, #\& #\= #\( #\) #\[ #\] #\{ #\} #\< #\> #\; #\- #\+ #\* #\/ #\\ #\:))

(define-coloring-type :php "Php"
  :default-mode :normal
  :transitions
  ((:normal
    ((scan-any *php-open-tags*)
      (set-mode :php
                :until (scan-any *php-close-tags*))))
   (:php
    ((or
      (scan-any *php-open-parens*)
      (scan-any *php-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    #+nil
    ((scan "case ")
     (set-mode :paren-ish
               :until (scan "break;")))
    ((scan "/*") ; multiline comments
     (set-mode :comment
               :until (scan "*/")))
    ((scan-any '(#\/ #\#)) ; 1-line comments
     (set-mode :comment
               :until (scan-any '(#\return #\newline))))
    ((scan #\$) ; variables
     (set-mode :variable
	       :until (scan-any *php-terminators*)
	       :advancing nil))
    ((scan-any *php-begin-word*)
     (set-mode :word-ish
               :until (scan-any *php-terminators*)
               :advancing nil))
    ((scan #\")
     (set-mode :string-2
               :until (scan #\")))
    ((scan #\')
     (set-mode :string
               :until (scan #\'))))
   (:string
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1))))
   (:string-2
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))
    ((scan #\$) ; variables in strings
     (set-mode :variable
	       :until (scan-any *php-terminators*)
	       :advancing nil))))
  :formatter-variables
  ((paren-counter 0))
  :formatter-after-hook (lambda nil
                          (format nil "~{~A~}"
                                  (loop for i from paren-counter downto 1
                                     collect "</span></span>")))
  :formatters
  ((:php
    (lambda (type s)
      (declare (ignore type))
              s))
   (:normal
    (lambda (type s)
      (declare (ignore type))
      s))
   (:matching
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
   (:string-2
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   (:variable 
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"variable\">~A</span>"
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
              (when (member (elt s 0) (coerce *php-open-parens* 'list))
                (setf open t)
                (setf count (mod paren-counter 6))
                (incf paren-counter))
              (when (member (elt s 0) (coerce *php-close-parens* 'list))
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
      (if (member s (append *php-reserved-words* *php-predefined-constants*) :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>" s)
          s)))))
