;; lang/perl.lisp

(in-package :colorize)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *version-token* (gensym)))

(defvar *perl-open-parens* "([{")
(defvar *perl-close-parens* ")]}")

(defvar *perl-reserved-words*
  '("my" "our" "use" "sub" "print" "no" "split" "chomp" "join"
    "map" "grep" "sort" "for" "while" "if" "unless" "do" "else" "elsif"
    "bless" "open" "close" "return" "or" "and"))

(defparameter *perl-begin-word* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
(defparameter *perl-terminators* '(#\space #\return #\tab #\newline #\. #\' #\" #\# #\, #\& #\= #\( #\) #\[ #\] #\{ #\} #\< #\> #\; #\- #\+ #\* #\/ #\\))

(define-coloring-type :perl "Perl"
  :default-mode :normal
  :transitions
  ((:normal
    ((or
      (scan-any *perl-open-parens*)
      (scan-any *perl-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((or (scan #\$)
	 (scan #\@)
	 (scan #\%))
     (set-mode :variable
	       :until (scan-any *perl-terminators*)
	       :advancing nil))
    ((scan-any *perl-begin-word*)
     (set-mode :word-ish
               :until (scan-any *perl-terminators*)
               :advancing nil))
    ((scan #\#)
     (set-mode :comment 
	       :until (scan #\newline)))
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
    ((or (scan "$")
	 (scan "@")
	 (scan "%"))
     (set-mode :variable
	       :until (scan-any *perl-terminators*)
	       :advancing nil))))
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
              (when (member (elt s 0) (coerce *perl-open-parens* 'list))
                (setf open t)
                (setf count (mod paren-counter 6))
                (incf paren-counter))
              (when (member (elt s 0) (coerce *perl-close-parens* 'list))
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
      (if (member s *perl-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>" s)
          s)))))
