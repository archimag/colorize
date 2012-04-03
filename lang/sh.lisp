;; lang/sh.lisp

(in-package :colorize)

(defvar *shell-open-parens* "([{`")
(defvar *shell-close-parens* ")]}`")

(defvar *shell-reserved-words*
  '("if" "then" "else" "fi" "case" "esac" "do" "done" "for" "while"
    "in" "read"))

(defparameter *shell-begin-word* "-+*/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
(defparameter *shell-terminators* '(#\space #\return #\tab #\newline #\' #\" #\# #\, #\& #\( #\) #\[ #\] #\{ #\} #\< #\> #\;))

(define-coloring-type :shell "Shell"
  :default-mode :normal
  :transitions
  ((:normal
    ((or
      (scan-any *shell-open-parens*)
      (scan-any *shell-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((scan #\$)
     (set-mode :variable
	       :until (scan-any *shell-terminators*)
	       :advancing nil))
    ((scan #\-)
     (set-mode :option
	       :until (scan-any *shell-terminators*)
	       :advancing nil))
    ((scan-any *shell-begin-word*)
     (set-mode :word-ish
               :until (scan-any *shell-terminators*)
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
    ((scan "$")
     (set-mode :variable
	       :until (scan-any *shell-terminators*)
	       :advancing nil)))
   (:word-ish
    ((scan #\=)
     (set-mode :normal))))
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
   (:option 
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"option\">~A</span>"
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
              (when (member (elt s 0) (coerce *shell-open-parens* 'list))
                (setf open t)
                (setf count (mod paren-counter 6))
                (incf paren-counter))
              (when (member (elt s 0) (coerce *shell-close-parens* 'list))
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
      (cond
	((member s *shell-reserved-words* :test #'string=)
	 (format nil "<span class=\"symbol\">~A</span>" s))
	((find #\= s)
	 (format nil "<span class=\"variable\">~A</span>="
		 (subseq s 0 (1- (length s)))))
	(t s))))))
