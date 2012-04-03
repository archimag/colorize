;; lang/refal.lisp

(in-package :colorize)

(defvar *refal-open-parens* "<([{")
(defvar *refal-close-parens* ">)]}")

(defvar *refal-reserved-words*
  '("$ENTRY" "$EXTRN"))

(defparameter *refal-begin-word* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-+*/0123456789")
(defparameter *refal-terminators* '(#\space #\return #\tab #\newline #\. #\' #\" #\# #\, #\& #\= #\: #\( #\) #\[ #\] #\{ #\} #\< #\> #\;))

(define-coloring-type :refal "Refal"
  :default-mode :normal
  :transitions
  ((:normal
    ((or
      (scan-any *refal-open-parens*)
      (scan-any *refal-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((or (scan "s.")
	 (scan "t.")
	 (scan "e."))
     (set-mode :variable
	       :until (scan-any *refal-terminators*)
	       :advancing nil))
    ((scan-any *refal-begin-word*)
     (set-mode :word-ish
               :until (scan-any *refal-terminators*)
               :advancing nil))
    ((scan "/*")
     (set-mode :comment
               :until (scan "*/")))
    ((or (scan #\=)
	 (scan #\:))
     (set-mode :matching
	       :until (advance 1)))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((scan #\')
     (set-mode :string
               :until (scan #\'))))
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
        (if (or (eql (length s) 1)
		(equalp s "&lt;")
		(equalp s "&gt;"))
            (progn
              (when (or (member (elt s 0) (coerce *refal-open-parens* 'list))
			(equalp s "&lt;"))
                (setf open t)
                (setf count (mod paren-counter 6))
                (incf paren-counter))
              (when (or (member (elt s 0) (coerce *refal-close-parens* 'list))
			(equalp s "&gt;"))
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
      (if (member s *refal-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>" s)
          s)))))
