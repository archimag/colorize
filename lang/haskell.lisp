;; lang/haskell.lisp

(in-package :colorize)

(defvar *haskell-open-parens* "([{")
(defvar *haskell-close-parens* ")]}")

(defvar *haskell-in-word*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")

(defvar *haskell-begin-id* "abcdefghijklmnopqrstuvwxyz")

(defvar *haskell-begin-cons* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defvar *haskell-in-symbol* "!#$%&*+./<=>?@\\^|-~:")

(defvar *haskell-reserved-symbols*
  '(".." "::" "@" "~" "=" "-&gt;" "&lt;-" "|" "\\"))

(defvar *haskell-reserved-words*
  '("case" "class" "data" "default" "deriving" "do" "else" "if"
    "import" "in" "infix" "infixl" "infixr" "instance" "let" "module"
    "newtype" "of" "then" "type" "where"))

(defvar *haskell-non-constituent*
  '(#\space #\return #\tab #\newline #\{ #\} #\( #\) #\" #\[ #\]))

(define-coloring-type :haskell "Haskell"
  :default-mode :normal
  :transitions
  (((:normal)
    ((scan-any *haskell-in-word*)
     (set-mode :identifier
	       :until (or (scan-any *haskell-non-constituent*)
			  (scan-any *haskell-in-symbol*))
	       :advancing nil))
    ((scan "--")
     (set-mode :comment
	       :until (scan-any '(#\return #\newline))
	       :advancing nil))
    ((scan "{-")
     (set-mode :multi-comment
	       :until (scan "-}")))
    ((scan #\")
     (set-mode :string
	       :until (scan #\")))
    ((scan #\`)
     (set-mode :backquote
	       :until (scan #\`)))
    ((scan "'")
     (set-mode :char
	       :until (scan #\')))
    ((scan-any *haskell-in-symbol*)
     (set-mode :symbol
	       :until (or (scan-any *haskell-non-constituent*)
			  (scan-any *haskell-in-word*)
			  (scan #\'))
	       :advancing nil))
    ((or (scan-any *haskell-open-parens*)
	 (scan-any *haskell-close-parens*))
     (set-mode :parenlike
	       :until (advance 1)
	       :advancing nil))
    ((scan #\newline)
     (set-mode :newline
	       :until (advance 1)
	       :advancing nil)))
   ((:string)
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1))))
   ((:char)
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))))
  :formatter-variables
  ((paren-counter 0)
   (beginning-of-line t))
  :formatter-after-hook (lambda nil
			  (format nil "~{~A~}"
				  (loop for i from paren-counter downto 1
                                     collect "</span></span>")))
  :formatters
  (((:normal)
    (lambda (type s)
      (declare (ignore type))
      (cond (beginning-of-line
	     (setq beginning-of-line nil)
	     (if (char= (elt s 0) #\space)
		 (concatenate 'string "&nbsp;" (subseq s 1))
                 s))
	    (t s))))
   ((:newline)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line t)
      s))
   ((:backquote)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (if (find (elt s 1) *haskell-begin-cons*)
	  (format nil "<span class=\"variable\">~A</span>"
		  s)
          (format nil "<span class=\"atom\">~A</span>"
                  s))))
   ((:comment :multi-comment)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (format nil "<span class=\"comment\">~A</span>"
	      s)))
   ((:string)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (format nil "<span class=\"string\">~A</span>"
	      s)))
   ((:char)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (format nil "<span class=\"character\">~A</span>"
	      s)))
   ((:identifier)
    (lambda (type s)
      (declare (ignore type))
      (prog1
	  (cond ((find (elt s 0) *haskell-begin-cons*)
		 (format nil "<span class=\"variable\">~A</span>" s))
		((member s *haskell-reserved-words* :test #'string=)
		 (format nil "<span class=\"keyword\">~A</span>" s))
		(beginning-of-line
		 (format nil "<span class=\"function\">~A</span>" s))
		(t s))
	(setq beginning-of-line nil))))
   ((:symbol)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (cond ((member s *haskell-reserved-symbols* :test #'string=)
	     (format nil "<span class=\"keyword\">~A</span>" s))
	    ((char= (elt s 0) #\:)
	     (format nil "<span class=\"variable\">~A</span>" s))
	    (t (format nil "<span class=\"atom\">~A</span>" s)))))
   ((:single-escape)
    (lambda (type s)
      (call-formatter (cdr type) s)))
   ((:parenlike)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (let ((open nil)
            (count 0))
        (if (eql (length s) 1)
            (progn
              (when (find (elt s 0) *haskell-open-parens*)
                (setf open t)
                (setf count (mod paren-counter 6))
                (incf paren-counter))
              (when (find (elt s 0) *haskell-close-parens*)
                (setf open nil)
                (decf paren-counter)
                (setf count (mod paren-counter 6)))
              (if open
                  (format nil "<span class=\"paren~A\">~A<span class=\"~A\">"
                          (1+ count) s *css-background-class*)
                  (format nil "</span>~A</span>"
                          s)))
            s))))))
