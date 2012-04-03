;; lang/erlang.lisp

(in-package :colorize)

(defvar *erlang-open-parens* "([{")
(defvar *erlang-close-parens* ")]}")

(defvar *erlang-reserved-words*
  '("after" "andalso" "begin" "catch" "case" "end" "fun" "if" "of" "orelse"
    "receive" "try" "when" "query" "is_atom" "is_binary" "is_constant"
    "is_float" "is_function" "is_integer" "is_list" "is_number" "is_pid"
    "is_port" "is_reference" "is_tuple" "is_record" "abs" "element" "float"
    "hd" "tl" "length" "node" "round" "self" "size" "trunc" "alive" "apply"
    "atom_to_list" "binary_to_list" "binary_to_term" "concat_binary"
    "date" "disconnect_node" "erase" "exit" "float_to_list" "garbage_collect"
    "get" "get_keys" "group_leader" "halt" "integer_to_list" "internal_bif"
    "link" "list_to_atom" "list_to_binary" "list_to_float" "list_to_integer"
    "make_ref" "node_link" "node_unlink" "notalive" "open_port" "pid_to_list"
    "process_flag" "process_info" "processes" "put" "register" "registered"
    "setelement" "spawn" "spawn_link" "split_binary" "statistics"
    "term_to_binary" "time" "throw" "trace" "trunc" "tuple_to_list"
    "unlink" "unregister" "whereis"))

(defparameter *erlang-begin-word* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
(defparameter *erlang-begin-fun* "abcdefghijklmnopqrstuvwxyz")
(defparameter *erlang-begin-var* "ABCDEFGHIJKLMNOPQRSTUVWXYZ_")
(defparameter *erlang-terminators* '(#\space #\return #\tab #\newline #\. #\; #\, #\/ #\- #\* #\+ #\( #\) #\' #\" #\[ #\] #\< #\> #\{ #\}))

(define-coloring-type :erlang "Erlang"
  :default-mode :first-char-on-line
  :transitions
  (((:normal :paren-ish)
    ((scan "%")
     (set-mode :comment
               :until (scan #\newline)))
    ((scan-any *erlang-begin-var*)
     (set-mode :variable
               :until (scan-any *erlang-terminators*)
               :advancing nil))
    ((scan-any *erlang-begin-word*)
     (set-mode :word-ish
               :until (scan-any *erlang-terminators*)
               :advancing nil))
    ((or
      (scan-any *erlang-open-parens*)
      (scan-any *erlang-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((scan #\')
     (set-mode :atom
               :until (scan #\')))
    ((scan #\?)
     (set-mode :macro
               :until (scan-any *erlang-terminators*)))
    ((scan #\$)
     (set-mode :char
               :until (scan-any *erlang-terminators*)))
    ((scan #\newline)
     (set-mode :first-char-on-line)))
   
   ((:function :attribute)
    ((or
      (scan-any *erlang-open-parens*)
      (scan-any *erlang-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((scan-any *erlang-terminators*)
     (set-mode :normal
               :until (scan #\newline))))
   
   (:first-char-on-line
    ((scan "%")
     (set-mode :comment
               :until (scan #\newline)))
    ((scan-any *erlang-begin-fun*)
     (set-mode :function
               :until (scan #\newline)
               :advancing nil))
    ((scan "-")
     (set-mode :attribute
               :until (scan #\newline)
               :advancing nil))
    ((advance 1)
     (set-mode :normal
               :until (scan #\newline))))
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
  (((:normal :first-char-on-line)
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
   (:function
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"function\">~A</span>"
              s)))
   (:attribute
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"attribute\">~A</span>"
              s)))
   (:macro
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"macro\">~A</span>"
              s)))
   (:atom
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"atom\">~A</span>"
              s)))
   (:char
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
              (when (member (elt s 0) (coerce *erlang-open-parens* 'list))
                (setf open t)
                (setf count (mod paren-counter 6))
                (incf paren-counter))
              (when (member (elt s 0) (coerce *erlang-close-parens* 'list))
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
      (if (member s *erlang-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>" s)
          s)))
   ))
