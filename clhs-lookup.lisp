(defpackage :clhs-lookup (:use :common-lisp) (:export :symbol-lookup
                                                      :populate-table
                                                      :spec-lookup))
(in-package :clhs-lookup)

;; aux

(defmacro defmulti (defname &rest bindings)
  `(progn ,@(loop for (name val . tl) on bindings by #'cddr
                  collect `(,defname ,name ,val))))

(defmacro defvars (&rest bindings)
  `(defmulti defvar ,@bindings))

;; vars
           
(defvars *hyperspec-root*      "http://www.lispworks.com/reference/HyperSpec/"
         *hyperspec-pathname*  #P"/usr/share/doc/hyperspec-7.0/HyperSpec/"
         *hyperspec-map-file*  (merge-pathnames "Map_Sym.txt" *hyperspec-pathname*)
         *hyperspec-dump-file* (merge-pathnames "clhs-tables.lisp-expr" #.*compile-file-pathname*)         
         *mop-map-file*        (merge-pathnames "Mop_Sym.txt" #.*compile-file-pathname*)
         *mop-root*            "http://www.alu.org/mop/"
         *symbol-table*        (make-hash-table :test 'equalp)
         *abbrev-table*        (make-hash-table :test 'equalp)
         *section-table*       (make-hash-table :test 'equalp)
         *format-table*        (make-hash-table :test 'equalp)
         *read-macro-table*    (make-hash-table :test 'equalp)         
         *populated-p*         nil
         *last-warn-time*      0)
                                                   
(defun add-clhs-section-to-table (&rest numbers)
  (let ((key (format nil "~{~d~^.~}" numbers))
        (target (concatenate 'string *hyperspec-root* (format nil "Body/~2,'0d_~(~{~36r~}~).htm" (car numbers) (mapcar #'(lambda (x) (+ x 9)) (cdr numbers))))))
    (setf (gethash key *section-table*) target)))

(defun valid-target (&rest numbers)
  (probe-file (format nil "Body/~2,'0d_~(~{~36r~}~).htm" (car numbers) (mapcar #'(lambda (x) (+ x 9)) (cdr numbers)))))

(defun build-tables ()
  (unless *populated-p*
    ;; Hyperspec
    (with-open-file (s *hyperspec-map-file* :if-does-not-exist nil)
      ;; populate the table with the symbols from the Map file
      ;; this bit is easy and portable.
      (unless s
        (when (> (- (get-universal-time) *last-warn-time*) 10)
          (format *trace-output* "Warning: could not find hyperspec map file. Adjust the path at the top of clhs-lookup.lisp to get links to the HyperSpec.~%")
          (setf *last-warn-time* (get-universal-time)))
        (return-from build-tables nil))
      (flet ((set-symbol (sym url)
               (if (char= (elt sym 0) #\&)
                   (setf sym (concatenate 'string "&AMP;" (subseq sym 1))))
               (let ((lt-pos (position #\< sym))
                     (gt-pos (position #\> sym)))
                 (if lt-pos
                     (setf sym (concatenate 'string (subseq sym 0 lt-pos) 
                                                    "&LT;"
                                                    (subseq sym (1+ lt-pos)))))
                 (if gt-pos
                     (setf sym (concatenate 'string (subseq sym 0 gt-pos) 
                                                    "&GT;"
                                                    (subseq sym (1+ gt-pos))))))
               (setf (gethash sym *symbol-table*) url)
               (let ((abbrev (abbrev:abbrev sym)))
                 (and abbrev
                      (pushnew sym (gethash abbrev *abbrev-table* nil)
                               :test #'string-equal)))))
        (do ((symbol-name (read-line s nil s) (read-line s nil s))
             (url (read-line s nil s) (read-line s nil s)))
            ((eq url s) 'done)
          (set-symbol symbol-name (concatenate 'string *hyperspec-root* (subseq url 3)))))
      ;; add in section references.
      (let ((*default-pathname-defaults* *hyperspec-pathname*))
        ;; Yuk. I know. Fixes welcome.
        (loop for section from 0 to 27
              do (add-clhs-section-to-table section)
              do (loop named s for s1 from 1 to 26
                       unless (valid-target section s1)
                       do (return-from s nil)
                       do (add-clhs-section-to-table section s1)
                       do (loop named ss for s2 from 1 to 26
                                unless (valid-target section s1 s2)
                                do (return-from ss nil)
                                do (add-clhs-section-to-table section s1 s2)
                                do (loop named sss for s3 from 1 to 26
                                         unless (valid-target section s1 s2 s3)
                                         do (return-from sss nil)
                                         do (add-clhs-section-to-table section s1 s2 s3)
                                         do (loop named ssss for s4 from 1 to 26
                                                  unless (valid-target section s1 s2 s3 s4)
                                                  do (return-from ssss nil)
                                                  do (add-clhs-section-to-table section s1 s2 s3 s4)
                                                  do (loop named sssss for s5 from 1 to 26
                                                           unless (valid-target section s1 s2 s3 s4 s5)
                                                           do (return-from sssss nil)
                                                           do (add-clhs-section-to-table section s1 s2 s3 s4 s5))))))))
      ;; format directives
      (loop for code from 32 to 127
            do (setf (gethash (format nil "~~~A" (code-char code)) *format-table*)
                     (concatenate 'string
                                  *hyperspec-root*
                                  (case (code-char code)
                                    ((#\c #\C) "Body/22_caa.htm")
                                    ((#\%) "Body/22_cab.htm")
                                    ((#\&) "Body/22_cac.htm")
                                    ((#\|) "Body/22_cad.htm")
                                    ((#\~) "Body/22_cae.htm")
                                    ((#\r #\R) "Body/22_cba.htm")
                                    ((#\d #\D) "Body/22_cbb.htm")
                                    ((#\b #\B) "Body/22_cbc.htm")
                                    ((#\o #\O) "Body/22_cbd.htm")
                                    ((#\x #\X) "Body/22_cbe.htm")
                                    ((#\f #\F) "Body/22_cca.htm")
                                    ((#\e #\E) "Body/22_ccb.htm")
                                    ((#\g #\G) "Body/22_ccc.htm")
                                    ((#\$) "Body/22_ccd.htm")
                                    ((#\a #\A) "Body/22_cda.htm")
                                    ((#\s #\S) "Body/22_cdb.htm")
                                    ((#\w #\W) "Body/22_cdc.htm")
                                    ((#\_) "Body/22_cea.htm")
                                    ((#\<) "Body/22_ceb.htm")
                                    ((#\i #\I) "Body/22_cec.htm")
                                    ((#\/) "Body/22_ced.htm")
                                    ((#\t #\T) "Body/22_cfa.htm")
                                    ;; FIXME
                                    ;((#\<) "Body/22_cfb.htm")
                                    ((#\>) "Body/22_cfc.htm")
                                    ((#\*) "Body/22_cga.htm")
                                    ((#\[) "Body/22_cgb.htm")
                                    ((#\]) "Body/22_cgc.htm")
                                    ((#\{) "Body/22_cgd.htm")
                                    ((#\}) "Body/22_cge.htm")
                                    ((#\?) "Body/22_cgf.htm")
                                    ((#\() "Body/22_cha.htm")
                                    ((#\)) "Body/22_chb.htm")
                                    ((#\p #\P) "Body/22_chc.htm")
                                    ((#\;) "Body/22_cia.htm")
                                    ((#\^) "Body/22_cib.htm")
                                    ((#\Newline) "Body/22_cic.htm")
                                    (t "Body/22_c.htm")))))
      ;; read macros
      (loop for (char page) in 
               '((#\( "a")
				 (#\) "b")
				 (#\' "c")
				 (#\; "d")
				 (#\" "e")
				 (#\` "f")
				 (#\, "g")
				 (#\# "h"))
	         do (setf (gethash (format nil "~A" char) *read-macro-table*)
		              (concatenate 'string
				                   *hyperspec-root*
                                   "Body/02_d"
                                   page
                                   ".htm")))
      (loop for code from 32 to 127
            do (setf (gethash (format nil "#~A" (code-char code)) *read-macro-table*)
                     (concatenate 'string
                                  *hyperspec-root*
                                  "Body/02_dh"
                                  (case (code-char code)
                                    ((#\\) "a")
                                    ((#\') "b")
                                    ((#\() "c")
                                    ((#\*) "d")
                                    ((#\:) "e")
                                    ((#\.) "f")
                                    ((#\b #\B) "g")
                                    ((#\o #\O) "h")
                                    ((#\x #\X) "i")
                                    ((#\r #\R) "j")
                                    ((#\c #\C) "k")
                                    ((#\a #\A) "l")
                                    ((#\s #\S) "m")
                                    ((#\p #\P) "n")
                                    ((#\=) "o")
                                    ((#\#) "p")
                                    ((#\+) "q")
                                    ((#\-) "r")
                                    ((#\|) "s")
                                    ((#\<) "t")
                                    ((#\)) "v")
                                    (t ""))
                                  ".htm"))))
    ;; MOP
    (with-open-file (s *mop-map-file*)
      (do ((symbol-name (read-line s nil s) (read-line s nil s))
           (url (read-line s nil s) (read-line s nil s)))
          ((eq url s) 'done)
        (setf (gethash (concatenate 'string "MOP:" symbol-name) *symbol-table*) (concatenate 'string *mop-root* url))))        
    (setf *populated-p* t)
    (dump-tables)))
    
(defun load-tables ()
  (with-open-file (i-stream *hyperspec-dump-file* :direction :input)
    (loop for table in (list *symbol-table* *abbrev-table* *section-table* 
                             *format-table* *read-macro-table*)
           do (loop for (key . val) in (read i-stream)
                     do (setf (gethash key table) val))))
  (setf *populated-p* t))

(defun dump-tables ()
  (with-open-file (o-stream *hyperspec-dump-file* :direction :output)
    (loop for table in (list *symbol-table* *abbrev-table* *section-table* 
                             *format-table* *read-macro-table*)
           do (print (loop for key being the hash-keys in table
                                   using (hash-value val)
                           collect (cons key val))
                     o-stream))))

(defun abbrev-lookup (term)
  (let ((abbrevs (gethash term *abbrev-table* nil)))
    (if (eql (length abbrevs) 0)
        nil
        (if (eql (length abbrevs) 1)
            (format nil "~A: ~A"
                    (car abbrevs)
                    (gethash (car abbrevs) *symbol-table*))
            (format nil "Matches: ~{~A~^ ~}"
                    abbrevs)))))
              
(defun populate-table ()
  (if (probe-file *hyperspec-dump-file*)
      (load-tables)
      (build-tables)))

(defun spec-lookup (term &key (type :all))
  (unless *populated-p*
    (populate-table))
  (ecase type
    (:all
     (or (gethash term *symbol-table*)
         (gethash term *section-table*)
         (gethash term *format-table*)
         (gethash term *read-macro-table*)
         (abbrev-lookup term)))
    (:abbrev
     (abbrev-lookup term))
    (:symbol
     (gethash term *symbol-table*))
    (:section
     (gethash term *section-table*))
    (:format
     (gethash term *format-table*))
    (:read-macro
     (gethash term *read-macro-table*))))

(defun symbol-lookup (term)
  (spec-lookup term :type :symbol))