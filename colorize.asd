;;;; Silly emacs, this is -*- Lisp -*-

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:colorize-system
    (:use #:cl #:asdf))

(in-package #:colorize-system)

(defsystem colorize
    :name "colorize"
    :author "Brian Mastenbrook"
    :licence "MIT"
    :depends-on (:html-encode :split-sequence)
    :components ((:file "colorize-package")
                 (:file "coloring-css" :depends-on ("colorize-package"))
                 (:file "colorize" :depends-on ("colorize-package" "coloring-css"))
                 (:file "abbrev")
                 (:file "clhs-lookup" :depends-on ("abbrev"))
                 (:file "r5rs-lookup")
                 (:file "elisp-lookup")
                 (:file "coloring-types"
                        :depends-on ("colorize" "clhs-lookup"))
                 (:file "lang/php" :depends-on ("colorize"))
                 (:file "lang/perl" :depends-on ("colorize"))
                 (:file "lang/diff" :depends-on ("colorize"))
                 (:file "lang/erlang" :depends-on ("colorize"))
                 (:file "lang/haskell" :depends-on ("colorize"))
                 (:file "lang/python" :depends-on ("colorize"))
                 (:file "lang/refal" :depends-on ("colorize"))
                 (:file "lang/sh" :depends-on ("colorize"))))
