(asdf:defsystem unix-in-lisp
  :depends-on (:named-closure
               :osicat :metabang-bind :ppath :fare-quasiquote-extras
               :trivial-package-locks :named-readtables
               :cl-advice :nhooks)
  :components ((:file "unix-in")
               #+swank (:file "unix-in-slime" :depends-on ("unix-in"))))
