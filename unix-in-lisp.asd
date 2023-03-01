(asdf:defsystem unix-in-lisp
  :depends-on (:named-closure
               :fof :metabang-bind
               :trivial-package-locks
               :cl-advice :nhooks)
  :components ((:file "unix-in")
               #+swank (:file "unix-in-slime" :depends-on ("unix-in"))))
