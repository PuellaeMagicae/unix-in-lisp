(asdf:defsystem unix-in-lisp
  :depends-on (:osicat
               :metabang-bind
               :ppath
               :fare-quasiquote-extras
               :named-readtables
               :named-closure
               :cl-advice
               :nhooks
               :native-lazy-seq
               :iolib
               :iolib/os
               :scriptl)
  :components ((:file "unix-in")
               #+(or swank slynk) (:file "unix-in-slime" :depends-on ("unix-in"))))
