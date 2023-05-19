(asdf:defsystem unix-in-lisp
  :depends-on (:osicat
               :metabang-bind
               :pathname-utils
               :ppath
               :fare-quasiquote-extras
               :named-readtables
               :cl-advice
               :nhooks
               :native-lazy-seq
               :iolib
               :iolib/os)
  :components ((:file "unix-in")
               #+(or swank slynk) (:file "unix-in-slime" :depends-on ("unix-in"))))
