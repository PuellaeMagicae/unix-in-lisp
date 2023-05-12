(asdf:defsystem unix-in-lisp
  :depends-on (:osicat
               :metabang-bind
               :ppath
               :fare-quasiquote-extras
               :named-readtables
               :cl-advice
               :nhooks
               :native-lazyseq
               :iolib
               :iolib/os)
  :components ((:file "unix-in")
               #+(or swank slynk) (:file "unix-in-slime" :depends-on ("unix-in"))))
