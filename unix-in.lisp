(defpackage #:unix-in-lisp
  (:use :cl :named-closure)
  (:import-from :metabang-bind #:bind)
  (:import-from :serapeum #:lastcar)
  (:export #:cd #:content #:install #:uninstall))
(defpackage #:unix-in-lisp.common
  (:use #:unix-in-lisp)
  (:export #:cd #:content))
(in-package #:unix-in-lisp)

(defun convert-case (string)
  (let ((*print-case* :upcase))
    (format nil "~a" (make-symbol string))))
(defun unconvert-case (string)
  (let ((*print-case* :downcase))
    (format nil "~a" (make-symbol string))))

(defclass file (fof:file)
  ((symbol :reader file-symbol :initarg :symbol)
   (package :reader file-package)))
(defun ensure-executable (file)
  (let ((symbol (file-symbol file)))
    (if (fof:executable? file)
        (setf (symbol-function symbol) (make-execute file))
        (fmakunbound symbol))))
(defmethod (setf fof:permissions) :after (permissions (file file))
  (ensure-executable file))
(defmethod initialize-instance :after ((file file) &key)
  (ensure-executable file))
(defmethod file-package ((file file))
  (serapeum:ensure (slot-value file 'package)
    (mount-directory (fof:path file))))
(defun make-file (filename symbol)
  (make-instance 'file :path filename :symbol symbol))
(defun ensure-pathname (filename)
  "Expand ~ as home directory in FILENAME, and make sure it's absolute."
  (uiop:ensure-pathname (uiop:native-namestring filename) :want-absolute t))
(defun mount-directory (filename)
  "Mount FILENAME as a Unix FS package, which must but a directory.
Returns the mounted package."
  (setq filename (ensure-pathname filename))
  (cond ((uiop:directory-exists-p filename)
         (setq filename (uiop:directory-exists-p filename)))
        ((uiop:file-exists-p filename)
         (error "~a is not a directory." filename))
        (t (restart-case (error "Directory ~a does not exist." filename)
             (create-directory ()
               (ensure-directories-exist filename)))))
  (bind ((package-name (convert-case (namestring filename))))
    (prog1 (or (find-package package-name)
               (uiop:ensure-package package-name :mix '("UNIX-IN-LISP.COMMON" "UNIX-IN-LISP.PATH" "CL")))
      (mapc #'mount-file (uiop:directory* (merge-pathnames uiop:*wild-file-for-directory* filename))))))

(defun mount-file (filename)
  "Mount FILENAME as a symbol in a Unix FS package mounted to its directory.
The symbol is bounded to a file object tracking attributes of FILENAME.
Returns two values: the file object and the symbol."
  (setq filename (ensure-pathname filename))
  (bind (((:values directory file)
          (if (uiop:directory-pathname-p filename)
              (values (namestring (make-pathname :directory (butlast (pathname-directory filename))))
                      (lastcar (pathname-directory filename)))
              (values (directory-namestring filename) (file-namestring filename))))
         (directory (convert-case directory))
         (file (convert-case file))
         (package (or (find-package directory) (mount-directory directory)))
         (symbol (progn
                   (when (eq (nth-value 1 (find-symbol file package)) :inherited)
                     (shadow (list file) package))
                   (intern file package))))
    (proclaim `(special ,symbol))
    (export symbol package)
    (values (setf (symbol-value symbol) (make-file (pathname filename) symbol)) symbol)))
(defun package-pathname (package)
  "Returns the pathname of the directory mounted to PACKAGE,
or NIL if PACKAGE is not a UNIX FS package."
  (uiop:absolute-pathname-p (package-name package)))

(defgeneric to-argument (object)
  (:documentation
   "Convert OBJECT into a string suitable for passing to Unix commands."))
(defmethod to-argument ((object string)) object)
(defmethod to-argument ((object symbol))
  (cond ((keywordp object)
         (serapeum:concat "-" (unconvert-case (symbol-name object))))
        (t (format nil "~a" object))))
(defmethod to-argument ((object fof:file)) (fof:path object))

(defclass result () ())
(defun repl-connect-result (result)
  (bind ((input-stream (uiop:process-info-input result))
         (output-stream (uiop:process-info-output result))
         (repl-input *standard-input*)
         (repl-output *standard-output*)
         (output-copier
          (bt:make-thread
           (lambda ()
             (unwind-protect
                  (loop
                    (handler-case
                        (write-char (read-char output-stream) repl-output)
                      (end-of-file () (return))
                      (stream-error () (return))))
               (uiop:close-streams result)))
           :name "Unix in Lisp REPL Output Copier"))
         (input-copier
          (bt:make-thread
           (lambda ()
             (catch 'finish
               (loop
                 (handler-case
                     (progn
                       (write-char (read-char repl-input) input-stream)
                       (force-output input-stream))
                   (end-of-file ()
                     (close input-stream))
                   (stream-error () (return))))))
           :name "Unix in Lisp REPL Input Copier")))
    (unwind-protect
         (uiop:wait-process result)
      (ignore-errors
       (bt:interrupt-thread input-copier (lambda ()
                                           (ignore-errors (throw 'finish nil))))))
    (values)))
(defmethod print-object ((object result) stream)
  (repl-connect-result object))

(defgeneric content (object &optional format))
(defmethod content ((object result) &optional (format :string))
  (unwind-protect
       (prog1 (uiop:slurp-input-stream format (uiop:process-info-output object))
         (uiop:wait-process object))
    (uiop:close-streams object)))
(defmethod content ((object file) &optional (format :string))
  (with-open-file (stream (fof:path object) :direction :input)
    (uiop:slurp-input-stream format stream)))

(defnclo execute (file) (&rest args &aux input)
  (when (typep (lastcar args) 'result)
    (setq input (lastcar args)
          args (butlast args)))
  (bind ((input-stream (if input (uiop:process-info-output input) :stream))
         (command (cons (fof:path file) (mapcar #'to-argument args)))
         (directory (uiop:absolute-pathname-p (package-name *package*))))
    (dynamic-mixins:ensure-mix
     (uiop:launch-program
      command
      :output :stream :error-output *error-output*
      :input input-stream :directory directory)
     'result)))

(defun cd (path)
  (let ((path (to-argument path)))
    (setq *package* (mount-directory path))))

(defun convert-symbol (symbol package)
  "Recognize shorthands when reading SYMBOL in PACKAGE.
Returns the SYMBOL with shorthand resolved."
  (cond ((not (eq (symbol-package symbol) package)) symbol)
        ((equal (symbol-name symbol) "~") (nth-value 1 (mount-file "~/")))
        ((uiop:absolute-pathname-p (symbol-name symbol))
            (nth-value 1 (mount-file (symbol-name symbol))))
        ((and (package-pathname package)
              (find (uiop:directory-separator-for-host) (symbol-name symbol)))
         (nth-value 1 (mount-file (uiop:ensure-pathname
                                   (symbol-name symbol)
                                   :defaults (pathname (package-name package))
                                   :ensure-absolute t))))
        (t symbol)))

(defun intern-hook (orig &rest args)
  (bind ((old (apply orig args))
         (new (convert-symbol old *package*)))
    (unless (eq new old) (unintern old))
    new))

(defun dot-read-macro (stream char)
  (flet ((delimiter-p (c)
           (or (eq c 'eof) (sb-impl:token-delimiterp c)))
         (unread (c)
           (unless (eq c 'eof) (unread-char c stream)))
         (standard-read ()
           (bind (((:values function terminating-p) (get-macro-character #\.)))
             (unwind-protect
                  (progn
                    (set-macro-character #\. nil t)
                    (read stream))
               (set-macro-character #\. function terminating-p)))))
    (if (package-pathname *package*)
        (let ((char-1 (read-char stream nil 'eof))
              (char-2 (read-char stream nil 'eof)))
          (cond
            ((delimiter-p char-1)
             (unread char-1)
             (intern "./"))
            ((and (eq char-1 #\.) (delimiter-p char-2))
             (unread char-2)
             (intern "../"))
            (t (unread char-2)
               (unread char-1)
               (unread char)
               (standard-read))))
        (progn (unread char)
               (standard-read)))))
(named-readtables:defreadtable readtable
  (:merge :standard)
  (:macro-char #\. 'dot-read-macro t)
  (:case :invert))

(defun install ()
  (named-readtables:in-readtable readtable)
  (when (find-package "UNIX-IN-LISP.PATH")
    (restart-case
        (error "There seems to be a previous Unix in Lisp installation.")
      (continue () :report "Uninstall first, then reinstall." (uninstall))
      (reckless-continue () :report "Install on top of it.")))
  (trivial-package-locks:without-package-locks
    (let ((intern #-sbcl 'intern #+sbcl 'sb-impl::%intern))
      (cl-advice:add-advice :around intern 'intern-hook)))
  ;; Make UNIX-IN-LISP.PATH first because FS packages in PATH will
  ;; circularly reference UNIX-IN-LISP.PATH
  (make-package "UNIX-IN-LISP.PATH")
  (let ((packages (mapcar #'mount-directory (uiop:getenv-pathnames "PATH"))))
    (uiop:ensure-package "UNIX-IN-LISP.PATH" :mix packages :reexport packages))
  (defmethod print-object :around ((object symbol) stream)
    (cond ((eq object (find-symbol (symbol-name object) *package*))
           (call-next-method))
          ((not (symbol-package object)) (call-next-method))
          ((package-pathname (symbol-package object))
           (write-string (package-name (symbol-package object)) stream)
           (write-string (symbol-name object) stream))
          (t (call-next-method))))
  (values))

(defun uninstall ()
  (alexandria:when-let (method (find-method #'print-object '(:around) '(symbol t) nil))
    (remove-method #'print-object method))
  (mapc
   (lambda (p)
     (when (package-pathname p)
       (handler-bind ((package-error #'continue))
         (delete-package p))))
   (list-all-packages))
  (when (find-package "UNIX-IN-LISP.PATH")
    (delete-package "UNIX-IN-LISP.PATH"))
  (trivial-package-locks:without-package-locks
    (let ((intern #-sbcl 'intern #+sbcl 'sb-impl::%intern))
      (cl-advice:remove-advice :around intern 'intern-hook)))
  (named-readtables:in-readtable :standard)
  (values))
