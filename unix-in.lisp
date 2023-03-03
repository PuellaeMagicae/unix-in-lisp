(defpackage #:unix-in-lisp
  (:use :cl :named-closure)
  (:import-from :metabang-bind #:bind)
  (:import-from :serapeum #:lastcar :concat :package-exports)
  (:export #:cd #:install #:uninstall))
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

(defun ensure-executable (symbol)
  (let ((pathname (symbol-pathname symbol)))
    (if (handler-case
            (intersection (osicat:file-permissions pathname)
                          '(:user-exec :group-exec :other-exec))
          (osicat-posix:enoent ()
            (warn "Probably broken symlink: ~a" pathname)
            nil))
        (setf (symbol-function symbol) (make-execute pathname))
        (fmakunbound symbol)))
  symbol)

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
  (bind ((package-name (convert-case (namestring filename)))
         (package (or (find-package package-name)
                      (uiop:ensure-package
                       package-name
                       :mix '("UNIX-IN-LISP.COMMON" "UNIX-IN-LISP.PATH" "CL")))))
    ;; In case the directory is already mounted, check and remove
    ;; symbols whose mounted file no longer exists
    (mapc (lambda (symbol)
            (when (not (uiop:file-exists-p (symbol-pathname symbol)))
              (unintern symbol package)))
          (package-exports package))
    (mapc #'mount-file (uiop:directory* (merge-pathnames uiop:*wild-file-for-directory* filename)))
    package))

(defun mount-file (filename)
  "Mount FILENAME as a symbol in the appropriate Unix FS package.
Returns the mounted self-evaluating symbol."
  (setq filename (ensure-pathname filename))
  (bind (((:values directory-1 file-1)
          (if (uiop:directory-pathname-p filename)
              (values (namestring (make-pathname :directory (butlast (pathname-directory filename))))
                      (lastcar (pathname-directory filename)))
              (values (directory-namestring filename) (file-namestring filename))))
         (directory (convert-case directory-1))
         (file (convert-case file-1))
         (package (or (find-package directory) (mount-directory directory)))
         (symbol (progn
                   (when (eq (nth-value 1 (find-symbol file package)) :inherited)
                     (shadow (list file) package))
                   (intern file package))))
    (proclaim `(special ,symbol))
    (cond ((not (boundp symbol)) (setf (symbol-value symbol) symbol))
          ((eq (symbol-value symbol) symbol))
          (t (restart-case
                 (error "Symbol ~S has previously bound to ~S." symbol (symbol-value symbol))
               (reckless-continue () :report "Overwrite the binding."
                 (setf (symbol-value symbol) symbol)))))
    (export symbol (symbol-package symbol))
    (when (uiop:file-exists-p filename)
      (ensure-executable symbol))
    symbol))

(defun package-pathname (package)
  "Returns the pathname of the directory mounted to PACKAGE,
or NIL if PACKAGE is not a UNIX FS package."
  (uiop:absolute-pathname-p (unconvert-case (package-name package))))

(defun symbol-pathname (symbol)
  (uiop:merge-pathnames* (unconvert-case (symbol-name symbol)) (package-pathname (symbol-package symbol))))

(defvar *post-command-hook* (make-instance 'nhooks:hook-void))

(defun remount-current-directory ()
  (alexandria:when-let (pathname (package-pathname *package*))
    (mount-directory pathname)))

(defclass pipeline ()
  ((input :reader input :initarg :input :type output-stream)
   (output :reader output :initarg :output :type input-stream)
   (processes :reader processes :initarg :processes)))
(defmethod chanl:send ((pipeline pipeline) value &key (blockp t)) ;; TODO: is this correct BLOCKP?
  (if (eq value 'eof)
      (close (input pipeline))
      (progn
        (princ value (input pipeline))
        (when blockp (force-output (input pipeline))))))
(defmethod chanl:recv ((pipeline pipeline) &key (format :line))
  (case format
    ((:char) (read-char (output pipeline)))
    (t (uiop:slurp-input-stream format (output pipeline)))))
(defun append-process (pipeline process)
  (if pipeline
      (with-slots (processes output) pipeline
        (setq output (uiop:process-info-output process)
              processes (nconc processes (list process)))
        pipeline)
      (make-instance 'pipeline
                     :input (uiop:process-info-input process)
                     :output (uiop:process-info-output process)
                     :processes (list process))))
(defun wait (pipeline)
  (mapc #'uiop:wait-process (processes pipeline)))
(defun close-streams (pipeline)
  (mapc #'uiop:close-streams (processes pipeline)))

(defun repl-connect (pipeline)
  (bind ((repl-input *standard-input*)
         (repl-output *standard-output*)
         (output-copier
          (bt:make-thread
           (lambda ()
             (unwind-protect
                  (loop
                    (handler-case
                        (write-char (chanl:recv pipeline :format :char) repl-output)
                      (end-of-file () (return))
                      (stream-error () (return))))
               (close-streams pipeline)))
           :name "Unix in Lisp REPL Output Copier"))
         (input-copier
          (bt:make-thread
           (lambda ()
             (catch 'finish
               (loop
                 (chanl:send pipeline (handler-case
                                          (read-char repl-input)
                                        (end-of-file () 'eof)
                                        (stream-error () (return)))))))
           :name "Unix in Lisp REPL Input Copier")))
    (unwind-protect
         (wait pipeline)
      (nhooks:run-hook *post-command-hook*)
      (ignore-errors
       (bt:interrupt-thread input-copier (lambda ()
                                           (ignore-errors (throw 'finish nil))))))
    (values)))
(defmethod print-object ((object pipeline) stream)
  (repl-connect object))

(defgeneric to-argument (object)
  (:method ((string string)) string)
  (:method ((symbol symbol))
    (if (keywordp symbol)
        (format nil "-~A" symbol)
        (format nil "~A" symbol))))

(defnclo execute (filename) (&rest args &aux pipeline)
  (when (typep (lastcar args) 'pipeline)
    (setq pipeline (lastcar args)
          args (butlast args)))
  (bind ((command (cons filename (mapcar #'to-argument args)))
         (directory (uiop:absolute-pathname-p (package-name *package*)))
         (process
          (uiop:launch-program
           command
           :output :stream :error-output *error-output*
           :input (if pipeline (output pipeline) :stream)
           :directory directory)))
    (append-process pipeline process)))

(defun cd (path)
  (setq *package* (mount-directory (to-argument path))))

(defun convert-symbol (symbol package)
  "Recognize shorthands when reading SYMBOL in PACKAGE.
Returns the SYMBOL with shorthand resolved."
  (cond ((not (eq (symbol-package symbol) package)) symbol)
        ((equal (symbol-name symbol) "~") (mount-file "~/"))
        ((uiop:absolute-pathname-p (symbol-name symbol))
         (mount-file (symbol-name symbol)))
        ((package-pathname package)
         (mount-file (uiop:merge-pathnames* (unconvert-case (symbol-name symbol)) (package-pathname package))))
        (t symbol)))

(defvar *inhibit-intern-hook* nil)
(defun intern-hook (orig &rest args &aux (old (apply orig args)))
  (if *inhibit-intern-hook* old
      (bind ((*inhibit-intern-hook* t)
             (new (convert-symbol old *package*)))
        (unless (eq new old) (unintern old))
        new)))

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
  (when (find-package "UNIX-IN-LISP.PATH")
    (restart-case
        (error "There seems to be a previous Unix in Lisp installation.")
      (continue () :report "Uninstall first, then reinstall." (uninstall))
      (reckless-continue () :report "Install on top of it.")))
  (named-readtables:in-readtable readtable)
  (trivial-package-locks:without-package-locks
    (let ((intern #-sbcl 'intern #+sbcl 'sb-impl::%intern))
      (cl-advice:add-advice :around intern 'intern-hook)))
  ;; Make UNIX-IN-LISP.PATH first because FS packages in PATH will
  ;; circularly reference UNIX-IN-LISP.PATH
  (make-package "UNIX-IN-LISP.PATH")
  (let ((packages (mapcar #'mount-directory (uiop:getenv-pathnames "PATH"))))
    (uiop:ensure-package "UNIX-IN-LISP.PATH" :mix packages :reexport packages))
  (defmethod print-object :around ((object symbol) stream)
    (cond ((eq (symbol-package object) *package*)
           (call-next-method))
          ((not (symbol-package object)) (call-next-method))
          ((package-pathname (symbol-package object))
           (write-string (convert-case (package-name (symbol-package object))) stream)
           (write-string (convert-case (symbol-name object)) stream))
          (t (call-next-method))))
  ;; TODO: something more clever, maybe `fswatch'
  (nhooks:add-hook *post-command-hook* 'remount-current-directory)
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
