(defpackage #:unix-in-lisp
  (:use :cl :named-closure)
  (:import-from :metabang-bind #:bind)
  (:import-from :serapeum #:lastcar :concat :package-exports)
  (:import-from :alexandria #:when-let #:if-let)
  (:export #:cd #:install #:uninstall #:setup #:*suppress-unbound-variable*))
(defpackage #:unix-in-lisp.common
  (:use #:unix-in-lisp)
  (:export #:cd))
(in-package #:unix-in-lisp)

(defun convert-case (string)
  "Convert external representation of STRING into internal representation
for `package-name's and `symbol-name's."
  (let ((*print-case* :upcase))
    (format nil "~a" (make-symbol string))))
(defun unconvert-case (string)
  (let ((*print-case* :downcase))
    (format nil "~a" (make-symbol string))))

(defun ensure-executable (symbol)
  (let ((filename (symbol-path symbol)))
    (if (handler-case
            (intersection (osicat:file-permissions filename)
                          '(:user-exec :group-exec :other-exec))
          (osicat-posix:enoent ()
            (warn "Probably broken symlink: ~a" filename)
            nil))
        (setf (symbol-function symbol) (make-execute filename))
        (fmakunbound symbol)))
  symbol)

(defun ensure-path (filename)
  "Expand ~ as home directory in FILENAME, and make sure it's absolute.
Trailing slashes are stripped."
  (cond ((pathnamep filename)
         (setq filename (uiop:native-namestring filename)))
        ((symbolp filename)
         (setq filename (symbol-path filename))))
  (let ((path (ppath:expanduser (ppath:normpath filename))))
    (unless (ppath:isabs path)
      (error "~S is not an absolute path." filename))
    path))
(defun to-dir (filename) (ppath:join filename ""))

(defun mount-directory (filename)
  "Mount FILENAME as a Unix FS package, which must but a directory.
Returns the mounted package."
  (setq filename (ensure-path filename))
  (cond ((ppath:isdir filename))
        ((ppath:lexists filename)
         (error "~a is not a directory." filename))
        (t (restart-case (error "Directory ~a does not exist." filename)
             (create-directory ()
               (ensure-directories-exist (to-dir filename))))))
  (bind ((package-name (convert-case filename))
         (package (or (find-package package-name)
                      (uiop:ensure-package
                       package-name
                       :mix '("UNIX-IN-LISP.COMMON" "UNIX-IN-LISP.PATH" "CL")))))
    ;; In case the directory is already mounted, check and remove
    ;; symbols whose mounted file no longer exists
    (mapc (lambda (symbol)
            (when (not (ppath:lexists (symbol-path symbol)))
              (unintern symbol package)))
          (package-exports package))
    (mapc #'mount-file (uiop:directory*
                        (merge-pathnames uiop:*wild-file-for-directory* (to-dir filename))))
    package))

(defun mount-file (filename)
  "Mount FILENAME as a symbol in the appropriate Unix FS package.
Returns the mounted self-evaluating symbol."
  (setq filename (ensure-path filename))
  (bind (((directory . file) (ppath:split filename))
         (package (or (find-package (convert-case directory)) (mount-directory directory)))
         (symbol-name (convert-case file))
         (symbol (progn
                   (when (eq (nth-value 1 (find-symbol symbol-name package)) :inherited)
                     (shadow (list symbol-name) package))
                   (intern symbol-name package))))
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

(defun package-path (package)
  "Returns the namestring of the directory mounted to PACKAGE,
or NIL if PACKAGE is not a UNIX FS package."
  (let ((filename (unconvert-case (package-name package))))
    (when (ppath:isabs filename) filename)))

(defun symbol-path (symbol)
  (ppath:join (package-path (symbol-package symbol))
              (unconvert-case (symbol-name symbol))))

(defvar *post-command-hook* (make-instance 'nhooks:hook-void))

(defun remount-current-directory ()
  (when-let (pathname (package-path *package*))
    (mount-directory pathname)))

(defclass pipeline ()
  ((input :reader input :initarg :input :type output-stream)
   (output :reader output :initarg :output :type input-stream)
   (processes :reader processes :initarg :processes)))
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
(defun alive (pipeline)
  (some #'uiop:process-alive-p (processes pipeline)))

(defun repl-connect (pipeline)
  (bind ((repl-output *standard-output*)
         (repl-thread (bt:current-thread))
         #+swank (connection swank::*emacs-connection*))
    (bt:make-thread
     (lambda ()
       (#+swank swank::with-connection #+swank (connection)
        #-swank progn
        (loop
          (sleep 0.1)
          (force-output repl-output)
          (handler-case
              (force-output (input pipeline))
            (stream-error () (return))))))
     :name "Unix in Lisp REPL Stream Forcer")
    (bt:make-thread
     (lambda ()
       (#+swank swank::with-connection #+swank (connection)
        #-swank progn
         (unwind-protect
              (loop
                (handler-case
                    (write-char (read-char (output pipeline)) repl-output)
                  (end-of-file () (return))
                  (stream-error () (return))))
           (close-streams pipeline)
           (bt:interrupt-thread
            repl-thread
            (lambda () (ignore-errors (throw 'finish nil)))))))
     :name "Unix in Lisp REPL Output Copier")
    (unwind-protect
         (catch 'finish
           (loop
             (handler-case
                 (progn
                   (write-char (read-char) (input pipeline))
                   (force-output (input pipeline)))
               (end-of-file ()
                 (close (input pipeline)))
               (stream-error () (return))))
           (wait pipeline))
      (nhooks:run-hook *post-command-hook*))
    (values)))
(defmethod print-object ((object pipeline) stream)
  (repl-connect object))

(defnclo execute (filename) (&rest args &aux pipeline)
  (when (typep (lastcar args) 'pipeline)
    (setq pipeline (lastcar args)
          args (butlast args)))
  (bind ((command (cons filename (mapcar #'princ-to-string args)))
         (directory (uiop:absolute-pathname-p (package-name *package*)))
         (process
          (uiop:launch-program
           command
           :output :stream :error-output *error-output*
           :input (if pipeline (output pipeline) :stream)
           :directory directory)))
    (append-process pipeline process)))

(defun cd (path)
  (setq *package* (mount-directory path)))

(defun convert-symbol (symbol package)
  "Recognize shorthands when reading SYMBOL in PACKAGE.
Returns the SYMBOL with shorthand resolved."
  (cond ((not (eq (symbol-package symbol) package)) symbol)
        ((equal (symbol-name symbol) "~") (mount-file "~/"))
        ((ppath:isabs (symbol-name symbol))
         (mount-file (unconvert-case (symbol-name symbol))))
        ((and (package-path package)
              (find ppath.details.constants:+separator+ (symbol-name symbol)))
         (mount-file (symbol-path symbol)))
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
    (if (package-path *package*)
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

(defvar *suppress-unbound-variable* t)

(defun debug-hook (orig condition hook)
  (if (and (typep condition 'unbound-variable)
           *suppress-unbound-variable*
           (package-path *package*))
      (invoke-restart 'use-value (cell-error-name condition))
      (funcall orig condition hook)))

(defun installed-p () (find-package "UNIX-IN-LISP.PATH"))

(defun install ()
  (when (installed-p)
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
  (defmethod print-object :around ((symbol symbol) stream)
    (cond ((eq (symbol-package symbol) *package*) (call-next-method))
          ((not (symbol-package symbol)) (call-next-method))
          ((package-path (symbol-package symbol))
           (write-string (symbol-path symbol) stream))
          (t (call-next-method))))
  ;; TODO: something more clever, maybe `fswatch'
  (nhooks:add-hook *post-command-hook* 'remount-current-directory)
  (values))

(defun setup ()
  (unless (installed-p)
    (install))
  (named-readtables:in-readtable readtable)
  (setq *debugger-hook* (cl-advice:ensure-advisable-function *debugger-hook*))
  (cl-advice:add-advice :around *debugger-hook* 'debug-hook)
  (cd "~/")
  (values))

(defun uninstall ()
  (when-let (method (find-method #'print-object '(:around) '(symbol t) nil))
    (remove-method #'print-object method))
  (mapc
   (lambda (p)
     (when (package-path p)
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
