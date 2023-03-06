(uiop:define-package #:unix-in-lisp
  (:use :cl :named-closure)
  (:import-from :metabang-bind #:bind)
  (:import-from :serapeum #:lastcar :concat :package-exports #:-> #:mapconcat)
  (:import-from :alexandria #:when-let #:if-let)
  (:export #:cd #:install #:uninstall #:setup #:ensure-path #:contents))
(uiop:define-package #:unix-in-lisp.common
  (:use #:unix-in-lisp)
  (:export #:cd #:contents))
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
        (setf (macro-function symbol) #'command-macro)
        (fmakunbound symbol)))
  symbol)

(defun package-path (package)
  "Returns the namestring of the directory mounted to PACKAGE,
or NIL if PACKAGE is not a UNIX FS package."
  (let ((filename (unconvert-case (package-name package))))
    (when (ppath:isabs filename) filename)))

(defun symbol-path (symbol)
  "Returns the namestring of the file mounted to SYMBOL,
Signals an error if the home package SYMBOL is not a Unix FS package.

Note that SYMBOL must be the canonical mounted symbol, to retrieve the
path designated by a symbol, consider using `ensure-path'."
  (if-let (dir (package-path (symbol-package symbol)))
    (ppath:join dir (unconvert-case (symbol-name symbol)))
    (error "Home package ~S of ~S is not a Unix FS package."
           (symbol-package symbol) symbol)))

(-> canonical-symbol (symbol) symbol)
(defun canonical-symbol (symbol)
  "Returns the symbol mounted to the path designated by SYMBOL.
This mounts the symbol in the process and may also mount required Unix
FS packages."
  (cond ((equal (symbol-name symbol) "~") (mount-file "~/"))
        ((ppath:isabs (symbol-name symbol))
         (mount-file (unconvert-case (symbol-name symbol))))
        ((package-path (symbol-package symbol))
         (mount-file (symbol-path symbol)))
        (t (error "~S does not designate a Unix path." symbol))))

(-> ensure-path (t) string)
(defun ensure-path (path)
  "Return the path (a string) designated by PATH."
  (cond ((pathnamep path)
         (setq path (uiop:native-namestring path)))
        ((symbolp path)
         (setq path (symbol-path (canonical-symbol path)))))
  (let ((path (ppath:expanduser (ppath:normpath path))))
    (unless (ppath:isabs path)
      (error "~S is not an absolute path." path))
    path))

(defun to-dir (filename) (ppath:join filename ""))

(-> mount-directory (t) package)
(defun mount-directory (path)
  "Mount PATH as a Unix FS package, which must but a directory.
Return the mounted package."
  (setq path (ensure-path path))
  (cond ((ppath:isdir path))
        ((ppath:lexists path)
         (error "~a is not a directory." path))
        (t (restart-case (error "Directory ~a does not exist." path)
             (create-directory ()
               (ensure-directories-exist (to-dir path))))))
  (bind ((package-name (convert-case path))
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
                        (merge-pathnames uiop:*wild-file-for-directory* (to-dir path))))
    package))

(-> ensure-homed-symbol (string package) symbol)
(defun ensure-homed-symbol (symbol-name package)
  "Make a symbol with PACKAGE as home package.
Return the symbol."
  (let ((symbol (find-symbol symbol-name package)))
    (cond ((not symbol) (intern symbol-name package))
          ((eq (symbol-package symbol) package) symbol)
          (t (let ((use-list (package-use-list package)))
               (unwind-protect
                    (progn
                      (mapc (lambda (p) (unuse-package p package)) use-list)
                      (unintern symbol package)
                      (shadow (list symbol-name) package)
                      (intern symbol-name package))
                 (mapc (lambda (p) (use-package p package)) use-list)))))))

(-> mount-file (t) symbol)
(defun mount-file (filename)
  "Mount FILENAME as a symbol in the appropriate Unix FS package.
Returns the mounted self-evaluating symbol."
  (setq filename (ensure-path filename))
  (bind (((directory . file) (ppath:split filename))
         (package (or (find-package (convert-case directory)) (mount-directory directory)))
         (symbol (ensure-homed-symbol (convert-case file) package)))
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

(defun remount-current-directory ()
  (when-let (pathname (package-path *package*))
    (mount-directory pathname)))

;; TODO: something more clever, maybe `fswatch'
(defvar *post-command-hook*
  (make-instance 'nhooks:hook-void
                 :handlers '(remount-current-directory)))

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

(defun teardown (pipeline)
  (mapc #'uiop:wait-process (processes pipeline))
  (mapc #'uiop:close-streams (processes pipeline)))
(defun alive (pipeline)
  (some #'uiop:process-alive-p (processes pipeline)))
(defvar *interactive-streams* nil)
(defvar *interactive-stream-forcer* nil)
(defun stream-forcer ()
  "Loop body to run in Stream Forcer thread."
  (sleep 0.1)
  (mapc
   (lambda (stream)
     (handler-case
         (force-output stream)
       (stream-error ()
         (setq *interactive-streams* (delete stream *interactive-streams*)))))
   *interactive-streams*))
(defun ensure-stream-forcer ()
  (unless (and *interactive-stream-forcer*
               (bt:thread-alive-p *interactive-stream-forcer*))
    (let (#+swank (connection swank::*emacs-connection*))
      (setq *interactive-stream-forcer*
            (bt:make-thread
             (lambda ()
               (#+swank swank::with-connection #+swank (connection)
                #-swank progn
                (loop (stream-forcer))))
             :name "Unix in Lisp Stream Forcer")))))
(defun add-interactive-stream (stream)
  (ensure-stream-forcer)
  (pushnew stream *interactive-streams*))
(defun stream-copier (input output cont)
  (let (#+swank (connection swank::*emacs-connection*))
    (lambda ()
      (#+swank swank::with-connection #+swank (connection)
       #-swank progn
       (unwind-protect
            (loop
              (handler-case
                  (write-char (read-char input) output)
                (end-of-file () (return))
                (stream-error () (return))))
         (funcall cont))))))
(defun repl-connect (pipeline)
  (let ((repl-thread (bt:current-thread)))
    (add-interactive-stream (input pipeline))
    (add-interactive-stream *standard-output*)
    (bt:make-thread
     (stream-copier
      (output pipeline) *standard-output*
      (lambda ()
        (bt:interrupt-thread
         repl-thread
         (lambda () (ignore-errors (throw 'finish nil))))))
     :name "Unix in Lisp REPL Output Copier")
    (catch 'finish
      (funcall (stream-copier *standard-input* (input pipeline)
                              (lambda ()
                                (close (input pipeline))
                                (teardown pipeline)
                                (nhooks:run-hook *post-command-hook*)))))
    (values)))
(defmethod print-object ((object pipeline) stream)
  (repl-connect object))

(defgeneric contents (object &optional format))
(defmethod contents ((pipeline pipeline) &optional (format :string))
  (unwind-protect (uiop:slurp-input-stream format (output pipeline))
    (teardown pipeline)))

(defgeneric to-argument (object)
  (:method ((symbol symbol))  (princ-to-string symbol))
  (:method ((string string)) string)
  (:method ((cons cons)) (mapconcat #'to-argument cons "")))

(defun execute-command (filename &rest args &aux pipeline)
  (setq filename (ensure-path filename))
  (when (typep (lastcar args) 'pipeline)
    (setq pipeline (lastcar args)
          args (butlast args)))
  (bind ((command (cons filename (mapcar #'princ-to-string args)))
         (directory (uiop:absolute-pathname-p (package-name *package*)))
         (process
          (uiop:launch-program
           command
           :output :stream :error-output :stream
           :input (if pipeline (output pipeline) :stream)
           :directory directory)))
    (bt:make-thread
     (stream-copier (uiop:process-info-error-output process) *standard-output*
                    (lambda ()))
     :name (format nil "~A Error Output Copier" filename))
    (add-interactive-stream *standard-output*)
    (append-process pipeline process)))
(defun command-macro (form env)
  (declare (ignore env))
  (bind (((command . args) form))
    `(apply #'execute-command ',command ,(list 'fare-quasiquote:quasiquote args))))

(defun cd (path)
  (setq *package* (mount-directory path)))

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
  (:merge :fare-quasiquote)
  (:macro-char #\. 'dot-read-macro t)
  (:case :invert))

(defun unquote-reader-hook (orig thunk)
  (if (and (eq *readtable* (named-readtables:find-readtable 'readtable))
           (= fare-quasiquote::*quasiquote-level* 0))
      (let ((fare-quasiquote::*quasiquote-level* 1)) (funcall orig thunk))
      (funcall orig thunk)))

(define-condition already-installed (error) ()
  (:report "There seems to be a previous Unix in Lisp installation."))
(defun install ()
  (when (find-package "UNIX-IN-LISP.PATH")
    (restart-case (error 'already-installed)
      (continue () :report "Uninstall first, then reinstall." (uninstall))
      (reckless-continue () :report "Install on top of it.")))
  (named-readtables:in-readtable readtable)
  ;; Make UNIX-IN-LISP.PATH first because FS packages in PATH will
  ;; circularly reference UNIX-IN-LISP.PATH
  (make-package "UNIX-IN-LISP.PATH")
  (let ((packages (mapcar #'mount-directory (uiop:getenv-pathnames "PATH"))))
    (uiop:ensure-package "UNIX-IN-LISP.PATH" :mix packages :reexport packages))
  (defmethod print-object :around ((symbol symbol) stream)
    (cond ((eq (find-symbol (symbol-name symbol) *package*) symbol) (call-next-method))
          ((not (symbol-package symbol)) (call-next-method))
          ((package-path (symbol-package symbol))
           (write-string (symbol-path symbol) stream))
          (t (call-next-method))))
  (cl-advice:add-advice :around 'fare-quasiquote:call-with-unquote-reader 'unquote-reader-hook)
  (cl-advice:add-advice :around 'fare-quasiquote:call-with-unquote-splicing-reader 'unquote-reader-hook)
  (values))

(defun uninstall ()
  (cl-advice:remove-advice :around 'fare-quasiquote:call-with-unquote-reader 'unquote-reader-hook)
  (cl-advice:remove-advice :around 'fare-quasiquote:call-with-unquote-splicing-reader 'unquote-reader-hook)
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
  (named-readtables:in-readtable :standard)
  (values))

(defun setup ()
  (handler-case (install)
    (already-installed ()))
  (named-readtables:in-readtable readtable)
  (cd "~/")
  (values))
