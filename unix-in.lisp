(uiop:define-package :unix-in-lisp
  (:use :cl :iter)
  (:import-from :metabang-bind #:bind)
  (:import-from :serapeum #:lastcar :package-exports #:-> #:mapconcat
                #:concat #:string-prefix-p #:synchronized #:ensure)
  (:import-from :alexandria #:when-let #:if-let #:deletef #:ignore-some-conditions
                #:assoc-value)
  (:export #:cd #:install #:uninstall #:setup #:ensure-path #:contents #:defile #:pipe
           #:repl-connect #:*jobs* #:ensure-env-var #:synchronize-env-to-unix
           #:with-relative-symbols))

(uiop:define-package :unix-in-lisp.common)
(uiop:define-package :unix-user
    (:mix :unix-in-lisp :unix-in-lisp.common :serapeum :alexandria :cl))

(in-package #:unix-in-lisp)
(named-readtables:in-readtable :standard)

(defvar *post-command-hook* (make-instance 'nhooks:hook-void))

;;; File system

;;;; package system mounting

(-> mount-file (t) symbol)
(-> ensure-path (t &optional boolean) string)
(-> mount-directory (t) package)
(-> ensure-homed-symbol (string package) symbol)

(defun convert-case (string)
  "Convert external representation of STRING into internal representation
for `package-name's and `symbol-name's."
  (let ((*print-case* :upcase))
    (format nil "~a" (make-symbol string))))
(defun unconvert-case (string)
  "Convert internal representation STRING from `package-name's and
`symbol-name's into external representations for paths."
  (let ((*print-case* :downcase))
    (format nil "~a" (make-symbol string))))

(defun ensure-executable (symbol)
  (let ((filename (symbol-path symbol)))
    (if (handler-case
            (intersection (osicat:file-permissions filename)
                          '(:user-exec :group-exec :other-exec))
          (osicat-posix:enoent ()
            ;; This warning is too noisy
            #+nil (warn "Probably broken symlink: ~a" filename)
            nil))
        (setf (macro-function symbol) #'command-macro)
        (fmakunbound symbol)))
  symbol)

(defun package-path (package)
  "Returns the pathname of the directory mounted to PACKAGE,
or NIL if PACKAGE is not a UNIX FS package."
  (let ((n (package-name package)))
    (when (ppath:isabs n)
      (unconvert-case n))))

(defun symbol-path (symbol &optional relative)
  "Returns the pathname of the file mounted to SYMBOL,
Signals an error if the home package SYMBOL is not a Unix FS package."
  (ppath:join
   (or (and (symbol-package symbol)
            (package-path (symbol-package symbol)))
       (if relative
           (uiop:native-namestring *default-pathname-defaults*)
           (error "Home package ~S of ~S is not a Unix FS package."
                  (symbol-package symbol) symbol)))
   (unconvert-case (symbol-name symbol))))

(defun symbol-home-p (symbol) (eq *package* (symbol-package symbol)))

(defun ensure-path (path &optional relative)
  "Return the path designated by PATH."
  (when (symbolp path)
    (setq path (symbol-path path relative)))
  (when (pathnamep path)
    (setq path (uiop:native-namestring path)))
  (setq path (ppath:expanduser path))
  (unless (ppath:isabs path)
    (if relative
        (setq path (ppath:join (uiop:native-namestring *default-pathname-defaults*) path))
        (error "~S is not an absolute path." path)))
  (ppath:normpath path))

(defun to-dir (path) (ppath:join path ""))

(define-condition wrong-file-kind (simple-error file-error)
  ((wanted-kind :initarg :wanted-kind) (actual-kind :initarg :actual-kind))
  (:report
   (lambda (c s)
     (with-slots (wanted-kind actual-kind) c
       (format s "~a is a ~a, wanted ~a."
               (file-error-pathname c) actual-kind wanted-kind)))))

(define-condition file-does-not-exist (simple-error file-error) ()
  (:report
   (lambda (c s)
     (format s "~a does not exist." (file-error-pathname c)))))

(defun assert-file-kind (path &rest kinds)
  (let ((kind (osicat:file-kind (uiop:parse-native-namestring path) :follow-symlinks t)))
    (unless (member kind kinds)
      (if kind
          (error 'wrong-file-kind :pathname path :wanted-kind kinds :actual-kind kind)
          (error 'file-does-not-exist :pathname path)))))

(defvar *stat-cache* (make-hash-table :weakness :key)
  "Map Unix FS packages (maybe in the future, also file symbols) to
the current idea Unix in Lisp have about their status (currently
`isys:stat' structures).

Used for checking mtime and skip remounting if it didn't change since
last mount.")

(defun mount-directory (path)
  "Mount PATH as a Unix FS package, which must be a directory.
Return the mounted package."
  (setq path (to-dir (ensure-path path)))
  (restart-case
      (assert-file-kind path :directory)
    (create-directory ()
      :test (lambda (c) (typep c 'file-does-not-exist))
      (ensure-directories-exist
       (uiop:parse-native-namestring path))))
  (bind ((package-name (convert-case path))
         (package (or (find-package package-name)
                      (uiop:ensure-package package-name)))
         (stat (isys:stat path)))
    ;; If mtime haven't changed, skip remounting.
    (when-let (old-stat (gethash package *stat-cache*))
      (when (= (isys:stat-mtime old-stat)
               (isys:stat-mtime stat))
        (return-from mount-directory package)))
    (setf (gethash package *stat-cache*) stat)
    ;; In case the directory is already mounted, check and remove
    ;; symbols whose mounted file no longer exists
    (mapc (lambda (symbol)
            (when (not (ppath:lexists (symbol-path symbol)))
              (unintern symbol package)))
          (package-exports package))
    (mapc #'mount-file (uiop:directory*
                        (uiop:merge-pathnames* uiop:*wild-file-for-directory*
                                               (uiop:parse-native-namestring path))))
    package))

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

(defun mount-file (path)
  "Mount PATH as a symbol in the appropriate Unix FS package."
  (setq path (ensure-path path))
  (bind (((directory . file) (ppath:split path))
         (package (or (find-package (convert-case directory)) (mount-directory directory))))
    (let ((symbol (ensure-homed-symbol (convert-case file) package)))
      (setq symbol (ensure-symbol-macro symbol `(access-file (symbol-path ',symbol))))
      (export symbol (symbol-package symbol))
      (ensure-executable symbol)
      symbol)))

;; TODO: something more clever, maybe `fswatch'
(defun remount-current-directory ()
  (mount-directory *default-pathname-defaults*))

(nhooks:add-hook *post-command-hook* 'remount-current-directory)

;;;; Structured file abstraction

(defun access-file (path)
  (if (uiop:directory-exists-p (uiop:parse-native-namestring path))
      (mount-directory (pathname-utils:force-directory path))
      (uiop:read-file-lines (uiop:parse-native-namestring path))))

(defun (setf access-file) (new-value path)
  (let ((kind (osicat:file-kind path :follow-symlinks t)))
    (when (eq kind :directory)
        (error 'wrong-file-kind :pathname path :kinds "not directory" :actual-kind kind)))
  (with-open-file
      (stream path :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (let ((read-stream (read-fd-stream new-value)))
      (unwind-protect
           (ignore-some-conditions (end-of-file)
             (loop (write-char (read-char read-stream) stream)))
        (close read-stream))))
  new-value)

(defmacro file (symbol)
  `(access-file (ensure-path ',symbol)))

(defun reintern-symbol (symbol)
  "Unintern SYMBOL, and intern a symbol with the same name and home
package as SYMBOL.  This is useful for \"clearing\" any bindings.
Code should then use the returned symbol in place of SYMBOL."
  (let ((package (symbol-package symbol)))
    (unintern symbol package)
    (intern (symbol-name symbol) package)))

(defun ensure-symbol-macro (symbol form)
  (let ((binding-type (sb-cltl2:variable-information symbol)))
    (cond ((or (not binding-type) (eq binding-type :symbol-macro))
           (eval `(define-symbol-macro ,symbol ,form)))
          (t (restart-case
                 (error "Symbol ~S already has a ~A binding." symbol binding-type)
               (reckless-continue () :report "Unintern the symbol and retry."
                 (ensure-symbol-macro (reintern-symbol symbol) form)))))))

(defmacro defile (symbol &optional initform)
  (setq symbol (mount-file (ensure-path symbol t)))
  (setq symbol (ensure-symbol-macro symbol `(access-file (symbol-path ',symbol))))
  `(progn
     ,(when initform `(setf ,symbol ,initform))
     ',symbol))

;;; FD watcher

(defvar *fd-watcher-thread* nil)
(defvar *fd-watcher-event-base* nil)

(defun fd-watcher ()
  (loop
    (restart-case
        (iolib:event-dispatch *fd-watcher-event-base*)
      (abort () :report "Abort processing current FD event."))))

(defun ensure-fd-watcher ()
  "Setup `*fd-watcher-thread*' and `*fd-watcher-event-base*'.
We mainly use them to interactively copy data between file descriptors
and Lisp streams. We don't use the implementation provided mechanisms
because they often have unsatisfying interactivity (e.g. as of SBCL
2.3.4, for quite a few cases the data is not transferred until the
entire input is seen, i.e. until EOF)."
  (unless (and *fd-watcher-event-base*
               (iolib/multiplex::fds-of *fd-watcher-event-base*))
    (setf *fd-watcher-event-base* (make-instance 'iolib:event-base)))
  (unless (and *fd-watcher-thread*
               (bt:thread-alive-p *fd-watcher-thread*))
    (setf *fd-watcher-thread* (bt:make-thread #'fd-watcher :name "Unix in Lisp FD watcher"))))

(defun cleanup-fd-watcher ()
  "Remove and close all file descriptors from `*fd-watcher-event-base*'.
This is mainly for debugger purpose, to clean up the mess when dubious
file descriptors are left open."
  (iter (for (fd _) in-hashtable (iolib/multiplex::fds-of *fd-watcher-event-base*))
    (iolib:remove-fd-handlers *fd-watcher-event-base* fd)
    (isys:close fd)))

(defun stop-fd-watcher ()
  "Destroy `*fd-watcher-thread*' and `*fd-watcher-event-base*'.
This is unsafe, for debug purpose only."
  (cleanup-fd-watcher)
  (bt:destroy-thread *fd-watcher-thread*)
  (close *fd-watcher-event-base*))

(defun copy-fd-to-stream (read-fd-or-stream stream &optional (continuation (lambda ())))
  "Copy characters from READ-FD-OR-STREAM to STREAM.
Characters are copied and FORCE-OUTPUT as soon as possible, making it
more suitable for interactive usage than some implementation provided
mechanisms."
  (declare (type function continuation))
  (bind (((:values read-fd read-stream)
          (if (streamp read-fd-or-stream)
              (values (sb-sys:fd-stream-fd read-fd-or-stream) read-fd-or-stream)
              (values read-fd-or-stream (sb-sys:make-fd-stream read-fd-or-stream :input t))))
         ((:labels clean-up ())
          (iolib:remove-fd-handlers *fd-watcher-event-base* read-fd)
          (close read-stream)
          (funcall continuation))
         (connection swank::*emacs-connection*)
         ((:labels read-data ())
          (swank::with-connection (connection)
            (handler-case
                (iter (for c = (read-char-no-hang read-stream))
                  (while c)
                  (write-char c stream)
                  (finally (force-output stream)))
              (end-of-file () (clean-up))
              (error (c) (describe c) (clean-up))))))
    (setf (isys:fd-nonblock-p read-fd) t)
    (ensure-fd-watcher)
    (iolib:set-io-handler
     *fd-watcher-event-base* read-fd
     :read
     (lambda (fd event error)
       (unless (eq event :read)
         (warn "FD watcher ~A get ~A ~A" fd event error))
       (read-data)))
    (values)))

;;; Job control

(defvar *jobs* nil)

;;; Effective Process

;;;; Abstract interactive process
(defclass process-mixin
    (native-lazy-seq:lazy-seq synchronized)
  ((status-change-hook
    :reader status-change-hook
    :initform (make-instance 'nhooks:hook-void))))

(defgeneric process-output (object)
  (:method ((object t))))
(defgeneric process-input (object)
  (:method ((object t))))
(defgeneric process-wait (object))
(defgeneric process-status (object))
(defgeneric description (object))

(defmethod print-object ((p process-mixin) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~A (~A)" (description p) (process-status p))))

(defmethod shared-initialize ((p process-mixin) slot-names &key)
  (setf (native-lazy-seq:generator p)
        (lambda ()
          (when (and (process-output p)
                     (open-stream-p (process-output p)))
            (handler-case
                (values (read-line (process-output p)) t)
              (end-of-file ()
                (close p)
                nil)))))
  (call-next-method))

(defmethod initialize-instance :around ((p process-mixin) &key)
  "Handle status change.
We use :AROUND method so that this method is called after the :AFTER
methods of any subclasses, to ensure status change hooks have been
setup before we add it to *jobs*."
  (call-next-method)
  (nhooks:add-hook
   (status-change-hook p)
   (make-instance 'nhooks:handler
                  :fn (lambda ()
                        (unless (member (process-status p) '(:running :stopped))
                          (deletef *jobs* p)))
                  :name 'remove-from-jobs))
  (when (member (process-status p) '(:running :stopped))
    (push p *jobs*)))

;;;; Simple process
;; Map 1-to-1 to UNIX process
(defclass simple-process (process-mixin)
  ((process :reader process :initarg :process)
   (description :reader description :initarg :description)))

(defmethod process-output ((p simple-process))
  (sb-ext:process-output (process p)))
(defmethod (setf process-output) (new-value (p simple-process))
  (setf (sb-ext:process-output (process p)) new-value))

(defmethod process-input ((p simple-process))
  (sb-ext:process-input (process p)))
(defmethod (setf process-input) (new-value (p simple-process))
  (setf (sb-ext:process-input (process p)) new-value))

(defmethod process-wait ((p simple-process))
  (sb-ext:process-wait (process p)))

(defmethod process-status ((p simple-process))
  (sb-ext:process-status (process p)))

(defvar *input-process-table* (make-hash-table :weakness :key)
  "Map input streams to simple-processes.
Used for removing close-input handler before closing input stream
to avoid race condition.")

(defmethod initialize-instance :after ((p simple-process) &key)
  (setf (sb-ext:process-status-hook (process p))
        (lambda (proc)
          (declare (ignore proc))
          (nhooks:run-hook (status-change-hook p))))
  ;; Grab input stream so that we can always close it,
  ;; even if other things set it to nil
  (when-let (input-stream (process-input p))
    (setf (gethash input-stream *input-process-table*) p)
    (flet ((close-input-maybe ()
             (when (member (process-status p) '(:exited :signaled))
               (close input-stream)
               (setf (process-input p) nil))))
      (nhooks:add-hook
          (status-change-hook p)
          (make-instance 'nhooks:handler
                         :fn #'close-input-maybe
                         :name 'close-input))
      (close-input-maybe))))

(defmethod close ((p simple-process) &key abort)
  (when abort
    (sb-ext:process-kill (process p) sb-unix:sigterm))
  (sb-ext:process-wait (process p))
  (sb-ext:process-close (process p))
  ;; SB-EXT:PROCESS-CLOSE may leave a closed stream.  Other part of
  ;; our code is not expecting this: `process-input'/`process-output'
  ;; shall either be open stream or nil, therefore we make sure to
  ;; set them to nil.
  (setf (process-input p) nil
        (process-output p) nil)
  t)

;;;; Pipeline
;; Consist of any number of UNIX processes and Lisp function stages
(defclass pipeline (process-mixin)
  ((processes :reader processes :initarg :processes)
   (process-input :accessor process-input :initarg :process-input)
   (process-output :accessor process-output :initarg :process-output)))

(defmethod process-wait ((p pipeline))
  (mapc #'process-wait (processes p)))

(defmethod close ((pipeline pipeline) &key abort)
  (iter (for p in (processes pipeline))
    (close p :abort abort)))

(defmethod initialize-instance :after ((p pipeline) &key)
  (mapc (lambda (child)
          ;; Remove children from *jobs* because the pipeline will be
          ;; put in *jobs* instead.
          (nhooks:remove-hook (status-change-hook child) 'remove-from-jobs)
          (deletef *jobs* child)
          (nhooks:add-hook
           (status-change-hook child)
           (make-instance
            'nhooks:handler
            :fn (lambda ()
                  (nhooks:run-hook (status-change-hook p)))
            :name 'notify-parent)))
        (processes p)))

(defmethod process-status ((p pipeline))
  (if (some (lambda (child) (eq (process-status child) :running))
            (processes p))
      :running
      :exited))

(defmethod description ((p pipeline))
  (serapeum:string-join (mapcar #'description (processes p)) ","))

;;;; Lisp process
(defclass lisp-process (process-mixin)
  ((thread :reader thread)
   (input :accessor process-input)
   (output :accessor process-output)
   (function :reader process-function)
   (status :reader process-status)
   (description :reader description :initarg :description))
  (:default-initargs :description "lisp"))

(defmethod initialize-instance
    ((p lisp-process) &key (function :function)
                        (input :stream) (output :stream) (error *trace-output*))
  (flet ((pipe ()
           (bind (((:values read-fd write-fd) (osicat-posix:pipe)))
             (values (sb-sys:make-fd-stream read-fd :input t :auto-close t)
                     (sb-sys:make-fd-stream write-fd :output t :auto-close t)))))
    (let (stdin stdout)
      (setf (values stdin (slot-value p 'input))
            (if (eq input :stream)
                (pipe)
                (values (read-fd-stream input) nil)))
      (setf (values (slot-value p 'output) stdout)
            (if (eq input :stream)
                (pipe)
                (values nil (write-fd-stream output))))
      (when (eq error :output)
        (setq error stdout))
      (setf (slot-value p 'function) function
            (slot-value p 'status) :running
            (slot-value p 'thread)
            (bt:make-thread
             (lambda ()
               (unwind-protect
                    (funcall function)
                 (close stdin)
                 (close stdout)
                 (setf (slot-value p 'status) :exited)
                 (nhooks:run-hook (status-change-hook p))))
             :initial-bindings
             `((*standard-input* . ,stdin)
               (*standard-output* . ,stdout)
               (*trace-output* . ,error)
               ,@bt:*default-special-bindings*)))))
  (call-next-method))

(defmethod process-wait ((p lisp-process))
  (ignore-errors (bt:join-thread (thread p))))

(defmethod close ((p lisp-process) &key abort)
  (when abort
    (bt:interrupt-thread
     (thread p)
     (lambda ()
       (sb-thread:abort-thread))))
  (ignore-errors (close (process-input p)))
  (ignore-errors (close (process-output p)))
  (ignore-errors (bt:join-thread (thread p))))

;;;; Process I/O streams

(defgeneric read-fd-stream (object)
  (:documentation "Return a fd-stream for reading cotents from OBJECT.
The returned fd-stream is intended to be passed to a child process,
and will be closed after child process creation.")
  (:method ((object (eql :stream))) :stream)
  (:method ((p process-mixin))
    (synchronized (p)
      (prog1
          (read-fd-stream (process-output p))
        ;; The consumer takes the output stream exclusively
        (setf (process-output p) nil))))
  (:method ((s sb-sys:fd-stream)) s)
  (:method ((s string))
    "Don't turn a STRING into lines of single characters."
    (read-fd-stream (list s)))
  (:method ((p sequence))
    (native-lazy-seq:with-iterators (element next endp) p
      (bind (((:values read-fd write-fd) (osicat-posix:pipe))
             ((:labels clean-up ())
              (iolib:remove-fd-handlers *fd-watcher-event-base* write-fd)
              (isys:close write-fd))
             ((:labels write-elements ())
              (let (more)
                (unwind-protect
                     (handler-case
                         (iter
                           (when (funcall endp)
                             (return-from write-elements nil))
                           (cffi:with-foreign-string
                               ((buf size)
                                (literal-to-string (funcall element)))
                             ;; Replace NUL with Newline
                             (setf (cffi:mem-ref buf :char (1- size)) 10)
                             (osicat-posix:write write-fd buf size))
                           (funcall next))
                       (osicat-posix:ewouldblock ()
                         (setf more t)))
                  (unless more (clean-up))))))
        (setf (isys:fd-nonblock-p write-fd) t)
        (ensure-fd-watcher)
        (iolib:set-io-handler
         *fd-watcher-event-base* write-fd
         :write
         (lambda (fd event error)
           (unless (eq event :write)
             (warn "FD watcher ~A get ~A ~A" fd event error))
           (write-elements)))
        (sb-sys:make-fd-stream read-fd :input t :auto-close t)))))

(defgeneric write-fd-stream (object)
  (:documentation "Return a fd-stream for writing cotents to OBJECT.
The returned fd-stream is intended to be passed to a child process,
and will be closed after child process creation.")
  (:method ((object (eql :stream))) :stream)
  (:method ((object (eql :output))) :output)
  (:method ((p process-mixin))
    (synchronized (p)
      (prog1
          (write-fd-stream (process-input p))
        ;; The producer takes the output stream exclusively
        (setf (process-input p) nil))))
  (:method ((s sb-sys:fd-stream)) s)
  (:method ((s stream))
    (bind (((:values read-fd write-fd) (osicat-posix:pipe)))
      (copy-fd-to-stream read-fd s)
      (setf (isys:fd-nonblock-p read-fd) t)
      (sb-sys:make-fd-stream write-fd :output t :auto-close t))))

(defgeneric repl-connect (object)
  (:method ((object t)))
  (:documentation "Display OBJECT more \"thoroughly\" than `print'.
Intended to be used at the REPL top-level to display the primary value
of evualtion results. Return T if we did any thing.  See the methods
for how we treat different types of objects."))

(defmethod repl-connect ((p process-mixin))
  "Connect `*standard-input*' and `*standard-output*' to P's input/output."
  (let ((repl-thread (bt:current-thread))
        ;; We take input/output of the process exclusively
        ;; TODO: proper mutex
        read-stream write-stream)
    (restart-case
        (unwind-protect
             (catch 'finish
               (synchronized (p)
                 (rotatef read-stream (process-output p))
                 (rotatef write-stream (process-input p)))
               (when read-stream
                 (copy-fd-to-stream
                  read-stream
                  *standard-output*
                  (lambda ()
                    (bt:interrupt-thread
                     repl-thread
                     (lambda ()
                       (ignore-errors (throw 'finish nil)))))))
               (cond (write-stream
                      (loop
                        (handler-case
                            (write-char (read-char) write-stream)
                          (end-of-file ()
                            (close write-stream)
                            (return)))
                        (force-output write-stream)))
                     (read-stream
                      ;; wait for output to finish reading
                      (loop (sleep 0.1)))
                     (t (return-from repl-connect nil))))
          (synchronized (p)
            (rotatef (process-output p) read-stream)
            (rotatef (process-input p) write-stream)))
      (background () :report "Run job in background.")
      (abort () :report "Abort job."
        (close p :abort t))))
  t)

(defmethod repl-connect ((s native-lazy-seq:lazy-seq))
  "Force evaluation of S and print each elements."
  (native-lazy-seq:with-iterators (element next endp) s
    (iter (until (funcall endp))
      (format t "~A~%" (funcall element))
      (force-output)
      (funcall next)))
  t)

;;; Fast loading command

(defvar *fast-load-functions*
  (make-instance 'nhooks:hook-any :combination #'nhooks:combine-hook-until-success))

(defun read-shebang (stream)
  (and (eq (read-char stream nil 'eof) #\#)
       (eq (read-char stream nil 'eof) #\!)))

(defun fast-load-sbcl-shebang (path args &key input output error directory)
  (let ((stream (open path :external-format :latin-1)))
    (if (ignore-errors
         (and (read-shebang stream)
              (string= (read-line stream) "/usr/bin/env sbcl --script")))
        (make-instance 'lisp-process
                       :function
                       (lambda ()
                         (unwind-protect
                              (uiop:with-current-directory (directory)
                                (let ((*default-pathname-defaults* directory))
                                  (with-standard-io-syntax
                                    (let ((*print-readably* nil) ;; good approximation to SBCL initial reader settings
                                          (sb-ext:*posix-argv* (cons (uiop:native-namestring path) args)))
                                      (load stream)))))
                           (close stream)))
                       :description (cdr (ppath:split path))
                       :input input :output output :error error)
        (progn
          (close stream)
          nil))))

(nhooks:add-hook *fast-load-functions* 'fast-load-sbcl-shebang)

;;; Command syntax

(defgeneric literal-to-string (object)
  (:documentation "Like `princ-to-string', but error when OBJECT is not \"literal\".
Use this when interfacing with Unix -- any complex S-expr
representation is unlikely to be recognized by Unix tools.")
  (:method ((symbol symbol))  (princ-to-string symbol))
  (:method ((seq sequence))
    "Zero element -> empty string.
One element -> that element.
More -> error."
    (native-lazy-seq:with-iterators (element next endp) seq
      (if (funcall endp)
          ""
          (let ((head (funcall element)))
            (funcall next)
            (if (funcall endp)
                head
                (error "More than 1 element in ~S." seq))))))
  (:method ((s string)) s)
  (:method ((n number)) (princ-to-string n)))

(defun split-args (args)
  "Split ARGS into keyword argument plist and other arguments.
Return two values: the plist of keywords and the list of other
arguments.
Example: (split-args a b :c d e) => (:c d), (a b e)"
  (iter (while args)
    (if (keywordp (car args))
        (progn
          (collect (car args) into plist)
          (collect (cadr args) into plist)
          (setq args (cddr args)))
        (progn
          (collect (car args) into rest)
          (setq args (cdr args))))
    (finally (return (values plist rest)))))

(defun execute-command (command args
                        &key (input :stream) (output :stream) (error *trace-output*))
  (let ((path (ensure-path command))
        (args (map 'list #'literal-to-string args))
        input-1 output-1 error-1)
    (flet ((close-maybe (s)
             (when (streamp s)
               (when-let (p (gethash s *input-process-table*))
                 (nhooks:remove-hook (status-change-hook p) 'close-input))
               ;; Now that the signal handler is gone, we have
               ;; exclusive access to S's state.
               (when (open-stream-p s)
                 (close s)))))
      (or (nhooks:run-hook *fast-load-functions* path args
                           :input input :output output :error error
                           :directory *default-pathname-defaults*)
          (unwind-protect
               (progn
                 (psetq input-1 (read-fd-stream input)
                        output-1 (write-fd-stream output)
                        error-1 (write-fd-stream error))
                 (make-instance
                  'simple-process
                  :process
                  (sb-ext:run-program
                   (uiop:parse-native-namestring path) args
                   :wait nil
                   :input input-1 :output output-1 :error error-1
                   :directory *default-pathname-defaults*
                   :environment (current-env))
                  :description (princ-to-string command)))
            (close-maybe input-1)
            (close-maybe output-1)
            (close-maybe error-1))))))

(defun command-macro (form env)
  (declare (ignore env)
           (sb-c::lambda-list (&rest args)))
  (bind (((command . args) form)
         ((:values plist command-args) (split-args args)))
    ;; The following macrolet make ,@<some-sequence> work, just like
    ;; ,@<some-list>.  This is done so that users can write
    ;; ,@<some-unix-command> easily, similar to POSIX shell command
    ;; substitutions.
    `(macrolet ((fare-quasiquote::append (&rest args)
                  `(append ,@ (mapcar (lambda (arg) `(coerce ,arg 'list)) args)))
                (fare-quasiquote::cons (x y)
                  `(cons ,x (coerce ,y 'list)))
                (fare-quasiquote::list* (&rest x)
                  `(list* ,@(butlast x) (coerce ,(lastcar x) 'list))))
       (execute-command
        ',command
        ,(list 'fare-quasiquote:quasiquote command-args)
        ,@plist))))

(defun placeholder-p (form)
  (and (symbolp form) (string= (symbol-name form) "_")))

(defmacro pipe (&rest forms)
  `(let (%processes)
     (push ,(car forms) %processes)
     ,@ (mapcar (lambda (form)
                  (if-let ((placeholder (and (listp form) (find-if #'placeholder-p form))))
                    `(push ,(substitute '(car %processes) placeholder form)
                           %processes)
                    `(push (,@form :input (car %processes))
                           %processes)))
                (cdr forms))
     (setq %processes (nreverse %processes))
     (make-instance 'pipeline
                    :processes (remove-if-not
                                (lambda (p) (typep p 'process-mixin))
                                %processes)
                    :process-input (process-input (car %processes))
                    :process-output (process-output (lastcar %processes)))))

;;; Built-in commands

(defmacro cd (&optional (path "~"))
  `(bind ((%path (pathname-utils:force-directory
                  (ensure-path ,(list 'fare-quasiquote:quasiquote path) t))))
     (setq *default-pathname-defaults* %path)))

;;; Reader syntax hacks

(defun call-without-read-macro (char thunk)
  (bind (((:values function terminating-p) (get-macro-character char)))
    (unwind-protect
         (progn (set-macro-character char nil t)
                (funcall thunk))
      (set-macro-character char function terminating-p))))

(defun dot-read-macro (stream char)
  (flet ((delimiter-p (c)
           (or (eq c 'eof) (sb-impl:token-delimiterp c)))
         (unread (c)
           (unless (eq c 'eof) (unread-char c stream)))
         (standard-read ()
           (call-without-read-macro #\. (lambda () (read stream)))))
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
           (standard-read))))))

(defun slash-read-macro (stream char)
  "If we're reading a symbol whose name is an *existing* Unix filename,
return the corresponding mounted symbol.  Otherwise return the original symbol.
Currently, this is intended to be used for *both* /path syntax and
~user/path syntax."
  (unread-char char stream)
  (let ((symbol (call-without-read-macro char (lambda () (read stream)))))
    (if (symbol-home-p symbol)
        (let ((path (ensure-path (unconvert-case (symbol-name symbol)))))
          (if (ppath:lexists path)
              (progn
                ;; avoid polluting package
                (unintern symbol)
                (mount-file (symbol-name symbol)))
              symbol))
        symbol)))

(defun dollar-read-macro (stream char)
  "If we're reading a symbol that starts with `$', rehome it to
`UNIX-IN-LISP.COMMON' and call `ensure-env-var'."
  (unread-char char stream)
  (let ((symbol (call-without-read-macro char (lambda () (read stream)))))
    (when (and (symbol-home-p symbol)
               (string-prefix-p "$" (symbol-name symbol)))
      (unintern symbol)
      (setq symbol (intern (symbol-name symbol) "UNIX-IN-LISP.COMMON"))
      (export symbol "UNIX-IN-LISP.COMMON")
      (ensure-env-var symbol))
    symbol))

(named-readtables:defreadtable unix-in-lisp
  (:merge :standard)
  (:macro-char #\. 'dot-read-macro t)
  (:macro-char #\/ 'slash-read-macro t)
  (:macro-char #\~ 'slash-read-macro t)
  (:macro-char #\$ 'dollar-read-macro t)

  (:case :invert))

(fare-quasiquote:enable-quasiquote
 :expansion-time 'macroexpand
 :table (named-readtables:find-readtable 'unix-in-lisp))

(defun unquote-reader-hook (orig thunk)
  (if (= fare-quasiquote::*quasiquote-level* 0)
      (let ((fare-quasiquote::*quasiquote-level* 1))
        (funcall orig thunk))
      (funcall orig thunk)))

;;; Top-level lexification

(defun intern-hook (orig &rest args)
  "If we see symbol corresponding to a existing file,
create a new uninterned symbol that merge bindings from the file
symbol and the actual symbol."
  (multiple-value-bind (symbol status) (apply orig args)
    (if (and (eq (named-readtables:readtable-name *readtable*)
                 'unix-in-lisp)
             (symbol-home-p symbol))
        (let ((path (ensure-path symbol t)))
          (if (ppath:lexists path)
              (bind ((file-symbol (mount-file path))
                     (new-symbol (make-symbol (symbol-name symbol))))
                (when (not (sb-cltl2:variable-information 'x))
                  (ensure-symbol-macro new-symbol file-symbol))
                (when (and (not (fboundp symbol)) (fboundp file-symbol))
                  (setf (macro-function new-symbol)
                        (macro-function file-symbol)))
                (values new-symbol :internal))
              (values symbol status)))
        (values symbol status))))

(defmacro toplevel (&body body)
  "Evaluate BODY, but with ergonomics improvements for using as a shell.
1. Use `repl-connect' to give returned primary value special
treatment if possible."
  `(let ((result (multiple-value-list (progn ,@body))))
     (when (repl-connect (car result))
       (pop result))
     (multiple-value-prog1 (values-list result)
       (nhooks:run-hook *post-command-hook*))))

;;; Environment variables

(defvar *env-vars* nil "An ALIST that maps symbols to Unix environment variable name strings.")

(defun ensure-env-var (symbol &optional unix-name)
  "Associate SYMBOL with Unix environment variable with UNIX-NAME.
If UNIX-NAME is nil or not provided, the SYMBOL must follow $FOO naming
convention and UNIX-NAME defaults to FOO."
  (unless (string-prefix-p "$" (symbol-name symbol))
    (warn "~S is being defined as a Unix environment variable, but its name does
not follow usual convention (like $~A)." symbol (symbol-name symbol)))
  (unless unix-name
    (if (string-prefix-p "$" (symbol-name symbol))
        (setq unix-name (subseq (symbol-name symbol) 1))
        (error "Please supply a Unix environment variable name for ~S, or use a symbol
that follow usual naming convention (like $~A)." symbol (symbol-name symbol))))
  (proclaim `(special ,symbol))
  (unless (boundp 'symbol)
    (setf (symbol-value symbol) (or (uiop:getenv unix-name) "")))
  (setf (assoc-value *env-vars* symbol) unix-name)
  symbol)

(defun current-env ()
  "Construct Unix environment according to Lisp symbol bindings.
The result is a list of strings with the form \"VAR=VALUE\", as in
environ(7)."
  (iter (for (symbol . name) in *env-vars*)
    (when (boundp symbol)
      (collect (concat name "=" (literal-to-string (symbol-value symbol)))))))

(defun synchronize-env-to-unix ()
  "Update the Unix environment of the Lisp image to reflect current Lisp
symbol bindings."
  (iter (for (symbol . name) in *env-vars*)
    (if (boundp symbol)
        (setf (uiop:getenv name) (literal-to-string (symbol-value symbol)))
        (sb-posix:unsetenv name))))

(nhooks:add-hook *post-command-hook* 'synchronize-env-to-unix)

;;; Installation/uninstallation

(define-condition already-installed (error) ()
  (:report "There seems to be a previous Unix in Lisp installation."))

(defun get-env-names ()
  (mapcar (lambda (s)
            (subseq s 0 (position #\= s)))
          (sb-ext:posix-environ)))

(defun install ()
  (when (find-package "UNIX-IN-LISP.PATH")
    (restart-case (error 'already-installed)
      (continue () :report "Uninstall first, then reinstall." (uninstall))
      (reckless-continue () :report "Install on top of it.")))
  (sb-ext:without-package-locks
    (cl-advice:add-advice :around 'sb-impl::%intern 'intern-hook))
  (let ((*readtable* (named-readtables:find-readtable 'unix-in-lisp)))
    ;;  Create UNIX-IN-LISP.PATH from $PATH
    (let ((packages (iter (for path in (uiop:getenv-pathnames "PATH"))
                      (handler-case
                          (collect (mount-directory (pathname-utils:force-directory path)))
                        (file-error (c) (warn "Failed to mount ~A in $PATH: ~A" path c))))))
      (uiop:ensure-package :unix-in-lisp.path :mix packages :reexport packages))
    ;; Populate UNIX-IN-LISP.COMMON
    (uiop:ensure-package :unix-in-lisp.common
                         :mix '(:unix-in-lisp.path)
                         :reexport '(:unix-in-lisp.path))
    (let ((*package* (find-package :unix-in-lisp.common)))
      (mapc (lambda (name)
              (let ((symbol (intern (concat "$" name))))
                (ensure-env-var symbol name)
                (export symbol)))
            (get-env-names)))
    (defmethod print-object :around ((symbol symbol) stream)
      (if (and *print-escape*
               (eq (named-readtables:readtable-name *readtable*)
                   'unix-in-lisp))
          (cond ((eq (find-symbol (symbol-name symbol) *package*) symbol) (call-next-method))
                ((not (symbol-package symbol)) (call-next-method))
                ((package-path (symbol-package symbol))
                 (write-string (symbol-path symbol) stream))
                (t (call-next-method)))
          (call-next-method)))
    (cl-advice:add-advice :around 'fare-quasiquote:call-with-unquote-reader 'unquote-reader-hook)
    (cl-advice:add-advice :around 'fare-quasiquote:call-with-unquote-splicing-reader 'unquote-reader-hook)
    (ensure-fd-watcher)
    t))

(defun uninstall ()
  (cl-advice:remove-advice :around 'fare-quasiquote:call-with-unquote-reader 'unquote-reader-hook)
  (cl-advice:remove-advice :around 'fare-quasiquote:call-with-unquote-splicing-reader 'unquote-reader-hook)
  (when-let (method (find-method #'print-object '(:around) '(symbol t) nil))
    (remove-method #'print-object method))
  ;; Delete all Unix FS packages
  (mapc
   (lambda (p)
     (when (package-path p)
       (handler-bind ((package-error #'continue))
         (delete-package p))))
   (list-all-packages))
  ;; Reap UNIX-IN-LISP.COMMON of its content
  (let ((*package* (find-package :unix-in-lisp.common)))
    (unuse-package :unix-in-lisp.path)
    (do-symbols (sym)
      (unintern sym)
      ;; Do a best effort at removing shadowing symbols
      ;; created by `:mix' option of `uiop:define-package'
      (dolist (p (package-used-by-list *package*))
        (unintern sym p))))
  ;; Delete UNIX-IN-LISP.PATH
  (when (find-package :unix-in-lisp.path)
    (delete-package :unix-in-lisp.path))
  (sb-ext:without-package-locks
    (cl-advice:remove-advice :around 'sb-impl::%intern 'intern-hook))
  (values))

(defun setup ()
  (ignore-some-conditions (already-installed) (install))
  (in-package :unix-user)
  (named-readtables:in-readtable unix-in-lisp)
  (cd)
  (values))
