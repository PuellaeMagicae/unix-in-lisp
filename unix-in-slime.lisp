(in-package #:unix-in-lisp)

(defun swank-untokenize-symbol-hook (orig package-name internal-p symbol-name)
  (cond ((and (string-prefix-p "/" package-name) (not internal-p))
         (concat package-name symbol-name))
        (t (funcall orig package-name internal-p symbol-name))))

(defun swank-tokenize-symbol-convert (symbol-name package-name internal-p convert-case-p)
  (flet ((convert-maybe (s)
           (if convert-case-p (convert-case s) s))
         (unconvert-maybe (s)
           (if convert-case-p (unconvert-case s) s)))
    (handler-case
        (cond ((and (not package-name)
                    (or (string-prefix-p "~" symbol-name)
                        (string-prefix-p "/" symbol-name)
                        (and (unix-in-slime-p)
                             (find #\/ symbol-name))))
               (bind ((path (uiop:parse-native-namestring (unconvert-maybe symbol-name)))
                      (dir (ensure-path (pathname-utils:to-directory path) (unix-in-slime-p)))
                      (file (pathname-utils:to-file path)))
                 (let ((*readtable* (named-readtables:find-readtable 'unix-in-lisp)))
                   (ignore-some-conditions (file-error)
                           (mount-directory dir)))
                 (values (convert-maybe (uiop:native-namestring file))
                         (convert-maybe (uiop:native-namestring dir))
                         nil)))
              (t (values symbol-name package-name internal-p)))
      (error () (values symbol-name package-name internal-p)))))

(defun swank-tokenize-symbol-hook (orig string)
  (multiple-value-call #'swank-tokenize-symbol-convert (funcall orig string) nil))

(defun swank-tokenize-symbol-thoroughly-hook (orig string)
  (multiple-value-call #'swank-tokenize-symbol-convert (funcall orig string) t))

(defvar *swank-port* nil)

(defun unix-in-slime-p (&optional (conn swank::*emacs-connection*))
  (when (and conn *swank-port*)
    (bind ((s (swank::connection.socket conn))
           ((:values addr port)
            (and (> (sb-bsd-sockets:socket-file-descriptor s) 0)
                 (sb-bsd-sockets:socket-name s))))
      (and (equalp addr #(127 0 0 1))
           (= port *swank-port*)))))

(defun swank-eval-region-hook (orig string)
  "Wrap forms to be evaluated with `toplevel' macro."
  (if (unix-in-slime-p)
      (with-input-from-string (stream string)
        (let (- values)
          (loop
            (let ((form (read stream nil stream)))
              (when (eq form stream)
                (finish-output)
                (return (values values -)))
              (setq - form)
              (setq values (multiple-value-list (eval `(toplevel ,form))))
              (finish-output)))))
      (funcall orig string)))

(defun swank-track-package-hook (orig fun)
  (if (unix-in-slime-p)
      (unwind-protect (funcall fun)
        (swank::send-to-emacs
         (list :new-package (package-name *package*)
               (concat (swank::package-string-for-prompt *package*) " "
                       (uiop:native-namestring *default-pathname-defaults*)))))
      (funcall orig fun)))

(defun swank-add-connection-hook (orig conn)
  "Add Unix in SLIME connection to the last of `swank::*connections*',
so that we never automatically become the default REPL."
  (if (unix-in-slime-p conn)
      (swank::with-lock swank::*connection-lock*
        (alexandria:nconcf swank::*connections* (list conn)))
      (funcall orig conn)))

(defun swank-globally-redirect-io-p-hook (orig)
  "Disable swank global redirection mechanism for Unix in Slime
connection, because it does not support multiple listeners well
(its synonym streams redirect to global variables and are not
thread-local/connection-local)."
  (unless (unix-in-slime-p)
    (funcall orig)))

(defun slime-install (&optional skip-installed)
  (cl-advice:add-advice :around 'swank::add-connection 'swank-add-connection-hook)

   (cl-advice:add-advice :around 'swank::untokenize-symbol 'swank-untokenize-symbol-hook)
   (cl-advice:add-advice :around 'swank::tokenize-symbol 'swank-tokenize-symbol-hook)
   ;; Difference: `swank::tokenize-symbol-thoroughly' handles escape characters
   ;; I feel like at least one of the two uses is subtlely wrong
  (cl-advice:add-advice :around 'swank::tokenize-symbol-thoroughly 'swank-tokenize-symbol-thoroughly-hook)

  (cl-advice:add-advice :around 'swank-repl::eval-region 'swank-eval-region-hook)
  (cl-advice:add-advice :around 'swank-repl::track-package 'swank-track-package-hook)

  (cl-advice:add-advice :around 'swank-repl::globally-redirect-io-p 'swank-globally-redirect-io-p-hook)

  (setq swank::*auto-abbreviate-dotted-packages* nil)
  (setq swank:*default-worker-thread-bindings* `((*readtable* . ,*readtable*))))

(defun slime-uninstall ()
  (cl-advice:remove-advice :around 'swank-repl::globally-redirect-io-p 'swank-globally-redirect-io-p-hook)

  (cl-advice:remove-advice :around 'swank-repl::track-package 'swank-track-package-hook)
  (cl-advice:remove-advice :around 'swank-repl::eval-region 'swank-eval-region-hook)

  (cl-advice:remove-advice :around 'swank::tokenize-symbol-thoroughly)
  (cl-advice:remove-advice :around 'swank::tokenize-symbol 'swank-tokenize-symbol-hook)
  (cl-advice:remove-advice :around 'swank::untokenize-symbol 'swank-untokenize-symbol-hook)

  (cl-advice:remove-advice :around 'swank::add-connection 'swank-add-connection-hook))
(cl-advice:add-advice :after 'install 'slime-install)
(cl-advice:add-advice :before 'uninstall 'slime-uninstall)

(defun ensure-swank-server (default-port)
  "Make sure a swank server for Unix in SLIME is running.
Return its port.  Start one on DEFAULT-PORT if none is running yet."
  (install t)
  (ensure *swank-port*
    (swank:create-server :port default-port :dont-close t)))
