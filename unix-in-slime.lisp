(in-package #:unix-in-lisp)

(defun swank-untokenize-symbol-hook (orig package-name internal-p symbol-name)
  (cond ((and (ppath:isabs package-name) (not internal-p))
         (ppath:join package-name symbol-name))
        (t (funcall orig package-name internal-p symbol-name))))

(defun swank-tokenize-symbol-convert (symbol-name package-name internal-p convert-case-p)
  (flet ((convert-maybe (s)
           (if convert-case-p (convert-case s) s))
         (unconvert-maybe (s)
           (if convert-case-p (unconvert-case s) s)))
    (handler-case
        (cond ((and (not package-name)
                    (or (string-prefix-p "~" symbol-name)
                        (ppath:isabs symbol-name)))
               (bind (((dir . file)
                       (ppath:split (ensure-path (unconvert-maybe symbol-name)))))
                 (ignore-some-conditions (file-error)
                   (mount-directory dir))
                 (values (convert-maybe file) (convert-maybe dir) nil)))
              ((and (not package-name) (package-path swank::*buffer-package*)
                    (find #\/ symbol-name))
               (bind (((dir . file)
                       (ppath:split
                        (ensure-path
                         (ppath:join (package-path swank::*buffer-package*)
                                     (unconvert-maybe symbol-name))))))
                 (ignore-some-conditions (file-error)
                   (mount-directory dir))
                 (values (convert-maybe file) (convert-maybe dir) nil)))
              (t (values symbol-name package-name internal-p)))
      (error () (values symbol-name package-name internal-p)))))

(defun swank-tokenize-symbol-hook (orig string)
  (multiple-value-call #'swank-tokenize-symbol-convert (funcall orig string) nil))

(defun swank-tokenize-symbol-thoroughly-hook (orig string)
  (multiple-value-call #'swank-tokenize-symbol-convert (funcall orig string) t))

(defvar *swank-port* nil)

(defun unix-in-slime-p (&optional (conn swank-api:*emacs-connection*))
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
  (cl-advice:add-advice :around #+swank 'swank::untokenize-symbol
                                #+slynk 'slynk::untokenize-symbol
                                'swank-untokenize-symbol-hook)

  (cl-advice:add-advice :around #+swank 'swank::tokenize-symbol
                                #+slynk 'slynk::tokenize-symbol
                                'swank-tokenize-symbol-hook)
  ;; Difference: `swank::tokenize-symbol-thoroughly' handles escape characters
  ;; I feel like at least one of the two uses is subtlely wrong
  (cl-advice:add-advice :around #+swank 'swank::tokenize-symbol-thoroughly
                                #+slynk 'slynk::tokenize-symbol-thoroughly
                                'swank-tokenize-symbol-thoroughly-hook)

  (cl-advice:add-advice :around #+swank 'swank::add-connection
                                'swank-add-connection-hook)

  (cl-advice:add-advice :around #+swank 'swank-repl::eval-region
                                'swank-eval-region-hook)
  (cl-advice:add-advice :around #+swank 'swank-repl::globally-redirect-io-p
                                'swank-globally-redirect-io-p-hook)

  (setq swank::*auto-abbreviate-dotted-packages* nil))

(defun slime-uninstall ()
  (cl-advice:remove-advice :around #+swank 'swank-repl::globally-redirect-io-p
                                'swank-globally-redirect-io-p-hook)
  (cl-advice:remove-advice :around #+swank 'swank-repl::eval-region
                                   'swank-eval-region-hook)

  (cl-advice:remove-advice :around #+swank 'swank::add-connection
                                'swank-add-connection-hook)

  (cl-advice:remove-advice :around #+swank 'swank::tokenize-symbol-thoroughly
                                   #+slynk 'slynk::tokenize-symbol-thoroughly
                                   'swank-tokenize-symbol-thoroughly-hook)
  (cl-advice:remove-advice :around #+swank 'swank::tokenize-symbol
                                   #+slynk 'slynk::tokenize-symbol
                                   'swank-tokenize-symbol-hook)

  (cl-advice:remove-advice :around #+swank 'swank::untokenize-symbol
                                   #+slynk 'slynk::untokenize-symbol
                                   'swank-untokenize-symbol-hook))
(cl-advice:add-advice :after 'install 'slime-install)
(cl-advice:add-advice :before 'uninstall 'slime-uninstall)
