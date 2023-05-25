(in-package #:unix-in-lisp)
(named-readtables:in-readtable :standard)

(defun swank-untokenize-symbol-hook (orig package-name internal-p symbol-name)
  (cond ((and (string-prefix-p "/" package-name) (not internal-p)
              (eq (named-readtables:readtable-name swank::*buffer-readtable*)
                  'unix-in-lisp))
         (if (and (unix-in-slime-p)
                  (string-prefix-p (uiop:native-namestring *default-pathname-defaults*) package-name))
             (concat (subseq package-name (length (uiop:native-namestring *default-pathname-defaults*)))
                     symbol-name)
             (concat package-name symbol-name)))
        (t (funcall orig package-name internal-p symbol-name))))

(defun swank-tokenize-symbol-convert (symbol-name package-name internal-p convert-case-p)
  (flet ((convert-maybe (s)
           (if convert-case-p (convert-case s) s))
         (unconvert-maybe (s)
           (if convert-case-p (unconvert-case s) s)))
    (handler-case
        (cond ((and (not package-name)
                    (eq (named-readtables:readtable-name swank::*buffer-readtable*)
                        'unix-in-lisp)
                    (or (string-prefix-p "~" symbol-name)
                        (string-prefix-p "/" symbol-name)
                        (and (unix-in-slime-p)
                             (find #\/ symbol-name))))
               (bind (((dir . file) (ppath:split (unconvert-maybe symbol-name))))
                 (when (unix-in-slime-p)
                   (setq dir (to-dir (ensure-path dir t))))
                 (let ((*readtable* (named-readtables:find-readtable 'unix-in-lisp)))
                   (ignore-errors (mount-directory dir)))
                 (values (convert-maybe file) (convert-maybe dir) nil)))
              (t (values symbol-name package-name internal-p)))
      (error () (values symbol-name package-name internal-p)))))

(defun swank-tokenize-symbol-hook (orig string)
  (multiple-value-call #'swank-tokenize-symbol-convert (funcall orig string) nil))

(defun swank-tokenize-symbol-thoroughly-hook (orig string)
  (multiple-value-call #'swank-tokenize-symbol-convert (funcall orig string) t))

(defun swank-fuzzy-find-matching-symbols-hook (orig string package &rest args)
  (if (and (eq package swank::*buffer-package*)
           (unix-in-slime-p))
      (bind (((:values matchings time-limit)
              (apply orig string (mount-directory *default-pathname-defaults*) args))
             ((:values matchings-1 time-limit-1)
              (apply orig string package :time-limit-in-msec time-limit args)))
        (values (concatenate 'vector matchings matchings-1) time-limit-1))
      (apply orig string package args)))

(defun swank-find-matching-symbols-hook (orig string package &rest args)
  (if (and (eq package swank::*buffer-package*) (unix-in-slime-p))
      (bind ((matchings (apply orig string (mount-directory *default-pathname-defaults*) args))
             (matchings-1 (apply orig string package args)))
        (nconc matchings matchings-1))
      (apply orig string package args)))

(defun swank-matching-symbols-hook (orig package &rest args)
  (if (and (eq package swank::*buffer-package*) (unix-in-slime-p))
      (bind ((matchings (apply orig (mount-directory *default-pathname-defaults*) args))
             (matchings-1 (apply orig package args)))
        (nconc matchings matchings-1))
      (apply orig package args)))

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
  (bind (((:values values -) (funcall orig string)))
    (if (unix-in-slime-p)
        (progn
          (nhooks:run-hook *post-command-hook*)
          (if (repl-connect (car values))
              (values (cdr values) -)
              (values values -)))
        (values values -))))

(defun swank-track-package-hook (orig fun)
  "Always update prompt instead only when *PACKAGE* change,
because *DEFAULT-PATHNAME-DEFAULTS* might also change."
  (if (unix-in-slime-p)
      (unwind-protect (funcall fun)
        (swank::send-to-emacs (list :new-package (package-name *package*)
                                    (swank::package-string-for-prompt *package*))))
      (funcall orig fun)))

(defun swank-package-string-for-prompt-hook (orig package)
  (if (unix-in-slime-p)
      (concat (funcall orig package) " "
              (uiop:native-namestring *default-pathname-defaults*))
      (funcall orig package)))

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

(defun swank-repl-loop-hook (connection)
  (declare (ignore connection))
  (setup))

(defun swank-create-repl-hook (orig &rest args)
  "Ugly hack to make the prompt on first start up display reasonably.
Otherwise for some reason the `*package*' is not setup when M-x unix-in-slime
is called the first time???"
  (if (unix-in-slime-p)
      (let ((*package* (find-package :unix-user))
            (*default-pathname-defaults*
              (uiop:parse-native-namestring (to-dir (ensure-path "~/")))))
        (apply orig args))
      (apply orig args)))

(defvar *swank-hooks* nil)

(defun safe-add-advice (where function advice)
  (if (fboundp function)
      (progn
        (cl-advice:add-advice where function advice)
        (push (list where function advice) *swank-hooks*))
      (warn "~S is not fbound, not adding ~S." function advice)))

(defun slime-install ()
  (safe-add-advice :around 'swank::add-connection 'swank-add-connection-hook)

  (safe-add-advice :around 'swank::untokenize-symbol 'swank-untokenize-symbol-hook)
  (safe-add-advice :around 'swank::tokenize-symbol 'swank-tokenize-symbol-hook)

  (safe-add-advice :around 'swank::tokenize-symbol-thoroughly 'swank-tokenize-symbol-thoroughly-hook)
  (safe-add-advice :around 'swank::fuzzy-find-matching-symbols 'swank-fuzzy-find-matching-symbols-hook)
  (safe-add-advice :around 'swank::find-matching-symbols 'swank-find-matching-symbols-hook)
  (safe-add-advice :around 'swank::matching-symbols 'swank-matching-symbols-hook)

  (safe-add-advice :around 'swank-repl::eval-region 'swank-eval-region-hook)
  (safe-add-advice :around 'swank-repl::track-package 'swank-track-package-hook)
  (safe-add-advice :around 'swank-repl::package-string-for-prompt 'swank-package-string-for-prompt-hook)

  (safe-add-advice :around 'swank-repl::globally-redirect-io-p 'swank-globally-redirect-io-p-hook)

  (setq swank::*auto-abbreviate-dotted-packages* nil)
  (setq swank:*default-worker-thread-bindings*
        `((*readtable* . ,*readtable*)
          (*default-pathname-defaults* . ,*default-pathname-defaults*)))

  (safe-add-advice :before 'swank-repl::repl-loop 'swank-repl-loop-hook)
  (safe-add-advice :around 'swank-repl::create-repl 'swank-create-repl-hook))

(defun slime-uninstall ()
  (iter (while *swank-hooks*)
    (apply #'cl-advice:remove-advice (pop *swank-hooks*))))
(cl-advice:add-advice :after 'install 'slime-install)
(cl-advice:add-advice :before 'uninstall 'slime-uninstall)

(defun ensure-swank-server (default-port)
  "Make sure a swank server for Unix in SLIME is running.
Return its port.  Start one on DEFAULT-PORT if none is running yet."
  (ignore-some-conditions (already-installed) (install))
  (ensure *swank-port*
    (swank:create-server :port default-port :dont-close t)))
