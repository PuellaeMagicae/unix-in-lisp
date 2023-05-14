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

(defun swank-mrepl-read-eval-print-hook (orig string)
  (declare (ignore orig))
  (with-input-from-string (in string)
    (setq / ())
    (loop
      (let* ((form (read in nil in)))
	(cond ((eq form in) (return))
	      (t (setq / (multiple-value-list (eval `(toplevel ,(setq + form)))))))))
    (force-output)
    (if /
	(format nil "~{~s~%~}" /)
	"; No values")))

(defun slime-install ()
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

  (cl-advice:add-advice :around #+swank 'swank-mrepl::read-eval-print
                                'swank-mrepl-read-eval-print-hook))
(defun slime-uninstall ()
  (cl-advice:remove-advice :around #+swank 'swank-mrepl::read-eval-print
                                   'swank-mrepl-read-eval-print-hook)

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
