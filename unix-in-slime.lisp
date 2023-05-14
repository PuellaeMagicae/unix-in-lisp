(in-package #:unix-in-lisp)

(defun swank-untokenize-symbol-hook (orig package-name internal-p symbol-name)
  (cond ((and (ppath:isabs package-name) (not internal-p))
         (ppath:join package-name symbol-name))
        (t (funcall orig package-name internal-p symbol-name))))

(defun swank-tokenize-symbol-hook (orig string)
  (multiple-value-bind (symbol-name package-name internal-p)
      (funcall orig string)
    (cond ((and (not package-name) (ppath:isabs symbol-name))
           (bind (((dir . file) (ppath:split symbol-name)))
             (ignore-some-conditions (file-error)
               (mount-directory dir))
             (values file dir nil)))
          ((and (not package-name) (package-path swank::*buffer-package*)
                (find #\/ symbol-name))
           (bind (((dir . file) (ppath:split
                                 (ppath:normpath
                                  (ppath:join (package-path swank::*buffer-package*)
                                              symbol-name)))))
             (ignore-some-conditions (file-error)
               (mount-directory dir))
             (values file dir nil)))
          (t (values symbol-name package-name internal-p)))))

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
  ;; TODO: is it correct to use the same advice for both?
  ;; Difference: `swank::tokenize-symbol-thoroughly' handles escape characters
  ;; I feel like at least one of the two uses is subtlely wrong
  (cl-advice:add-advice :around #+swank 'swank::tokenize-symbol-thoroughly
                                #+slynk 'slynk::tokenize-symbol-thoroughly
                                'swank-tokenize-symbol-hook)

  (cl-advice:add-advice :around #+swank 'swank-mrepl::read-eval-print
                                'swank-mrepl-read-eval-print-hook))
(defun slime-uninstall ()
  (cl-advice:remove-advice :around #+swank 'swank-mrepl::read-eval-print
                                   'swank-mrepl-read-eval-print-hook)

  (cl-advice:remove-advice :around #+swank 'swank::tokenize-symbol-thoroughly
                                   #+slynk 'slynk::tokenize-symbol-thoroughly
                                   'swank-tokenize-symbol-hook)
  (cl-advice:remove-advice :around #+swank 'swank::tokenize-symbol
                                   #+slynk 'slynk::tokenize-symbol
                                   'swank-tokenize-symbol-hook)

  (cl-advice:remove-advice :around #+swank 'swank::untokenize-symbol
                                   #+slynk 'slynk::untokenize-symbol
                                   'swank-untokenize-symbol-hook))
(cl-advice:add-advice :after 'install 'slime-install)
(cl-advice:add-advice :before 'uninstall 'slime-uninstall)
