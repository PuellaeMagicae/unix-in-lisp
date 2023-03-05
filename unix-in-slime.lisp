(in-package #:unix-in-lisp)

(defun swank-untokenize-symbol-hook (orig package-name internal-p symbol-name)
  (cond ((and (ppath:isabs package-name) (not internal-p))
         (ppath:join package-name symbol-name))
        (t (funcall orig package-name internal-p symbol-name))))
(defun swank-tokenize-symbol-hook (orig string)
  (multiple-value-bind (symbol-name package-name internal-p)
      (funcall orig string)
    (cond ((and (not package-name) (ppath:isabs symbol-name))
           (bind (((dir . file)(ppath:split symbol-name)))
             (values file dir nil)))
          (t (values symbol-name package-name internal-p)))))

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
                                'swank-tokenize-symbol-hook))
(defun slime-uninstall ()
  (cl-advice:remove-advice :around 'swank::tokenize-symbol-thoroughly 'swank-tokenize-symbol-hook)
  (cl-advice:remove-advice :around 'swank::tokenize-symbol 'swank-tokenize-symbol-hook)
  (cl-advice:remove-advice :around 'swank::untokenize-symbol 'swank-untokenize-symbol-hook))
(cl-advice:add-advice :after 'install 'slime-install)
(cl-advice:add-advice :before 'uninstall 'slime-uninstall)
