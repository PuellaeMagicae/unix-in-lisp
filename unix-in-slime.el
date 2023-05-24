;;; unix-in-slime.el --- Unix in Lisp support for SLIME -*- lexical-binding: t; -*-

;; Author: Sakurakouji Sena <qhong@alum.mit.edu>
;; Maintainer: Sakurakouji Sena <qhong@alum.mit.edu>
;; Package-Requires: ((emacs "28") (slime "2.27"))
;; Keywords: lisp
;; URL: https://gitub.com/PuellaeMagicae/unix-in-lisp
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements Unix in SLIME listener based on the SLIME
;; listener.

;;; Code:


(require 'ansi-color)
(require 'nadvice)
(require 'slime)

(defgroup unix-in-slime nil
  "SLIME frontend for Unix in Lisp"
  :group 'applications)

(defcustom unix-in-slime-default-port 4010
  "Default port to start up Unix in SLIME server."
  :type 'integer
  :group 'unix-in-slime)

(defvar unix-in-slime-port nil)

(define-advice slime-repl-emit
    (:after (_string) unix-in-slime)
  "Add ANSI colors and escape sequence to the SLIME REPL."
  (when (unix-in-slime-p)
    (with-current-buffer (slime-output-buffer)
      (comint-carriage-motion slime-output-start slime-output-end)
      (ansi-color-apply-on-region slime-output-start slime-output-end))))

;;;###autoload
(defun unix-in-slime ()
  "Create a SLIME listener running Unix in Lisp.
This ensures a SLIME session is running from the current Emacs
instance, and creates a swank server dedicated to Unix in Lisp
listening on `unix-in-slime-default-port'."
  (interactive)
  (if (slime-connected-p)
      ;; If `slime-current-thread'=`:repl-thread', creation of Unix in
      ;; SLIME listeners (the `slime-eval-async' call in particular)
      ;; will be blocked by the current REPL evaluation request.
      ;; Therefore we rebind it to `t'.
      (let ((slime-current-thread t))
        (slime-eval-async
            `(cl:progn
              (asdf:require-system "unix-in-lisp")
              (cl:funcall (cl:find-symbol "ENSURE-SWANK-SERVER" "UNIX-IN-LISP")
                          ,unix-in-slime-default-port))
          (lambda (port)
            (setq unix-in-slime-port port)
            ;; don't let `slime-connect' change default connection
            (let ((slime-default-connection slime-default-connection))
              (slime-connect "localhost" port)))))
    (save-selected-window (slime-start :init-function #'unix-in-slime))))

(defun unix-in-slime-p ()
  (when (and unix-in-slime-port (ignore-errors (slime-connection)))
    (equal (process-contact (slime-connection))
           (list "localhost" unix-in-slime-port))))

(defun unix-in-slime-disconnect-maybe ()
  (when (and (derived-mode-p 'slime-repl-mode) (unix-in-slime-p))
    (remove-hook 'kill-buffer-hook 'unix-in-slime-disconnect-maybe t)
    (slime-disconnect)))

(add-hook 'kill-buffer-hook #'unix-in-slime-disconnect-maybe)

(defun unix-in-slime-repl-setup ()
  (when (unix-in-slime-p)
    (rename-buffer "*unix-in-slime*" t)))

(add-hook 'slime-repl-mode-hook #'unix-in-slime-repl-setup)

(define-advice slime-repl-insert-prompt
    (:after () unix-in-slime)
  (let ((dir (slime-eval '(uiop:native-namestring cl:*default-pathname-defaults*))))
    (setq-local default-directory dir)))

(provide 'unix-in-slime)
;;; unix-in-slime.el ends here
