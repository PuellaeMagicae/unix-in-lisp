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

;; This package adds ANSI colors and escape sequence to the SLIME REPL.

;;; Code:


(require 'ansi-color)
(require 'nadvice)
(require 'slime)
(require 'slime-mrepl)

(define-advice slime-repl-emit
    (:after (string) unix-in-slime)
  (with-current-buffer (slime-output-buffer)
    (comint-carriage-motion slime-output-start slime-output-end)
    (ansi-color-apply-on-region slime-output-start slime-output-end)))

;;;###autoload
(defun unix-in-slime ()
  "Create a SLIME listener running Unix in Lisp."
  (interactive)
  (require 'slime-company)
  (require 'slime-autodoc)
  (add-to-list 'slime-company-major-modes 'slime-mrepl-mode)
  (add-hook 'slime-mrepl-mode-hook #'slime-company-maybe-enable)
  (add-hook 'slime-mrepl-mode-hook #'slime-autodoc-mode)
  (if (slime-connected-p)
      (progn
        (slime-enable-contrib 'slime-mrepl)
        (let ((channel (slime-make-channel slime-listener-channel-methods)))
          (slime-eval-async
              `(swank-mrepl:create-mrepl ,(slime-channel.id channel))
            (slime-rcurry
             (lambda (result channel)
               (cl-destructuring-bind (remote thread-id package prompt) result
                 (pop-to-buffer (generate-new-buffer (slime-buffer-name :mrepl)))
                 (slime-mrepl-mode)
                 (setq slime-current-thread thread-id)
                 (setq slime-buffer-connection (slime-connection))
                 (set (make-local-variable 'slime-mrepl-remote-channel) remote)
                 (slime-channel-put channel 'buffer (current-buffer))
                 (slime-mrepl-send
                  '(:process "(cl:progn
 (ql:quickload \"unix-in-lisp\")
 (cl:funcall (cl:find-symbol \"SETUP\" \"UNIX-IN-LISP\")))"))))
             channel))))
      (save-selected-window (slime-start :init-function #'unix-in-slime))))

(define-advice slime-mrepl-prompt
    (:after (package prompt) unix-in-slime)
  (when (file-name-absolute-p prompt)
    (setq-local default-directory
                (substring (file-name-concat (slime-eval `(unix-in-lisp::unconvert-case ,prompt)) "x") 0 -1))))

(provide 'unix-in-slime)
;;; unix-in-slime.el ends here
