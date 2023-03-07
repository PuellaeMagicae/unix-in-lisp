;;; unix-in-sly.el --- Unix in Lisp support for SLY -*- lexical-binding: t; -*-

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

;; This package adds ANSI colors and escape sequence to the SLY REPL.

;;; Code:


(require 'ansi-color)
(require 'sly)
(require 'sly-mrepl)

(add-hook 'sly-mrepl-output-filter-functions 'ansi-color-filter-apply)

(defun unix-in-sly ()
  "Create a SLY listener running Unix in Lisp."
  (interactive)
  (sly-eval '(asdf:load-system "unix-in-lisp"))
  (with-current-buffer (sly-mrepl-new (sly-connection) "unix") ;; todo: generate new name
    (setq-local comint-inhibit-carriage-motion nil)
    (add-hook 'sly-mrepl-hook (lambda () (sly-mrepl--eval-for-repl '(unix-in-lisp:setup))) nil t)))

(define-advice sly-mrepl--insert-prompt
    (:after (_package nickname &rest _args) unix-in-sly)
  ;; TODO: use PACKAGE instead, and convert case properly
  (when (file-name-absolute-p nickname)
    (setq-local default-directory (substring (file-name-concat nickname "x") 0 -1))))

(provide 'unix-in-slime)
;;; unix-in-slime.el ends here
