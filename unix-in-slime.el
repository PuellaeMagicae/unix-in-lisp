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
(require 'slime)

(define-advice slime-repl-emit
    (:after (string) unix-in-slime)
  (with-current-buffer (slime-output-buffer)
    (comint-carriage-motion slime-output-start slime-output-end)
    (ansi-color-apply-on-region slime-output-start slime-output-end)))

(require 'slime-mrepl)
(defun unix-in-slime ()
  "Create a SLIME listener running Unix in Lisp."
  (interactive)
  (slime-new-mrepl ))

(provide 'unix-in-slime)
;;; unix-in-slime.el ends here
