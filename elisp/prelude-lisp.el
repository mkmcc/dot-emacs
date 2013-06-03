;;; prelude-lisp.el --- Emacs Prelude: Configuration common to all lisp modes.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration shared between all modes related to lisp-like languages.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; a great lisp coding hook
(defun prelude-lisp-coding-hook ()
  (paredit-mode +1))

;; interactive modes don't need whitespace checks
(defun prelude-interactive-lisp-coding-hook ()
  (paredit-mode +1)
  (prelude-turn-off-whitespace))

(add-hook 'scheme-mode-hook 'prelude-lisp-coding-hook)


(eval-after-load "paredit"
  '(progn
     (defun paredit-wrap-round-from-behind ()
       (interactive)
       (forward-sexp -1)
       (paredit-wrap-round)
       (insert " ")
       (forward-char -1))

     (define-key paredit-mode-map (kbd "M-)")
       'paredit-wrap-round-from-behind)))

(provide 'prelude-lisp)

;;; prelude-lisp.el ends here
