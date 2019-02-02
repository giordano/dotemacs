;;; dotemacs-elisp.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2013 Mosè Giordano
;;
;; Author: Mosè Giordano

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs Lisp related customizations.

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:

(eval-after-load "lisp-mode"
  '(progn
     (define-key emacs-lisp-mode-map [f8] 'eval-buffer)
     (define-key emacs-lisp-mode-map [f9]
       (lambda ()
	 (interactive)
	 (byte-compile-file (buffer-file-name))))))

(use-package paredit
  :ensure t)

;; Paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    ;; XXX: `turn-on-eldoc-mode' is an obsolete function (as of 24.4);
	    ;; you will have to use `eldoc-mode' at some point of time.
	    (turn-on-eldoc-mode)
	    (enable-paredit-mode)))
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "<C-left>") 'left-word)
     (define-key paredit-mode-map (kbd "<C-right>") 'right-word)
     (define-key paredit-mode-map (kbd "<M-left>") 'paredit-backward-slurp-sexp)
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)))

;;; dotemacs-elisp.el ends here
