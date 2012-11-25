;;; dotemacs-fortran.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012 Mosè Giordano
;;
;; Author: Mosè Giordano

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Fortran related customizations.

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

;; Code:

;; Ci sono due modalità in Emacs per il Fortran: `fortran' e `f90'.  La prima è
;; associata ai file con estensione `.f' e `.for' (convenzionalmente considerati
;; `fixed-form'), la seconda ai file con estensioni `.f90' e `.f95'
;; (convenzionalmente considerati `free-form').

;; uso la modalità `f90-mode' anche per i file con estensione `.f'
(add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))
;; Per informazioni su `f90' vedi http://jblevins.org/notes/f90-mode
;; Vedi se il pacchetto `f90-interface-browser' può essere utile
(eval-after-load "f90"
  (setq f90-associate-indent 2
	f90-continuation-indent 4
	f90-do-indent 2
	f90-if-indent 2
	f90-program-indent 2
	f90-type-indent 2))
(add-hook 'f90-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
	     (f90-add-imenu-menu)
	     (turn-on-auto-fill)
	     (abbrev-mode 1)))

(eval-after-load "fortran"
  (setq fortran-do-indent 2
	fortran-if-indent 2
	fortran-continuation-indent 4
	fortran-line-number-indent 4
	fortran-continuation-string "&"
	fortran-blink-matching-if t))
(add-hook 'fortran-mode-hook
	  '(lambda()
	     (turn-on-auto-fill)
	     (abbrev-mode 1)))

;;; dotemacs-fortran.el ends here
