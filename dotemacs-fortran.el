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

;;; Code:

;; Ci sono due modalità in Emacs per il Fortran: `fortran' e `f90'.  La prima è
;; associata ai file con estensione `.f' e `.for' (convenzionalmente considerati
;; `fixed-form'), la seconda ai file con estensioni `.f90' e `.f95'
;; (convenzionalmente considerati `free-form').

;; Per informazioni su `f90' vedi http://jblevins.org/notes/f90-mode
;; Vedi se il pacchetto `f90-interface-browser' può essere utile

(eval-after-load "f90"
  '(progn
     (setq f90-do-indent 2
	   f90-if-indent 2
	   f90-continuation-indent 4
	   f90-associate-indent 2
	   f90-program-indent 2
	   f90-type-indent 2)
     (add-hook 'f90-mode-hook
	       '(lambda ()
		  (f90-add-imenu-menu)
		  (turn-on-auto-fill)
		  (abbrev-mode 1)))))

(eval-after-load "fortran"
  '(progn
     (setq fortran-do-indent 2
	   fortran-if-indent 2
	   fortran-continuation-indent 4
	   fortran-line-number-indent 4
	   fortran-blink-matching-if t)
     (add-hook 'fortran-mode-hook
	       '(lambda ()
		  (imenu-add-menubar-index)
		  (turn-on-auto-fill)
		  (abbrev-mode 1)))
     (defun mg-fortran-kill-line ()
       "Kill the rest of the current line, as if with `kill-line'.

If it is called without prexif argument \\[universal-argument]
and the next line is the continuation of the current one, kill it
as well."
       (interactive)
       (kill-line current-prefix-arg)
       (if (and
	    (null current-prefix-arg)
	    ;; Has the whole line been killed?  i.e., are still there non white
	    ;; spaces?
	    (save-excursion
	      (skip-chars-backward " \t")
	      (bolp))
	    ;; Is the next line the continuation of the current one?
	    (save-excursion
	      (forward-char)
	      (and
	       ;; Is point at beginning of line?  XXX: probably unnecessary.
	       (bolp)
	       ;; Is the sixth column of the line non empty?  Exclude comments.
	       ;; XXX: " \\{5\\}[^ ]" instead of "[^cC].\\{4\\}[^ ]"?
	       (looking-at "[^cC].\\{4\\}[^ ]"))))
	   ;; Then kill the next line.
	   (progn
	     (kill-line)
	     (mg-fortran-kill-line))))
     (define-key fortran-mode-map (kbd "C-k") 'mg-fortran-kill-line)))

;;; dotemacs-fortran.el ends here
