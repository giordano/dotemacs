;;; dotemacs-fortran.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012-2013  Mosè Giordano
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
	       (lambda ()
		 (f90-add-imenu-menu)
		 (turn-on-auto-fill)
		 (abbrev-mode 1)))))

(eval-after-load "fortran"
  '(progn
     (setq fortran-do-indent 2
	   fortran-if-indent 2
	   fortran-continuation-indent 4
	   fortran-line-number-indent 4
	   fortran-blink-matching-if t
	   fortran-continuation-string "&")
     (add-hook 'fortran-mode-hook
	       (lambda ()
		 (imenu-add-menubar-index)
		 (turn-on-auto-fill)
		 (abbrev-mode 1)))

     (defun mg-fortran-kill-line (&optional arg)
       "Kill the rest of the current line.

If optional ARG is non-nil, behave like `kill-line', otherwise if
following lines are the continuation of the current one, kill
them as well."
       (interactive "P")
       (kill-line arg)
       (if (and
	    (null arg)
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
	       ;; Is the line non empty?
	       (null (eolp))
	       ;; Is the sixth column of the line non empty?  Exclude comments
	       ;; and preprocessor directives.  XXX: " \\{5\\}[^ ]" instead of
	       ;; "[^cC#].\\{4\\}[^ ]"?
	       (looking-at "[^cC#].\\{4\\}[^ ]"))))
	   ;; Then kill the next line.
	   (progn
	     (append-next-kill)
	     (kill-line)
	     (mg-fortran-kill-line))))

     (defun mg-fortran-display-label (labelnum &optional how)
       "Display label LABELNUM.

HOW optional argument determine how the label is displayed:
  nil:           Move point to the label.
  echo:          Show one-line info in echo area.
  other-window:  Display the label in another window."
       (interactive "nLabel: ")
       (let ((labelstring (prin1-to-string labelnum))
	     labelpos)
	 ;; Don't be verbose when `how' == `echo'.
	 (if (> (length labelstring) 5)
	     (or (eq how 'echo)
		 (message "Labels cannot be longer than 5 digits."))
	   (save-excursion
	     (fortran-beginning-of-subprogram)
	     (condition-case nil
		 ;; Search in the current subprogram for the label.
		 (and
		  (re-search-forward
		   (concat "^ *\\<" labelstring "\\> +")
		   (save-excursion (fortran-end-of-subprogram) (point)))
		  (if (<= (current-column) 6) (setq labelpos (point))))
	       ;; If search fails issue a message.
	       (search-failed
		(or (eq how 'echo)
		    (message (concat "Label " labelstring " not found."))))))
	   (if labelpos
	       ;; Display the label according to `how'.
	       (cond
		((eq how 'echo)
		 (save-excursion
		   (goto-char labelpos)
		   (message (buffer-substring
			     (line-beginning-position)
			     (line-end-position)))))
		((eq how 'other-window)
		 (switch-to-buffer-other-window (current-buffer))
		 (goto-char labelpos)
		 (other-window -1))
		(t
		 (set-mark (point))
		 (goto-char labelpos)
		 (deactivate-mark)))))))

     (defun mg-fortran-display-label-at-point ()
       "Display label at point in another window."
       (interactive)
       (let ((label (number-at-point)))
	 (if label
	     (mg-fortran-display-label label 'other-window)
	   (message "Cannot find label at point."))))

     (defun mg-fortran-echo-label-at-point ()
       "Display the label at point in the echo area."
       (and (eq major-mode 'fortran-mode)
	    (let ((label (number-at-point)))
	      (if label
		  (mg-fortran-display-label label 'echo)))))

     (defcustom mg-fortran-echo-label t
       "Whether to display labels in echo area."
       :type 'boolean
       :group 'fortran)
     (defcustom mg-fortran-echo-label-idle-delay 1
       "Number of seconds of idle time to wait before displaying labels."
       :type 'number
       :group 'fortran)

     (add-hook 'fortran-mode-hook
	       (lambda ()
		 (if mg-fortran-echo-label
		     (run-with-idle-timer mg-fortran-echo-label-idle-delay t
					  'mg-fortran-echo-label-at-point))))

     (define-key fortran-mode-map (kbd "C-c &")
       'mg-fortran-display-label-at-point)
     (define-key fortran-mode-map (kbd "C-k") 'mg-fortran-kill-line)
     (define-key fortran-mode-map [f9]
       (lambda ()
	 (interactive)
	 (compile "make -k")))
     (define-key fortran-mode-map [M-up] 'fortran-beginning-of-subprogram)
     (define-key fortran-mode-map [M-down] 'fortran-end-of-subprogram)))

;;; dotemacs-fortran.el ends here
