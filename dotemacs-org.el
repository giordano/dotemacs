;;; dotemacs-org.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2013, 2014, 2017 Mosè Giordano
;;
;; Author: Mosè Giordano

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Org-mode related customizations.

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

(use-package org
  :ensure t)

(eval-after-load "org"
  '(progn
     ;; From Org Mode manual
     (defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
	 (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

     (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

     (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,")
     (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

     ;; Suggested key bindings from Org manual
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-cc" 'org-capture)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)

     ;;; Org Diary
     (defun org-insert-date-heading (&optional arg)
       "Insert lines of the type
  * YEAR		:YYYY:
  ** MONTHNAME YEAR	:YYYYMM:
  *** <TIMESTAMP>	:YYYYMMDD:
  **** _
and leave point under `_'.  By default, use current date.  It is
useful to write a simple diary.  Lines for current year, current
month and time stamp are inserted only if they haven't been
already inserted. (Note: searches only backward.)  It is assumed
that point is in the correct place.  If this command is called
with a prefix argument, prompt for a different date."
       (interactive "P")
       (let (date)
	 (if arg
	     (setq date (org-time-string-to-time (org-read-date)))
	   (setq date (current-time)))
	 (end-of-line)
	 (if (save-excursion
	       ;; If the current line contains only whitespaces...
	       (re-search-backward "^[ \t]*$" (line-beginning-position) t))
	     ;; ...then, delete the line...
	     (delete-region (point) (line-beginning-position))
	   ;; ...else, insert a newline.
	   (insert "\n"))
	 (unless (save-excursion
		   ;; Search bacward for lines of the type
		   ;;   * YEAR	:YYYY:
		   (re-search-backward
		    (format-time-string "^\\*[ \t]+%Y[ \t]+:%Y:$" date) nil t))
	   ;; Insert the line
	   ;;   * YEAR	:YYYY:
	   (insert (format-time-string "* %Y\t\t\t:%Y:\n" date)))
	 (unless (save-excursion
		   ;; Search bacward for lines of the type
		   ;;   ** MONTH_NAME YEAR	:YYYYMM:
		   (re-search-backward
		    (concat "^\\*\\*[ \t]+"
			    (calendar-month-name
			     (string-to-number (format-time-string "%m" date)))
			    (format-time-string "[ \t]+%Y[ \t]+:%Y%m:$" date)) nil t))
	   ;; Insert the line
	   ;;   ** MONTH_NAME YEAR	:YYYYMM:
	   (insert "** "
		   (calendar-month-name
		    (string-to-number (format-time-string "%m" date)))
		   (format-time-string " %Y\t\t:%Y%m:\n" date)))
	 (unless (save-excursion
		   ;; Search backward for lines of the type
		   ;;   *** <TIME_STAMP>	:YYYYMMDD:
		   (re-search-backward
		    (format-time-string "^\\*\\*\\*[ \t]+<%Y-%m-%d.*>[ \t]+:%Y%m%d:" date)
		    nil t))
	   ;; Insert the line
	   ;;   *** <TIME_STAMP>	:YYYYMMDD:
	   (insert "*** ")
	   (org-insert-time-stamp date)
	   (insert (format-time-string "\t:%Y%m%d:\n" date))))
       (insert "**** "))
     (define-key org-mode-map "\C-cd" 'org-insert-date-heading)

     (define-key org-mode-map "'" 'mg-insert-typographic-apostrophe)))

(eval-after-load "ox-publish"
  '(load "~/repo/sito/sito"))

(eval-after-load "ox"
  '(setq org-export-with-smart-quotes t))

;;; dotemacs-org.el ends here
