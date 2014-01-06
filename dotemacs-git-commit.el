;;; dotemacs-git-commit.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012 Mosè Giordano
;;
;; Author: Mosè Giordano

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Imposta Emacs come editor di testo per i commit di git:
;;   git config --global core.editor "emacs -Q -l <PERCORSO>/dotemacs-git-commit.el"
;; In questo modo verrà caricato solo questo file di inizializzazione.

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

(add-to-list
 'load-path
 ;; Search for the directory of the ELPA `git-commit-mode'.
 (car (directory-files (concat user-emacs-directory "/elpa")
		       t "git-commit-mode-[.0-9]+")))
(setq-default fill-column 72)
(column-number-mode)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(require 'cl-lib)
(require 'git-commit-mode)
(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)

;;; dotemacs-git-commit.el ends here
