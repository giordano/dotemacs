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

;; Code:

(add-to-list 'load-path "~/.emacs.d/") ;; cartella in cui si trova git-commit.el
(turn-on-auto-fill)
(setq-default fill-column 72)
(column-number-mode)
(require 'cl) ;; serve per `git-commit'
(require 'git-commit)

;;; dotemacs-git-commit.el ends here