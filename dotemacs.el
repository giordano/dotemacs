;;; dotemacs.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012-2013 Mosè Giordano
;;
;; Author: Mosè Giordano

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Generic customizations.

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

(when (>= emacs-major-version 24)
  ;; non mi piace il font di default di Emacs 24
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
  ;; in Emacs 24 vengono affiancate le etichette alle icone della
  ;; barra degli strumenti, preferisco avere solo le icone
  (setq tool-bar-style 'image)
  ;; generalmente nei terminali ho lo sfondo nero, quindi quando Emacs viene
  ;; avviato in un terminale carico un tema adatto
  (unless (display-graphic-p)
    (load-theme 'manoj-dark))
  (eval-after-load "package"
    ;; aggiungo altri repository da cui scaricare pacchetti
    '(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			      ("ELPA" . "http://tromey.com/elpa/")
			      ("marmalade" . "http://marmalade-repo.org/packages/")
			      ("technomancy" . "http://repo.technomancy.us/emacs/")
			      ("melpa" . "http://melpa.milkbox.net/packages/")))))

;; nelle sessioni X11...
(when (display-graphic-p)
  (global-unset-key (kbd "C-z")) ;; ...disabilita C-z...
  (global-hl-line-mode 1)        ;; ...ed evidenzia la riga corrente
  ;;; Autocompletamento.  Globalmente: (global-auto-complete-mode t).
  ;; Uso l'autocompletamento solo se Emacs ha una finestra grafica perché in
  ;; genere lo avvio da terminale per modifiche rapide e `auto-complete'
  ;; rallenta l'avvio e (soprattutto) la chiusura di Emacs.
  (add-to-list 'load-path (concat user-emacs-directory "elpa/popup/"))
  (add-to-list 'load-path (concat user-emacs-directory "elpa/auto-complete/"))
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "elpa/auto-complete/dict"))
  (ac-config-default)
  ;; Aggiungo alcune altre modalità a quelle in cui usare di default
  ;; `auto-complete-mode'
  (ac-flyspell-workaround))

(add-to-list 'load-path user-emacs-directory)

(setq inhibit-startup-screen t ; nasconde la schermata di avvio
      isearch-allow-scroll t
      compilation-scroll-output t
      ;; campanella "visiva" invece di quella sonora, in caso di errore
      visible-bell t
      ;; File dove salvare i punteggi dei giochi
      tetris-score-file (concat user-emacs-directory "tetris-scores")
      snake-score-file (concat user-emacs-directory "snake-scores")
      vc-follow-symlinks t)
(tabbar-mode 1) ; attiva la visualizzazione delle schede
(column-number-mode 1) ; mostra i numeri di riga e colonna nella mode line
(display-time-mode 1) ; mostra l'orario nella mode line
(setq-default fill-column 80) ; imposta il numero massimo di caratteri per riga
(global-linum-mode 1) ; mostra i numeri di riga sulla sinistra
(shell-command-completion-mode 1)
(show-paren-mode 1) ; evidenzia le parentesi corrispondenti
(size-indication-mode 1) ; mostra la dimensione del buffer nella mode line
(setq text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
(delete-selection-mode 1) ; il testo inserito sostituisce la regione selezionata
(electric-pair-mode +1)

;; vedi https://twiki.cern.ch/twiki/bin/view/CDS/EmacsTips
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;;; marking inserted text.  Taken from `lisp/gnus/message.el'
(defvar mg-message-mark-insert-begin
  "--8<---------------cut here---------------start------------->8---\n"
  "How to mark the beginning of some inserted text.")

(defvar mg-message-mark-insert-end
  "--8<---------------cut here---------------end--------------->8---\n"
  "How to mark the end of some inserted text.")

(defun mg-message-mark-inserted-region (beg end &optional verbatim)
  "Mark some region in the current article with enclosing tags.
See `mg-message-mark-insert-begin' and `mg-message-mark-insert-end'.
If VERBATIM, use slrn style verbatim marks (\"#v+\" and \"#v-\")."
  (interactive "r\nP")
  (save-excursion
    ;; add to the end of the region first, otherwise end would be invalid
    (goto-char end)
    (insert (if verbatim "#v-\n" mg-message-mark-insert-end))
    (goto-char beg)
    (insert (if verbatim "#v+\n" mg-message-mark-insert-begin))))
;;; end marking insert text.

;; ;; Vedi https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/ Per il
;; ;; momento non ho intenzione di usare il server quindi commento
;; (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Funzione per aprire il file manager nella cartella in cui è salvato il
;; buffer.  Ispirata a questa guida
;; http://zhangda.wordpress.com/2010/02/03/open-the-path-of-the-current-buffer-within-emacs/
(defun open-buffer-path ()
  "Run file manager on the directory of the current buffer."
  (interactive)
  (shell-command (concat "xdg-open " default-directory)))

;; Keybindings impostati da me: C-next (Ctrl + Pag ↓) per andare avanti nei
;; buffer, C-prior (Ctrl + Pag ↑) per tornare indietro
(global-set-key (kbd "<C-next>") 'next-buffer)
(global-set-key (kbd "<C-prior>") 'previous-buffer)
(global-set-key (kbd "<f4>") 'open-buffer-path)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'delete-trailing-whitespace) ;; oppure `whitespace-cleanup'
(global-set-key (kbd "<f7>") 'eval-buffer)
(global-set-key (kbd "<f8>") 'check-parens)
(global-set-key (kbd "C-c M-m") 'mg-message-mark-inserted-region)
;; associo RET (default: `newline') a `reindent-then-newline-and-indent', se non
;; ti piace il fatto che reindenti la riga attuale prima di andare a capo
;; sostituisci con `newline-and-indent'
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
;; cycle through buffers with Ctrl-Tab (like Firefox). See
;; http://emacs-fu.blogspot.com/2008/12/cycling-through-your-buffers-with-ctrl.html
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; Apre i file con estensione `.m' con `matlab-mode'
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
;; Apre i file con estensione `.md' con `markdown-mode'
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Cedet. Per ora lo rimuovo
;; (load-file "/usr/share/emacs/site-lisp/cedet-common/cedet.el")
;; (global-ede-mode 1)                 ; Enable the Project management system
;; (semantic-load-enable-code-helpers) ; Enable prototype help and smart completion
;; (global-srecode-minor-mode 1)       ; Enable template insertion menu

;; ;; invio di email (funzionante)
;; (setq send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials
;;       '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       (expand-file-name "~/.authinfo")
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-debug-info t)
;; (require 'smtpmail)

;;; Hook

;; ;; Attiva `flyspell' per tutti i file \O/
;; (add-hook 'find-file-hook 'flyspell-mode)

;; Nella modalità `makefile' la funzione `reindent-then-newline-and-indent' non
;; si comporta come mi aspetto, allora associo `RET' alla semplice
;; `newline'
(eval-after-load "make-mode"
  '(add-hook 'makefile-mode-hook
	     '(lambda ()
		(local-set-key (kbd "RET") 'newline))))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'before-save-hook 'time-stamp)
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
(eval-after-load "cc-mode"
  '(add-hook 'c-mode-common-hook
	     '(lambda ()
		(c-toggle-auto-state 1)
		(c-toggle-hungry-state 1)
		(subword-mode 1)
		(setq c-report-syntactic-errors t))))
;;; dotemacs.el ends here
