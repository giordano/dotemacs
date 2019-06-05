;;; dotemacs.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012-2019 Mosè Giordano
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

;;; Code:

(eval-when-compile
  (require 'use-package))

(when (>= emacs-major-version 24)
  (electric-pair-mode +1)
  ;; in Emacs 24 vengono affiancate le etichette alle icone della
  ;; barra degli strumenti, preferisco avere solo le icone
  (setq tool-bar-style 'image)
  ;; generalmente nei terminali ho lo sfondo nero, quindi quando Emacs viene
  ;; avviato in un terminale carico un tema adatto
  (unless (display-graphic-p)
    (load-theme 'tsdh-dark)
    (custom-theme-set-faces
     'tsdh-dark
     '(linum ((t (:foreground "black" :background "lightgrey"))))))
  ;; new repositories
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("ELPA" . "http://tromey.com/elpa/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    (package-initialize)))

;;; Install packages with `use-package'
(use-package ac-math
  :ensure t)
(use-package auto-complete
  :ensure t)
(use-package cmake-mode
  :ensure t)
(use-package debbugs
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package forge
  :after magit)
(use-package gnuplot
  :ensure t)
(use-package helm
  :ensure t
  :bind
  :init
  (setq helm-split-window-inside-p            t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-M-x-fuzzy-match                  t ; optional fuzzy matching for helm-M-x
	helm-ff-file-name-history-use-recentf t
	helm-ff-newfile-prompt-p              nil))
(use-package julia-mode
  :ensure t)
(use-package lua-mode
  :ensure t)
(use-package magit
  :ensure t
  :init
  (setq magit-auto-revert-mode nil
	magit-last-seen-setup-instructions "1.4.0"
	magit-pull-or-fetch t))
(use-package markdown-mode
  :ensure t)
(use-package org
  :ensure t)
(use-package paredit
  :ensure t)
(use-package shell-command
  :ensure t
  :config (shell-command-completion-mode 1))
(use-package tabbar
  :ensure t
  :config (tabbar-mode 1))
(use-package toml-mode
  :ensure t)
(use-package xcscope
  :ensure t
  :init
  (cscope-setup)
  (setq cscope-option-other '("-R")))

;; nelle sessioni X11...
(when (display-graphic-p)
  (global-unset-key (kbd "C-z")) ;; ...disabilita C-z...
  (global-hl-line-mode 1)        ;; ...ed evidenzia la riga corrente
  ;; Autocompletamento.  Globalmente: (global-auto-complete-mode t).
  ;; Uso l'autocompletamento solo se Emacs ha una finestra grafica perché in
  ;; genere lo avvio da terminale per modifiche rapide e `auto-complete'
  ;; rallenta l'avvio e (soprattutto) la chiusura di Emacs.
  (when (fboundp 'auto-complete-mode)
    (require 'auto-complete-config)
    (ac-flyspell-workaround)
    (ac-config-default))
  ;; Maximize Emacs at startup, see http://emacs.stackexchange.com/a/3017/620.
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;; Change the font to "DejaVu Sans Mono-10".  Set `emacs-font' to nil to leave
;; it as it is.  You can also set this variable to the font-size combination of
;; your choice.
(unless (and (boundp 'emacs-font)
	     (null emacs-font))
  (add-to-list 'default-frame-alist `(font . ,(or (and (boundp 'emacs-font)
						       emacs-font)
						  "DejaVu Sans Mono-10"))))

(require 'uniquify)

(setq inhibit-startup-screen t ; nasconde la schermata di avvio
      isearch-allow-scroll t
      compilation-scroll-output t
      ;; campanella "visiva" invece di quella sonora, in caso di errore
      visible-bell t
      ;; File dove salvare i punteggi dei giochi
      tetris-score-file (concat user-emacs-directory "tetris-scores")
      snake-score-file (concat user-emacs-directory "snake-scores")
      uniquify-buffer-name-style 'post-forward-angle-brackets
      ;; For the followings see
      ;; https://twiki.cern.ch/twiki/bin/view/CDS/EmacsTips
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t       ; use versioned backups
      ediff-split-window-function 'split-window-horizontally)
(column-number-mode 1) ; mostra i numeri di riga e colonna nella mode line
(display-time-mode 1) ; mostra l'orario nella mode line
(setq-default fill-column 80) ; imposta il numero massimo di caratteri per riga
;; mostra i numeri di riga sulla sinistra
(if (>= emacs-major-version 25)
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))
(show-paren-mode 1) ; evidenzia le parentesi corrispondenti
(size-indication-mode 1) ; mostra la dimensione del buffer nella mode line
(setq text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
(delete-selection-mode 1) ; il testo inserito sostituisce la regione selezionata

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
  (shell-command (concat "xdg-open "
			 (expand-file-name default-directory))))

(defvar mg-typographic-apostrophe t
  "If non-nil, insert \"’\" pressing \"'\".")
(defun mg-insert-typographic-apostrophe (&optional arg)
  "Insert typographic apostrophe sign \"’\"."
  (interactive)
  (if (or (not mg-typographic-apostrophe)
	  ;; Do not use typographic apostrophe in "`foo1-bar2'".
	  (save-excursion
	    (re-search-backward "`[[:alnum:]-]*\\=" (line-beginning-position) t)))
      (insert "'")
    (insert "’")))
(define-key text-mode-map "'" 'mg-insert-typographic-apostrophe)

;; Keybindings impostati da me: C-next (Ctrl + Pag ↓) per andare avanti nei
;; buffer, C-prior (Ctrl + Pag ↑) per tornare indietro
(global-set-key [C-next] 'next-buffer)
(global-set-key [C-prior] 'previous-buffer)
(global-set-key [M-f1] 'open-buffer-path)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'delete-trailing-whitespace) ;; oppure `whitespace-cleanup'
(global-set-key [f7] 'check-parens)
(global-set-key (kbd "C-c M-m") 'mg-message-mark-inserted-region)
;; `mouse-6' and `mouse-7' are the horizontal scrolling gestures of the
;; touchpad.  Ignore them to silence the errors thrown when using the touchpad.
(global-set-key [mouse-6] 'ignore)
(global-set-key [mouse-7] 'ignore)

;; Map the C-/ (with / from numeric keypad) to the standard C-/.
(define-key key-translation-map (kbd "<C-kp-divide>") (kbd "C-/"))

;; associo RET (default: `newline') a `reindent-then-newline-and-indent', se non
;; ti piace il fatto che reindenti la riga attuale prima di andare a capo
;; sostituisci con `newline-and-indent'
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
;; cycle through buffers with Ctrl-Tab (like Firefox). See
;; http://emacs-fu.blogspot.com/2008/12/cycling-through-your-buffers-with-ctrl.html
(global-set-key [C-tab] 'bury-buffer)
;; Inserisci un carattere scrivendo il suo nome Unicode, o il codice
;; esadecimale/decimale/ottale.  Emula il metodo di input di caratteri Unicode
;; in ambiente GTK: `C-U' seguito dal codice esadecimale.
(global-set-key (kbd "C-s-u") 'insert-char)
;; Key bindings per Magit
(global-set-key (kbd "s-m b") 'magit-blame)
(global-set-key (kbd "s-m s") 'magit-status)

;; Apre i file con estensione `.m' con `matlab-mode'
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
;; Apre i file con estensione `.md' con `markdown-mode'
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;; Open `*.gnuplot' files with `gnuplot-mode'
(add-to-list 'auto-mode-alist '("\\.gnuplot$" . gnuplot-mode))

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
	     (lambda ()
	       (local-set-key (kbd "RET") 'newline))))
(add-hook 'before-save-hook 'time-stamp)
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
(eval-after-load "cc-mode"
  '(add-hook 'c-mode-common-hook
	     (lambda ()
	       (c-toggle-auto-newline 1)
	       (c-toggle-hungry-state 1)
	       (subword-mode 1)
	       (setq c-report-syntactic-errors t)
	       (set (make-local-variable 'electric-pair-mode) nil))))

(with-eval-after-load "ess-julia"
  (add-hook 'ess-julia-mode-hook
	    (lambda ()
	      (setq fill-column 92))))

;; http://lists.gnu.org/archive/html/bug-auctex/2013-04/msg00004.html
(defun raise-client-frame ()
  (let ((wmctrl (executable-find "wmctrl")))
    (if wmctrl
	(start-process "wmctrl" nil wmctrl "-R" (frame-parameter nil 'name)))))
(add-hook 'server-switch-hook 'raise-client-frame)

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2013-05/msg00365.html
(dotimes (i 10)                         ; for all keys
  (dolist (prefix (list "M" "C"))       ; for both modifiers
    (global-set-key
     (read (format "[%s-kp-%s]" prefix i))
     'digit-argument)
    (put
     (read (format "%s-kp-%s" prefix i))
     'ascii-character
     (+ ?0 i))))

;; Helm configuration, see https://tuhdo.github.io/helm-intro.html
(if (null (fboundp 'helm-mode))
    (ido-mode 1)
  (require 'helm)
  (require 'helm-config)
  (helm-mode 1)

  (eval-after-load "helm"
    '(progn
       (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
       (define-key helm-map (kbd "C-i")   #'helm-execute-persistent-action) ; make TAB works in terminal
       (define-key helm-map (kbd "C-z")   #'helm-select-action) ; list actions using C-z
       ))

  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x")     #'helm-M-x)
  (global-set-key (kbd "C-c h")   #'helm-command-prefix)
  (global-set-key (kbd "M-y")     #'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t)))

;;; dotemacs.el ends here
