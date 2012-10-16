(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(display-time-mode t)
 '(fill-column 80)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(isearch-allow-scroll t)
 '(message-log-max 300)
 '(shell-command-completion-mode t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tabbar-mode t nil (tabbar))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))))

;; Aggiunge la cartella ~/.emacs.d/ al load-path di emacs
(add-to-list 'load-path "~/.emacs.d/")

;; Autocompletamento
;; (global-auto-complete-mode t)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; Aggiungo alcune altre modalità a quelle in cui usare di default
;; `auto-complete-mode'
(add-to-list 'ac-modes 'latex-mode)
(ac-flyspell-workaround)

;; non ricordo a cosa serva (forse ha a che fare con emacsclient)
;; (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; cycle through buffers with Ctrl-Tab (like Firefox). See
;; http://emacs-fu.blogspot.com/2008/12/cycling-through-your-buffers-with-ctrl.html
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; Keybindings impostati da me: C-next (Ctrl + Pag ↑) per andare avanti nei
;; buffer C-prior (Ctrl + Pag ↓) per tornare indietro
(global-set-key (kbd "<C-next>") 'next-buffer)
(global-set-key (kbd "<C-prior>") 'previous-buffer)

;; Attiva gnuplot
(require 'gnuplot)

;; File dove salvare i punteggi dei giochi
(setq tetris-score-file
      "~/.emacs.d/tetris-scores")
(setq snake-score-file
      "~/.emacs.d/snake-scores")

;; Dovrebbe aprire tutti i file .m con `matlab-mode'
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
;; Apri i file con estensione `.gnuplot' con `gnuplot-mode'
(add-to-list 'auto-mode-alist '("\\.gnuplot$" . gnuplot-mode))

;; Cedet. Per ora lo rimuovo
;; (load-file "/usr/share/emacs/site-lisp/cedet-common/cedet.el")
(global-ede-mode 1)                    ; Enable the Project management system
;; (semantic-load-enable-code-helpers) ; Enable prototype help and smart completion
;; (global-srecode-minor-mode 1)       ; Enable template insertion menu

;; mandare mail funzionante
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

;; Attiva git
(require 'git)

;; Attiva `flyspell' per tutti i file \O/
;; (add-hook 'find-file-hook 'flyspell-mode)

;; (require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("technomancy" . "http://repo.technomancy.us/emacs/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;; Funzione per aprire il file manager nella cartella in cui è salvato il buffer.
;; http://zhangda.wordpress.com/2010/02/03/open-the-path-of-the-current-buffer-within-emacs/
(defun open-buffer-path ()
  "Run file manager on the directory of the current buffer."
  (interactive)
  (shell-command (concat "xdg-open " default-directory)))
(global-set-key (kbd "M-<f3>") 'open-buffer-path)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'whitespace-cleanup)
(global-set-key (kbd "<f7>") 'eval-buffer)

(add-hook 'c-mode-common-hook '(lambda ()
				 (c-toggle-auto-state 1)
				 (c-toggle-hungry-state 1)
				 (subword-mode 1)
				 (setq c-report-syntactic-errors t)))
(delete-selection-mode t)

;; vedi https://twiki.cern.ch/twiki/bin/view/CDS/EmacsTips
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq visible-bell t)

(load "~/Documenti/sito/sito")
