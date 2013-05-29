;;; dotemacs-latex.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012-2013 Mosè Giordano
;;
;; Author: Mosè Giordano

;; This file is not part of GNU Emacs.

;;; Commentary:

;; LaTeX related customizations.

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

;; Se AUCTeX non è già caricato, caricalo da `auctex-dir'.  In `.emacs' imposta
;; il suo valore al percorso in cui si trova AUCTeX.
(when (and (boundp 'auctex-dir) (not (featurep 'tex-site)))
  (setq TeX-data-directory auctex-dir)
  (add-to-list 'load-path auctex-dir)
  (load "auctex.el" nil t t)
  (add-to-list 'load-path (concat auctex-dir "preview/"))
  (load "preview-latex.el" nil t t)
  (add-to-list 'Info-default-directory-list (concat auctex-dir "doc/")))

(eval-after-load "tex"
  '(progn
     (TeX-global-PDF-mode 1)
     ;; per latexmk vedi
     ;; http://lists.gnu.org/archive/html/auctex/2012-10/msg00031.html
     (add-to-list 'TeX-expand-list
		  '("%(-PDF)"
		    (lambda ()
		      (if (and (not TeX-Omega-mode)
			       (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
			  "-pdf -pdflatex=\"pdflatex -interaction=nonstopmode\""
			"-dvi -latex=\"latex -interaction=nonstopmode\""))))
     (add-to-list 'TeX-command-list
		  '("Latexmk" "latexmk %(-PDF) %s"
		    TeX-run-TeX nil t
		    :help "Run Latexmk on file to build everything."))
     (add-to-list 'TeX-command-list '("Make" "make" TeX-run-TeX nil t))
     (setq TeX-macro-global '("/usr/share/texmf/tex/" "/usr/share/texmf/bibtex/bst/"
			      "/usr/local/texlive/2012/texmf-dist/tex/")
	   TeX-newline-function 'newline-and-indent
	   TeX-debug-bad-boxes t
	   TeX-source-correlate-mode t
	   TeX-source-correlate-start-server t
	   TeX-view-program-selection `(((output-dvi style-pstricks) "xdg-open")
					;; Imposta `auctex-dvi-viewer' e
					;; `auctex-pdf-viewer' in `.emacs'.
					(output-dvi ,auctex-dvi-viewer)
					(output-pdf ,auctex-pdf-viewer)
					(output-html "xdg-open"))
	   TeX-electric-sub-and-superscript 1
	   TeX-math-close-single-dollar t
	   TeX-debug-warnings t
	   TeX-auto-save t
	   TeX-parse-self t
	   LaTeX-command-style
	   '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
     (setq-default TeX-master nil)
     (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
     (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
     (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
     (add-hook 'TeX-mode-hook
	       '(lambda ()
		  (flyspell-mode)
		  (turn-on-auto-fill)
		  (turn-on-reftex)))
     (defun mg-TeX-kpsewhich-find-file (&optional name)
       "Visit file associated to NAME searching for it with kpsewhich.
If NAME is nil prompt for a file name.  If there is an active
region, use it as initial input.  When it is called with
\\[universal-argument] prefix, visit file in another window, in
the current one otherwise."
       (interactive)
       (if (executable-find "kpsewhich")
	   (let* ((fun (if current-prefix-arg 'find-file-other-window 'find-file))
		  (default-directory (TeX-master-directory))
		  (name (or name (TeX-read-string
				  "File name: "
				  (if (TeX-active-mark)
				      (buffer-substring-no-properties
				       (region-beginning) (region-end))))))
		  (file (replace-regexp-in-string
			 "[\n\r]*\\'" ""
			 (shell-command-to-string (concat "kpsewhich " name)))))
	     (if (and (not (zerop (length file))) (file-exists-p file))
		 (funcall fun file)
	       (message (concat "File " name " not found."))))
	 (message "Kpsewhich not available.")))
     (define-key TeX-mode-map (kbd "C-c k") 'mg-TeX-kpsewhich-find-file)))

(eval-after-load "reftex-vars"
  '(progn
     (setq reftex-plug-into-AUCTeX t
	   reftex-label-alist '(AMSTeX)
	   reftex-bibliography-commands
	   '("bibliography" "nobibliography" "addbibresource"))))

(eval-after-load "latex"
  '(progn
     (TeX-add-style-hook
      "tensor"
      (lambda ()
	(TeX-add-symbols
	 '("tensor" ["Before"] 2)
	 '("tensor*" ["Before"] 2)
	 '("indices" 1)
	 '("indices*" 1))))
     (setq LaTeX-clean-intermediate-suffixes (append
					      LaTeX-clean-intermediate-suffixes
					      '("\\.fdb_latexmk" "\\.fls"))
	   LaTeX-top-caption-list '("table"))
     (add-hook 'LaTeX-mode-hook
	       '(lambda ()
		  (LaTeX-math-mode)))
     ;; http://soundandcomplete.com/2010/05/13/emacs-as-the-ultimate-latex-editor/
     ;; (require 'flymake)
     ;; (defun flymake-get-tex-args (file-name)
     ;;  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
     ;; (list "chktex" (list "-q" "-v0" file-name)))
     ;; (add-hook 'LaTeX-mode-hook 'flymake-mode)
     ;; l'ho commentato perché preferisco attivarlo manualmente, è molto dispendioso
     (when (featurep 'auto-complete)
       ;; vedi http://code.google.com/p/ac-math/
       (require 'ac-math)
       (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of {{{latex-mode}}}
       (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
	 (setq ac-sources
	       (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		       ac-sources)))
       (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup))

     ;; From an original idea of John Wickerson and a first simple
     ;; implementation of David Carlisle: http://tex.stackexchange.com/q/113376
     (defun mg-LaTeX-find-file-at-point ()
       "Visit LaTeX file searching for it with kpsewhich.
File basename is guessed from text around point and its extension
is guessed from current macro.  When it is called with
\\[universal-argument] prefix, visit file in another window, in
the current one otherwise.

See also `mg-TeX-kpsewhich-find-file'."
       (interactive)
       (let* ((file-name-regexp "-~/A-Za-z0-9_.$#%:+")
	      ;; Get filename at point.
	      (name
	       ;; Check whether character at point is a valid file name
	       ;; character.
	       (if (string-match (concat "[" file-name-regexp "]")
				 (string (char-after)))
		   (save-excursion
		     (skip-chars-backward file-name-regexp)
		     (looking-at (concat "\\([" file-name-regexp "]+\\)"))
		     (TeX-match-buffer 1))))
	      ;; Get current macro once.
	      (current-macro (TeX-current-macro))
	      ;; Guess file extension based on current macro.
	      (extension (cond
			  ((or (equal "usepackage" current-macro)
			       (equal "RequirePackage" current-macro)
			       (equal "RequirePackageWithOptions" current-macro))
			   ".sty")
			  ((or (equal "documentclass" current-macro)
			       (equal "documentstyle" current-macro)
			       (equal "LoadClass" current-macro)
			       (equal "LoadClassWithOptions" current-macro))
			   ".cls")
			  ((equal "include" current-macro) ".tex")
			  ((equal "input" current-macro)
			   ;; `input' macro accepts a file name with extension, in
			   ;; that case use an empty but non-nil extension.
			   (if (and name (file-name-extension name)) "" ".tex"))
			  ((equal "bibliography" current-macro) ".bib")
			  ((equal "addbibresource" current-macro) "")
			  (t nil))))
	 (and name extension
	      (mg-TeX-kpsewhich-find-file (concat name extension)))
	   (message "Cannot guess file name at point.")))
     (define-key LaTeX-mode-map (kbd "C-c f") 'mg-LaTeX-find-file-at-point)))

(eval-after-load "preview"
  '(progn
     (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t)))
;;; dotemacs-latex.el ends here
