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
	   TeX-view-program-selection '(((output-dvi style-pstricks) "xdg-open")
					(output-dvi "xdg-open")
					(output-pdf "xdg-open")
					(output-html "xdg-open"))
	   TeX-electric-sub-and-superscript 1
	   TeX-math-close-single-dollar t
	   TeX-debug-warnings t
	   TeX-auto-save t
	   TeX-parse-self t)
     (setq-default TeX-master nil)
     (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
     (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
     (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
     (add-hook 'TeX-mode-hook
	       '(lambda ()
		  (flyspell-mode)
		  (turn-on-auto-fill)
		  (turn-on-reftex)))))

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

     (defun LaTeX-find-file ()
       "Find LaTeX file at point."
       (interactive)
       (if (executable-find "kpsewhich")
	   (let* ((name (thing-at-point 'symbol))
		  (extension (cond
			      ((or (string-equal (TeX-current-macro)
						 "documentclass")
				   (string-equal (TeX-current-macro)
						 "documentstyle")
				   (string-equal (TeX-current-macro)
						 "LoadClass")
				   (string-equal (TeX-current-macro)
						 "LoadClassWithOptions"))
			       ".cls")
			      ((or (string-equal (TeX-current-macro)
						 "usepackage")
				   (string-equal (TeX-current-macro)
						 "RequirePackage")
				   (string-equal (TeX-current-macro)
						 "RequirePackageWithOptions"))
			       ".sty")
			      ((or (string-equal (TeX-current-macro) "include")
				   (string-equal (TeX-current-macro) "input"))
			       ".tex")))
		  (file (replace-regexp-in-string
			 "[\n\r]*" ""
			 (shell-command-to-string
			  (concat "kpsewhich " name extension)))))
	     (if (zerop (length file))
		 (message "No file found.")
	       (find-file-other-window file)))
	 (message "LaTeX-find-file requires kpsewhich.")))
     (global-set-key (kbd "C-c f") 'LaTeX-find-file)))

(eval-after-load "preview"
  '(progn
     (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t)))
;;; dotemacs-latex.el ends here
