;;; dotemacs-latex.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012 Mosè Giordano
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

;; Code:

(eval-after-load "tex"
  '(progn
     (require 'cl) ;; serve per Biber in AUCTeX
     (TeX-global-PDF-mode 1)
     (TeX-add-style-hook
      "tensor"
      (lambda ()
	(TeX-add-symbols
	 '("tensor" ["Before"] 2)
	 '("tensor*" ["Before"] 2)
	 '("indices" 1)
	 '("indices*" 1))))
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
     (setq LaTeX-clean-intermediate-suffixes '("\\.aux" "\\.bbl" "\\.bcf" "\\.blg"
					       "\\.brf" "\\.fdb_latexmk" "\\.fls"
					       "\\.fot" "\\.glo" "\\.gls" "\\.idx"
					       "\\.ilg" "\\.ind" "\\.lof" "\\.log"
					       "\\.lot" "\\.nav" "\\.out" "\\.snm"
					       "\\.synctex\\.gz" "\\.run\\.xml"
					       "\\.tex~" "\\.toc" "\\.url")
	   LaTeX-fill-break-at-separators '({ } \[ \\\( \\\) \\\[ \\\])
	   LaTeX-indent-environment-list '(("verbatim" current-indentation)
					   ("verbatim*" current-indentation)
					   ("Verbatim" current-indentation)
					   ("lstlisting" current-indentation)
					   ("array") ("displaymath")
					   ("eqnarray") ("eqnarray*")
					   ("equation") ("equation*")
					   ("picture") ("tabbing") ("table")
					   ("table*") ("tabular") ("tabular*"))
	   LaTeX-top-caption-list '("table")
	   LaTeX-verbatim-environments '("verbatim" "verbatim*" "lstlisting" "Verbatim")
	   LaTeX-verbatim-macros-with-braces '("url")
	   TeX-macro-global '("/usr/share/texmf/tex/" "/usr/share/texmf/bibtex/bst/"
			      "/usr/local/texlive/2012/texmf-dist/tex/")
	   TeX-newline-function 'newline-and-indent
	   TeX-view-program-selection '(((output-dvi style-pstricks) "xdg-open")
					(output-dvi "xdg-open")
					(output-pdf "xdg-open")
					(output-html "xdg-open"))
	   TeX-debug-bad-boxes t ; visualizza numero bad boxes
	   TeX-debug-warnings t ; visualizza numero warnings
	   TeX-auto-save t
	   TeX-parse-self t
	   LaTeX-always-use-Biber t
	   reftex-plug-into-AUCTeX t
	   reftex-label-alist '(AMSTeX)
	   TeX-electric-sub-and-superscript 1)
     (setq-default TeX-master nil)
     ;; vedi http://code.google.com/p/ac-math/
     (require 'ac-math)
     (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of {{{latex-mode}}}
     (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
       (setq ac-sources
	     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		     ac-sources))
       )))

;; ;; Attiva di default la modalità LaTeX-math-mode per tutte le modalità di AUCTeX
;; (add-hook 'TeX-mode-hook 'LaTeX-math-mode)

(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)

(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     ;; Attiva automaticamente la correzione ortografica aprendo un
	     ;; documento LaTeX.  Finalmente \O/
	     (flyspell-mode)
	     ;; Attiva la modalità con la quale un rigo non può essere più lungo
	     ;; di `fill-column' caratteri
	     (turn-on-auto-fill)
	     ;; Attiva di default la modalità LaTeX-math-mode per la latex-mode
	     (LaTeX-math-mode)
	     ;; Modalità per evidenziare le parentesi corrispondenti in LaTeX.
	     ;; http://centaur.maths.qmw.ac.uk/emacs/files/latex-paren.el
	     ;; (require 'latex-paren)
	     (turn-on-reftex)
	     (ac-latex-mode-setup)))

;; http://soundandcomplete.com/2010/05/13/emacs-as-the-ultimate-latex-editor/
;; (require 'flymake)
;; (defun flymake-get-tex-args (file-name)
;;  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
;; (list "chktex" (list "-q" "-v0" file-name)))
;; (add-hook 'LaTeX-mode-hook 'flymake-mode)
;; l'ho commentato perché preferisco attivarlo manualmente, è molto dispendioso

;;; dotemacs-latex.el ends here
