;;; dotemacs-latex.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012 Mosè Giordano
;;
;; Author: Mosè Giordano

;; This file is not part of GNU Emacs.

;;; Commentary:

;; LaTeX relatex customizations.

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
     (TeX-add-style-hook
      "bm"
      (lambda ()
	(TeX-add-symbols
	 '("bm" 1))))
     (TeX-add-style-hook
      "tensor"
      (lambda ()
	(TeX-add-symbols
	 '("tensor" ["Before"] 2)
	 '("tensor*" ["Before"] 2)
	 '("indices" 1)
	 '("indices*" 1))))
     (TeX-add-style-hook
      "kpfonts"
      (lambda ()
	(TeX-run-style-hooks "amsmath")))
     (add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t))
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
					   ("lstlisting" current-indentation)
					   ("array") ("displaymath")
					   ("eqnarray") ("eqnarray*")
					   ("equation") ("equation*")
					   ("picture") ("tabbing") ("table")
					   ("table*") ("tabular") ("tabular*"))
	   LaTeX-verbatim-environments '("verbatim" "verbatim*" "lstlisting" "Verbatim")
	   TeX-PDF-mode t
	   TeX-macro-global '("/usr/share/texmf/tex/" "/usr/share/texmf/bibtex/bst/"
			      "/usr/local/texlive/2012/texmf-dist/tex/")
	   TeX-newline-function 'newline-and-indent
	   TeX-view-program-selection '(((output-dvi style-pstricks) "dvips and gv")
					(output-dvi "xdvi") (output-pdf "xdg-open")
					(output-html "xdg-open"))
	   TeX-auto-save t
	   TeX-parse-self t
	   LaTeX-always-use-Biber t
	   reftex-plug-into-AUCTeX t
	   reftex-label-alist '(AMSTeX)
	   TeX-electric-sub-and-superscript 1)
     (setq-default TeX-master nil)))

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
	     (require 'latex-paren)
	     (turn-on-reftex)))

;; http://soundandcomplete.com/2010/05/13/emacs-as-the-ultimate-latex-editor/
;; (require 'flymake)
;; (defun flymake-get-tex-args (file-name)
;;  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
;; (list "chktex" (list "-q" "-v0" file-name)))
;; (add-hook 'LaTeX-mode-hook 'flymake-mode)
;; l'ho commentato perché preferisco attivarlo manualmente, è molto dispendioso

;;; dotemacs-latex.el ends here
