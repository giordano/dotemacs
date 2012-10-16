;; configurazioni che hanno a che fare con LaTeX

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
     (setq LaTeX-clean-intermediate-suffixes (quote ("\\.aux" "\\.bbl" "\\.bcf" "\\.blg" "\\.brf" "\\.fot" "\\.glo" "\\.gls" "\\.idx" "\\.ilg" "\\.ind" "\\.lof" "\\.log" "\\.lot" "\\.nav" "\\.out" "\\.snm" "\\.toc" "\\.url" "\\.synctex\\.gz" "\\.tex~" "\\.run\\.xml")))
     (setq LaTeX-fill-break-at-separators (quote ({ } \[ \\\( \\\) \\\[ \\\])))
     (setq LaTeX-indent-environment-list (quote (("verbatim" current-indentation) ("verbatim*" current-indentation) ("array") ("displaymath") ("eqnarray") ("eqnarray*") ("equation") ("equation*") ("picture") ("tabbing") ("table") ("table*") ("tabular") ("tabular*") ("lstlisting" current-indentation))))
     (setq LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "lstlisting" "Verbatim")))
     (setq TeX-PDF-mode t)
     (setq TeX-macro-global (quote ("/usr/share/texmf/tex/" "/usr/share/texmf/bibtex/bst/" "/usr/local/texlive/2012/texmf-dist/tex/")))
     (setq TeX-newline-function (quote newline-and-indent))
     (setq TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "xdg-open") (output-html "xdg-open"))))
     (setq TeX-auto-save t)
     (setq TeX-parse-self t)
     (setq-default TeX-master nil)
     (setq LaTeX-always-use-Biber t)
     (setq reftex-plug-into-AUCTeX t)
     (setq reftex-label-alist '(AMSTeX))
     (setq TeX-electric-sub-and-superscript 1)))

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
