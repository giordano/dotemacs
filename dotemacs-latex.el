;; configurazioni che hanno a che fare con LaTeX

(require 'cl) ;; serve per Biber in AUCTeX
(eval-after-load "tex"
  '(TeX-add-style-hook
   "bm"
   (lambda ()
     (TeX-add-symbols
      '("bm" 1)))))
(eval-after-load "tex"
  '(TeX-add-style-hook
   "tensor"
   (lambda ()
     (TeX-add-symbols
      '("tensor" ["Before"] 2)
      '("tensor*" ["Before"] 2)
      '("indices" 1)
      '("indices*" 1)))))
(eval-after-load "tex"
  '(TeX-add-style-hook
    "kpfonts"
    (lambda ()
      (TeX-run-style-hooks "amsmath"))))

;; Impostazioni per AUCTeX suggerite nel manuale
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq LaTeX-always-use-Biber t) ; always use Biber

;; Modalità per evidenziare le parentesi corrispondenti in LaTeX.
;; http://centaur.maths.qmw.ac.uk/emacs/files/latex-paren.el
;;NON cambiare `latex' in `LaTeX'!
(add-hook 'latex-mode-hook
	  (function (lambda () (require 'latex-paren))))

;; ;; Attiva di default la modalità LaTeX-math-mode per tutte le modalità di AUCTeX
;; (add-hook 'TeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook
	  '(lambda ()
;; Attiva automaticamente la correzione ortografica aprendo un documento LaTeX.
;; Finalmente \O/
	     (flyspell-mode)
;; Attiva la modalità con la quale un rigo non può essere più lungo di
;; `fill-column' caratteri
	     (turn-on-auto-fill)
;; Attiva di default la modalità LaTeX-math-mode per la latex-mode
	     (LaTeX-math-mode)))

;; http://soundandcomplete.com/2010/05/13/emacs-as-the-ultimate-latex-editor/
;; (require 'flymake)
;; (defun flymake-get-tex-args (file-name)
;;  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
;; (list "chktex" (list "-q" "-v0" file-name)))
;; (add-hook 'LaTeX-mode-hook 'flymake-mode)
;; l'ho commentato perché preferisco attivarlo manualmente, è molto dispendioso

(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-label-alist '(AMSTeX))
(setq TeX-electric-sub-and-superscript 1)

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t)))
