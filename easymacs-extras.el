;;; Some additions to Easymacs that depend upon external applications

;;; PDF-tools as LaTeX viewer

;; Done on demand, since require first time is slooow.
(add-to-list 'auto-mode-alist
	     '("\\.pdf\\'" .
	       (lambda ()
		 (require 'pdf-tools)
		 (pdf-tools-install)
		 (add-hook 'pdf-tools-enabled-hook 'auto-revert-mode))))

(add-hook 'TeX-after-compilation-finished-functions
	  'TeX-revert-document-buffer)
(setq revert-without-query (quote (".*.pdf")))

(setq TeX-view-program-list
      '(("PDF Tools" TeX-pdf-tools-sync-view)))

(setq TeX-view-program-selection'((output-dvi "open")
				  (output-pdf "PDF Tools")
				  (output-html "open")))
