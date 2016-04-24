;;; PDF-tools as LaTeX viewer: very handy, but tricky to install

;; This is very slow to load, so defer.
(use-package pdf-tools
  :ensure t
  :defer t)

;; Done on demand, since require first time is so slow.
(add-to-list 'auto-mode-alist
	     '("\\.pdf\\'" .
	       (lambda ()
		 (require 'pdf-tools)
		 (pdf-tools-install)
		 (add-hook 'pdf-tools-enabled-hook 'auto-revert-mode))))

(add-hook 'TeX-after-compilation-finished-functions
	  'TeX-revert-document-buffer)
(setq revert-without-query (quote (".*.pdf")))

(defun easymacs-TeX-pdf-tools-sync-view ()
    ;; Delayed require
    (require 'pdf-tools)
    (TeX-pdf-tools-sync-view))

(setq TeX-view-program-list
      '(("PDF Tools" easymacs-TeX-pdf-tools-sync-view)))

(setq TeX-view-program-selection'((output-dvi "open")
				  (output-pdf "PDF Tools")
				  (output-html "open")))

(defun easymacs-latex-pdf-frame ()
  (interactive)
  (let ((output-file (TeX-active-master (TeX-output-extension))))
    (if (file-exists-p output-file)
	(progn
	  (find-file-other-frame output-file)
	  (revert-buffer))
      (message "Output file %S does not exist." output-file))))
