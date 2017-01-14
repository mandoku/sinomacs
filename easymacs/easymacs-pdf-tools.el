;;; PDF-tools as LaTeX viewer: very handy, but tricky to install

;; This is very slow to load, so defer.
(use-package pdf-tools
  :ensure t
  :defer t)

(use-package org-pdfview
  :config
  (eval-after-load 'org '(require 'org-pdfview))
  (delete '("\\.pdf\\'" . default) org-file-apps)
  ;; this is for org version 9.0
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link) (org-pdfview-open link))))
  )

(defun easymacs-pdf-tools-require ()
  (unless (featurep 'pdf-tools)
    (message "Please wait: initializing PDF Tools")
    (require 'pdf-tools)
    (pdf-tools-install)))

(add-to-list 'auto-mode-alist
	     '("\\.pdf\\'" .
	       (lambda ()
		 (easymacs-pdf-tools-require)
		 (add-hook 'pdf-tools-enabled-hook 'auto-revert-mode))))

(add-hook 'TeX-after-compilation-finished-functions
	  'TeX-revert-document-buffer)
(setq revert-without-query (quote (".*.pdf")))

(defun easymacs-TeX-pdf-tools-sync-view ()
  (easymacs-pdf-tools-require)
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
