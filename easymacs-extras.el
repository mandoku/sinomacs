;;; Some additions to Easymacs that are advanced or that depend upon external applications

;;; PDF-tools as LaTeX viewer

(use-package pdf-tools
  :ensure t)

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

(defun easymacs-latex-pdf-frame ()
  (interactive)
  (let ((output-file (TeX-active-master (TeX-output-extension))))
    (if (file-exists-p output-file)
	(progn
	  (find-file-other-frame output-file)
	  (revert-buffer))
      (message "Output file %S does not exist." output-file))))

;; Elpy + yasnippet is a bit of a heavyweight install and beginners
;; might do better with Idle, so this is an extra.

;; Python
;; brew install python3
;; pip install virtualenv rope jedi ipython nltk

(use-package elpy
  :ensure t
  :config (progn
	    (elpy-enable)
	    (setq elpy-rpc-python-command "python3")
	    ;; Why does this default to being on for all major modes?
	    (pyvenv-mode -1)
	    ;(elpy-use-ipython)
	    ))
;; :bind doesn't work
(add-hook 'elpy-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<f5>") 'elpy-flymake-next-error)
	     (local-set-key (kbd "<S-f5>") 'elpy-flymake-previous-error)
	     (local-set-key (kbd "<f9>") 'elpy-doc)
	     (local-set-key (kbd "<f10>") 'elpy-check)
	     (local-set-key (kbd "<S-f10>") 'elpy-format-code)
	     (local-set-key (kbd "<C-f10>") 'elpy-refactor)
	     (local-set-key (kbd "<f11>") 'elpy-shell-switch-to-shell)
	     (local-set-key (kbd "<f12>")
			    'elpy-shell-send-region-or-buffer)))
	  
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --classic")

(add-hook 'inferior-python-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<f11>") 'elpy-shell-switch-to-buffer)))

;; Installed by elpy as a dependency, so we might as well turn it on
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

