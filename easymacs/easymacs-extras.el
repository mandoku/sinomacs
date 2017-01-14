;;; Some additions to Easymacs that are for advanced users or that depend upon external applications

;; Python

;; Elpy + yasnippet is a bit of a heavyweight install and beginners
;; might do better with Idle, so this is an extra.

;; Python for Mac
;; brew install python3

;; Useful modules
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

;; Somewhat annoying package
;(use-package yasnippet
;  :ensure t
;  :config (yas-global-mode 1))

;;; Auctex

(use-package tex-site
  :ensure auctex)
(use-package company-auctex
  :ensure t
  :config (company-auctex-init))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master 'dwim)
(setq tex-default-mode 'latex-mode)
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(setq-default TeX-save-query nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
(setq reftex-plug-into-AUCTeX t)
(setq TeX-source-correlate-method 'synctex)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)
(add-hook 'LaTeX-mode-hook '(lambda ()
			      (TeX-fold-mode 1)
			      ;; For folding comments
			      (hs-minor-mode 1)))

; (defun LaTeX-insert-footnote ()
;   "Insert a \\footnote{} macro in a LaTeX-document."
;   (interactive)
;   (TeX-insert-macro "footnote")
;   (insert "\n")
;   (forward-char)
;   (insert " %")
;   (unless (looking-at "\n")
;     (insert "\n"))
;   (backward-char 4))

(defun LaTeX-insert-footnote ()
  "Insert a \\footnote{} macro in a LaTeX-document."
  (interactive)
  (TeX-insert-macro "footnote"))

(defun LaTeX-insert-emph ()
  "Insert an \\emph{} macro in a LaTeX-document."
  (interactive)
  (TeX-insert-macro "emph"))

(defun LaTeX-insert-textbf ()
  "Insert a \\textbf{} macro in a LaTeX-document."
  (interactive)
  (TeX-insert-macro "textbf"))

(defun LaTeX-insert-textsc ()
  "Insert a \\textsc{} macro in a LaTeX-document."
  (interactive)
  (TeX-insert-macro "textsc"))

(defun LaTeX-insert-uline ()
  "Insert a \\uline{} macro in a LaTeX-document."
  (interactive)
  (TeX-insert-macro "uline"))

(defun easymacs-run-latex ()
  "Save and LaTeX `TeX-master-file' (without querying the user).
Any files \\input by `TeX-master-file' are also saved without prompting."
  (interactive)
  (let (TeX-save-query)
    (TeX-save-document (TeX-master-file)))
  (TeX-command "LaTeX" 'TeX-master-file))

(defun easymacs-auctex-help-at-point ()
  (interactive)
  (save-excursion
    (goto-char (or (TeX-find-macro-start)
		   (re-search-backward (regexp-quote TeX-esc) nil t)))
    (re-search-forward (concat (regexp-quote TeX-esc) "\\sw+") nil t)
    (info-lookup-symbol (match-string 0))))

(add-hook 'LaTeX-mode-hook '(lambda ()
    (local-set-key (kbd "C-e") 'LaTeX-insert-emph)
    (local-set-key (kbd "C-b") 'LaTeX-insert-textbf)
    (local-set-key (kbd "M-p") 'LaTeX-insert-textsc)
    (local-set-key (kbd "M-f") 'LaTeX-insert-footnote)
    (local-set-key (kbd "<f9>") 'easymacs-auctex-help-at-point)
    (local-set-key (kbd "<S-f9>")  'reftex-grep-document)
    (local-set-key (kbd "<f10>") 'LaTeX-close-environment)
    (local-set-key (kbd "<S-f10>") 'TeX-complete-symbol)
    (local-set-key (kbd "<M-f10>") 'LaTeX-environment)
    (local-set-key (kbd "<M-S-f10>") '(lambda () (interactive)
					(LaTeX-environment t)))
    (local-set-key (kbd "<f11>") 'TeX-view)
    ;; Needs pdf-tools
    (local-set-key (kbd "<C-f11>") 'easymacs-latex-pdf-frame)
    (local-set-key (kbd "<S-f11>") 'pdf-sync-forward-search)
    (local-set-key (kbd "<f12>") 'easymacs-run-latex)
    (local-set-key (kbd "<S-f12>") 'TeX-command-master)))


;; Bibtex
;; Prompt for bibtex entry types
(defun easymacs-insert-bibtex-entry ()
  (interactive)
  (funcall (intern
	    (concat "bibtex-"
		    (completing-read
		     "Entry type (tab for list): " 
		     (mapcar 'car bibtex-entry-field-alist))))))
(add-hook 'bibtex-mode-hook '(lambda ()
			      (local-set-key (kbd "<f12>")
					     'bibtex-clean-entry)
			      (local-set-key (kbd "<f11>")
					     'easymacs-insert-bibtex-entry)
			      (local-set-key (kbd "<f10>")
					     'bibtex-fill-entry)
			      (local-set-key (kbd "C-e")
					     'LaTeX-insert-emph)))

(add-hook 'reftex-index-phrases-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-e")
			   'LaTeX-insert-emph)))

