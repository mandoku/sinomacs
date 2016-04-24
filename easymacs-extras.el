;;; Some additions to Easymacs that are for advanced users or that depend upon external applications

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

