;;; Easymacs
;;
;; easymacs.el is a configuration for Emacs which is designed to be a learning environment for the digital humanities.  It provides an easy-to-install, cross-platform, comprehensive tool with key-bindings for basic editing tasks that should be familiar to non-technical users (some of these overwrite Emacs defaults).   It provides, for example, a schema-aware validating XML editor (nxml), a cross-platform command-line (eshell), integration with Git (magit) and a rich devopment environment for teaching programming in a variety of languages.
;; 
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; Maintainer: Peter Heslin <p.j.heslin@dur.ac.uk>
;; 
;; Copyright (C) 2003-16 Peter Heslin
;; 
;; This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License along with GNU Emacs; see the file COPYING.  If not, write to the Free Software Foundation, 675 Massachusettes Ave, Cambridge, MA 02139, USA.

(defvar easymacs-version "3.0")
(unless (string-match "^24.[56789]\\|^2[56789]\\|^[3456789]" emacs-version)
  (error "This version of Emacs is too old to run Easymacs; aborting."))
(defvar easymacs-dir (file-name-directory
		      (or load-file-name
			  buffer-file-name)))
(add-to-list 'load-path easymacs-dir)


;;; Set-up for packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
;; Needed by use-package
(use-package diminish :ensure t)
(use-package bind-key :ensure t)
;; Internal packages
(require 'misc)

;;; General Settings

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)
;; Use dialog boxes, if available
(setq use-dialog-box t)
;; Put current line number and column in the mode line
(line-number-mode 1)
(setq column-number-mode t)
;; Use menu-bar
(menu-bar-mode 1)
;; Paste at cursor, rather than pointer
(setq mouse-yank-at-point t)
;; Makes debugging easier
(setq message-log-max 1000)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
;; save command history
(savehist-mode 1)
;; Save our session
(require 'saveplace)
(setq-default save-place t)

;; set current buffer's filename, and full path in titlebar
;(setq frame-title-format '("Emacs %b" (buffer-file-name ": %f")))
;; Show path info in buffers with otherwise identical filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
;; Make very frequent autosaves
(setq auto-save-interval 5)
;; Make all backups in a single directory
(when (boundp 'backup-directory-alist)
  (let ((dir (expand-file-name "~/.emacs-backups")))
    (or (file-directory-p dir) (make-directory dir))
    (setq backup-directory-alist `(("." . ,dir)))))

;; Word wrapping
(use-package adaptive-wrap :ensure t)
(add-hook 'visual-line-mode-hook
	  (lambda ()
	    (adaptive-wrap-prefix-mode t)
	    (diminish 'visual-line-mode)))
(global-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; CUA-mode
(cua-mode t)
(setq cua-enable-cursor-indications t)
(setq cua-normal-cursor-color '(bar . "black")
      cua-overwrite-cursor-color '(box . "blue")
      cua-read-only-cursor-color '(box . "red"))

;; Undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (defalias 'undo 'undo-tree-undo)
  (defalias 'redo 'undo-tree-redo)
  :bind* (("C-z" . undo)
	 ("C-S-z" . redo)
	 ("C-y" . redo)
	 ("M-z" . undo-tree-visualize)))

;; Enable recently-opened files menu
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("[.]recentf" "[.]bm-repository$"
			   "[.]bmk$" "[.]abbrev_defs"
			  "[.]elc$" "ido.last" "autoloads.el"))
;; Save list when used, in case of crashes
(defadvice recentf-open-files (after easymacs-recentf-advice activate)
  (recentf-save-list))

;; Enable font-lock (syntax highlighting) in modes which support it
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; show matching and mismatching brackets etc
(setq show-paren-delay 0)
(show-paren-mode t)

;; Completion
(setq dabbrev-check-all-buffers t)
(use-package company
  :ensure t
  :diminish company-mode
  :config (global-company-mode)
  :bind* (("<f10>" . company-complete)
	  :map company-active-map
	  ("<escape>" . company-abort)))

;; Ido for buffer switching
(ido-mode 'buffer)
;; ibuffer-auto-mode refreshes after every command
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
(setq ido-use-virtual-buffers t)

; (use-package smex
;   :ensure t
;   :bind* (("M-x" . smex))
;   :config (smex-initialize))

;; Programming aids
(add-hook 'prog-mode-hook 'linum-mode)

(use-package magit
  :ensure t
  :bind* ("<C-f6>" . magit-status))
;; To finish magit sub-editor
(eval-after-load "with-editor"
    '(define-key with-editor-mode-map (kbd "<f12>") 'with-editor-finish))
(use-package git-gutter-fringe
  :ensure t
  :diminish 'git-gutter-mode
  :config (global-git-gutter-mode 1))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython")))
  
;; Visible bookmarks
(use-package bm :ensure t)

;;; Utility functions

(defun easymacs-comment-line-or-region (arg)
  (interactive)
  (let ((start (line-beginning-position))
	(end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
		    (goto-char (region-beginning))
		    (beginning-of-line)
		    (point))
	    end (save-excursion
		  (goto-char (region-end))
		  (end-of-line)
		  (point))))
    (comment-region start end arg)))

(defun easymacs-kill-buffer ()
    "Kill buffer and delete window if split without prompting"
    (interactive)
    (let ((buffer (current-buffer)))
      (ignore-errors (delete-window (selected-window)))
      (kill-buffer buffer)))

;;; Mac stuff
(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil
	mac-command-modifier 'control
	x-select-enable-clipboard t))
(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)))

;;; Spell-checking
;; Get hunspell dictionaries like so:
;; svn co https://src.chromium.org/chrome/trunk/deps/third_party/hunspell_dictionaries/
;; make sure that one dictionary is soft-linked to default.dic and default.aff
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;;; Auctex
(use-package tex-site
  :ensure auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master 'dwim)
(setq tex-default-mode 'latex-mode)
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(setq-default TeX-save-query nil)

;;(add-hook 'tex-mode-hook (function (lambda () (setq
;;						 ispell-parser 'tex))))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(setq reftex-plug-into-AUCTeX t)
(setq TeX-source-correlate-method 'synctex)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(add-hook 'LaTeX-mode-hook '(lambda ()
			      (flyspell-mode 1)))
  (defun LaTeX-insert-footnote ()
    "Insert a \\footnote{} macro in a LaTeX-document."
    (interactive)
    (TeX-insert-macro "footnote")
    (insert "\n")
    (forward-char)
    (insert " %")
    (unless (looking-at "\n")
      (insert "\n"))
    (backward-char 4))

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

(add-hook 'LaTeX-mode-hook '(lambda ()
    (local-set-key (kbd "C-e") 'LaTeX-insert-emph)
    (local-set-key (kbd "C-b") 'LaTeX-insert-textbf)
    (local-set-key (kbd "M-p") 'LaTeX-insert-textsc)
    (local-set-key (kbd "M-f") 'LaTeX-insert-footnote)
    (local-set-key (kbd "<f10>") 'TeX-complete-symbol)
    (local-set-key (kbd "<f11>") 'TeX-view)
    (local-set-key (kbd "<S-f11>") 'pdf-sync-forward-search)
    (local-set-key (kbd "<f12>") 'easymacs-run-latex)
    (local-set-key (kbd "<S-f12>") 'TeX-command-master)
    ))

;;; Eshell
;; Always save eshell history without asking
(setq eshell-save-history-on-exit 't)
(setq eshell-ask-to-save-history 'always)
(setq eshell-prefer-to-shell t)
;; Don't auto-complete ambiguities
(setq eshell-cmpl-cycle-completions nil)


;;; Dired

  ;; Make dired less weird -- it always opens new files or directories
  ;; in the current buffer, rather than endless spawning of new buffers
  (defun easymacs-dired-mouse-find-file-same-window (event)
    ;; Never open a new buffer from dired, even when clicking with the mouse
    ;; Modified from dired.el
    "In Dired, visit the file or directory name you click on."
    (interactive "e")
    (let (window pos file)
      (save-excursion
	(setq window (posn-window (event-end event))
	      pos (posn-point (event-end event)))
	(if (not (windowp window))
	    (error "No file chosen"))
	(set-buffer (window-buffer window))
	(goto-char pos)
	(setq file (dired-get-file-for-visit)))
      (select-window window)
      (find-alternate-file (file-name-sans-versions file t))))

  (eval-after-load "dired"
    '(progn
       ;; Never open a new buffer from dired, neither for files nor directories.
       (defadvice dired-find-file (around dired-subst-directory activate)
	 "Replace current buffer if file is a directory."
	 (interactive)
	 (let ((orig (current-buffer))
	       (filename (dired-get-filename nil t)))
	   ad-do-it
	   (kill-buffer orig)))
       (define-key dired-mode-map [mouse-2]
	 'easymacs-dired-mouse-find-file-same-window)
       (define-key dired-mode-map "^" (function
				       (lambda nil (interactive)
					 (find-alternate-file ".."))))))

;;; Isearch

(bind-key* (kbd "C-d") 'isearch-forward-symbol-at-point)
(bind-key* (kbd "C-f") 'isearch-forward)
(bind-key* (kbd "C-S-f") 'isearch-backward)
(bind-key* (kbd "C-r") 'replace-string)
(bind-key* (kbd "C-S-r") 'query-replace)
(bind-key* (kbd "M-r") 'replace-regexp)
(bind-key* (kbd "M-S-r") 'query-replace-regexp)
(bind-key* (kbd "C-*") 'easymacs-vi-star)
(bind-key* (kbd "C-#") 'easymacs-vi-hash)
(bind-key* (kbd "<S-f3>") 'occur)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-*") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-#") 'isearch-repeat-backward)
(define-key isearch-mode-map [escape] 'isearch-cancel)
(define-key isearch-mode-map (kbd "<C-up>") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<C-down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<f2>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<S-f2>") 'isearch-repeat-backward)

;;; Regexps: re-builder and pcre2el
(use-package pcre2el
  :ensure t
  :config (pcre-mode t)
  :diminish pcre-mode)

(use-package re-builder
  :config (setq reb-re-syntax 'pcre)
  :bind* (("<S-f3>" . re-builder)
	 :map reb-mode-map
	 ("<f2>" . reb-next-match)
	 ("<S-f2>" . reb-prev-match)
	 ("C-q" . reb-quit)
	 ("C-c" . reb-copy)))

;;; Elisp
(defun easymacs-elisp-help ()
  (interactive)
  (let ((sym (intern-soft (thing-at-point 'symbol))))
    (cond
     ((and sym
	   (fboundp sym)
	   (not (boundp sym)))
      (describe-function sym))
     ((and sym
	   (not (fboundp sym))
	   (boundp sym))
      (describe-variable sym))
     ((and sym
	   (fboundp sym)
	   (boundp sym))
      (if (yes-or-no-p "Both value and function are bound; describe function? ")
	  (describe-function sym)
	(describe-variable sym)))
     (t
      (call-interactively 'describe-function)))))
(define-key emacs-lisp-mode-map (kbd "<S-f10>") 'easymacs-elisp-help)
(define-key emacs-lisp-mode-map (kbd "<f10>")  'completion-at-point)
(define-key emacs-lisp-mode-map (kbd "<f11>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "<f12>") 'eval-defun)

(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook '(lambda ()
				   (turn-on-eldoc-mode)
				   (diminish 'eldoc-mode)))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;;; Global key-bindings
(bind-key* [escape] 'keyboard-escape-quit)
(bind-key* (kbd "<S-escape>") 'delete-other-windows)
(bind-key* (kbd "C-`") 'other-frame)
(bind-key* (kbd "<C-tab>") 'next-buffer)
(bind-key* (kbd "<C-S-tab>") 'previous-buffer)
(bind-key* (kbd "C-a") 'mark-whole-buffer)
(bind-key* (kbd "C-s") 'save-buffer)
(bind-key* (kbd "C-n") '(lambda () (interactive)
				 (let ((last-nonmenu-event nil))
				   (call-interactively 'find-file))))
(bind-key* (kbd "C-S-n") 'make-frame)
(bind-key* (kbd "C-o") '(lambda () (interactive)
				 (let ((last-nonmenu-event nil))
				   (call-interactively 'find-file-existing))))
(bind-key* (kbd "C-q") 'save-buffers-kill-emacs)
(bind-key* (kbd "C-w") 'easymacs-kill-buffer)
(bind-key* (kbd "C-S-w") 'easymacs-kill-some-buffers)

;;; Function keys

;; F1
(bind-key* (kbd "<f1>") 'ido-switch-buffer)
(bind-key* (kbd "<C-f1>") 'find-file)
(bind-key* (kbd "<M-f1>") 'recentf-open-files)
(bind-key* (kbd "<S-f1>") 'ibuffer)

;; F2
(bind-key* (kbd "<f2>") 'next-error)
(bind-key* (kbd "<S-f2>") 'previous-error)
(bind-key* (kbd "<M-f2>") 'bm-next)
(bind-key* (kbd "<M-S-fs>") 'bm-previous)
(bind-key* (kbd "<C-f2>") 'bm-toggle)

;; F3
(bind-key* (kbd "<f3>") 'dabbrev-expand)
(bind-key* (kbd "<S-f3>") '(lambda () (interactive)
			     (copy-from-above-command 1)))
(bind-key* (kbd "<C-f3>") '(lambda () (interactive)
			     (copy-from-above-command)))
(bind-key* (kbd "<M-f3>") '(lambda () (interactive)
			     (easymacs-comment-line-or-region 1)))
(bind-key* (kbd "<M-S-f3>") '(lambda () (interactive)
			     (easymacs-comment-line-or-region -1)))
;; F4
(bind-key* (kbd "<f4>") 'delete-other-windows)
(bind-key* (kbd "<S-f4>") 'other-window)
(bind-key* (kbd "<C-f4>") 'linum-mode)
(bind-key* (kbd "<M-f4>") 'save-buffers-kill-emacs)

;; F5
(bind-key* (kbd "<f5>") 'flyspell-auto-correct-previous-word)
(bind-key* (kbd "<S-f5>")
	   '(lambda () (interactive)
	      (eww (concat "http://www.wordnik.com/words/"
				  (substring-no-properties
				    (thing-at-point 'word))))))
(bind-key* (kbd "<C-f5>")
	   '(lambda () (interactive)
	      (eww (concat "http://moby-thesaurus.org/search?q="
				  (substring-no-properties
				    (thing-at-point 'word))))))


;; F6
(bind-key* (kbd "<f6>")
	   '(lambda () (interactive)
	      (if (string= (buffer-name) "*eshell*")
		  (switch-to-buffer (other-buffer (current-buffer)))
		(eshell))))
;; C-F6 is magit
