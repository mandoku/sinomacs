;;; Packages
(require 'package)
(package-initialize)
(require 'diminish)
(require 'adaptive-wrap)
(require 'bind-key)
(require 'misc)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

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
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'undo 'undo-tree-undo)
(defalias 'redo 'undo-tree-redo)
(diminish 'undo-tree-mode)

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

;; save command history
(savehist-mode 1)
;; Save our session
(require 'saveplace)
(setq-default save-place t)

(setq dabbrev-check-all-buffers t)

;; buffer switching
(ido-mode 'buffer)
;; ibuffer-auto-mode refreshes after every command
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; programming
(add-hook 'prog-mode-hook 'linum-mode)

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
	x-select-enable-clipboard t)
  (exec-path-from-shell-initialize))

;;; Spell-checking
;; Get hunspell dictionaries like so:
;; svn co https://src.chromium.org/chrome/trunk/deps/third_party/hunspell_dictionaries/
;; make sure that one dictionary is soft-linked to default.dic and default.aff
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;;; Auctex
(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)
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

;;; Visible bookmarks

;; Make sure the repository is loaded as early as possible
(setq bm-restore-repository-on-load t)
(require 'bm)
;; Loading the repository from file when on start up.
(add-hook' after-init-hook 'bm-repository-load)
;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)
;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when Emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
				(bm-buffer-save-all)
				(bm-repository-save)))
;; Update bookmark repository when saving the file.
(add-hook 'after-save-hook 'bm-buffer-save)
;; Restore bookmarks when buffer is reverted.
(add-hook 'after-revert-hook 'bm-buffer-restore)
;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)


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
(require 'pcre2el)
(setq reb-re-syntax 'pcre)
(pcre-mode t)
(diminish 'pcre-mode)
(bind-key* (kbd "<S-f3>") 're-builder)
(define-key reb-mode-map (kbd "<f2>") 'reb-next-match)
(define-key reb-mode-map (kbd "<S-f2>") 'reb-prev-match)
(define-key reb-mode-map (kbd "<C-q>") 'reb-quit)
(define-key reb-mode-map (kbd "<C-c>") 'reb-copy)

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
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
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
(bind-key* (kbd "C-z") 'undo)
(bind-key* (kbd "C-S-z") 'redo)
(bind-key* (kbd "C-y") 'redo)
(bind-key* (kbd "M-z") 'undo-tree-visualize)

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
(bind-key* (kbd "<C-f6>")     'magit-status)
(eval-after-load "with-editor"
    '(define-key with-editor-mode-map (kbd "<f12>") 'with-editor-finish))

