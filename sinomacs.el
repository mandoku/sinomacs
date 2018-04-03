;;; Sinomacs
;;
;; Sinomacs is a configuration for Emacs especially geared towards
;; Sinologists.  I is based on and heavily inspired by Easymacs, which
;; is developed and maintained by Peter Heslin and is a learning
;; environment for the digital humanities. Files that have been
;; retained from Easymacs have been moved to the folder easymacs. Some
;; of the general settings in the original easymacs.el have been moved
;; to this file. The original notice is still available in that
;; file. Easymacs can be inspected and downloaded from
;; https://github.com/pjheslin/easymacs
;; 
;; Original Author: Peter Heslin  <p.j.heslin@dur.ac.uk>
;; Copyright (C) 2003-16 Peter Heslin
;;
;; Author: Christian Wittern <cwittern@gmail.com>
;; Maintainer: Christian Wittern <cwittern@gmail.com>
;; 
;; Copyright (C) 2017- Christian Wittern
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Massachusettes Ave, Cambridge, MA
;; 02139, USA.

(defvar sinomacs-version "0.1")
(unless (string-match "^24.[56789]\\|^2[56789]\\|^[3456789]" emacs-version)
  (error "This version of Emacs is too old to run Easymacs; aborting."))

(defvar easymacs-dir (concat
		      (file-name-directory
		      (or load-file-name
			  buffer-file-name))
		      "easymacs/"))
(add-to-list 'load-path easymacs-dir)

(defvar sinomacs-dir (file-name-directory
		      (or load-file-name
			  buffer-file-name)))
(add-to-list 'load-path sinomacs-dir)

;;; Set-up for packages
(require 'package)
(setq package-enable-at-startup nil)
;; No TLS library for https on Windows
(add-to-list 'package-archives
	     (if (eq system-type 'windows-nt)
		 '("melpa" . "http://melpa.org/packages/")
	       '("melpa" . "https://melpa.org/packages/"))
	     t)
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
;;; Org
;; org needs to come early to avoid preloading of the built-in version
;; ensure at least version 9.0
(load "sinomacs-helper")
(sinomacs-package-update 'org '(9 0 0))
(use-package org
  :init
  (setq org-support-shift-select 'always
	org-return-follows-link t
	org-agenda-use-time-grid t
	org-log-done t
	org-agenda-include-diary t
	org-completion-use-ido t
	;; we need texlive, or at least xelatex for this to work:-)
	org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :config (run-at-time "00:59" 1800 'org-save-all-org-buffers)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c r" . org-capture)
   ("C-c L" . org-insert-link-global)
   ("M-<down>" . org-metadown)
   ("M-<up>" . org-metaup)
   ("C-c L" . org-insert-link-global)
   ("C-c o" . org-open-at-point-global))
  )
(use-package org-ref
  :ensure t)


;; Easymacs packages to load
(require 'fold-dwim)
(require 'tei-html-docs-p5)
;; Internal Emacs packages to load
(require 'misc)
(require 'hideshow)
(require 'outline)
(require 'thingatpt)

(require 'info)
(info-initialize)
(add-to-list 'Info-directory-list sinomacs-dir)

;;; General Settings

(setq message-log-max 1000)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq confirm-kill-emacs 'y-or-n-p)
;(kill-buffer (get-buffer "*scratch*"))
(set-language-environment "UTF-8")

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)
;; Use dialog boxes, if available
(setq use-dialog-box t)
;; Put current line number and column in the mode line
(line-number-mode 1)
(setq column-number-mode t)
; For visual-line-mode
(setq line-number-display-limit-width 2000000)
;; Use menu-bar
(menu-bar-mode 1)
;; Paste at cursor, rather than pointer
(setq mouse-yank-at-point t)
;; save command history
(savehist-mode 1)
;; Save our session
(require 'saveplace)
(setq-default save-place t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq frame-title-format 
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

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
;; Tabs are evil and break adaptive-wrap
(setq-default indent-tabs-mode nil)

;; CUA-mode
(cua-mode t)
(setq cua-enable-cursor-indications nil)
(setq cua-normal-cursor-color '(bar . "black")
      cua-overwrite-cursor-color '(box . "blue")
      cua-read-only-cursor-color '(box . "red"))
;; Keep selection active after copy
(setq cua-keep-region-after-copy t)

;; redo+ -- simple, linear undo/redo
;; this is gone from melpa, removing for now to avoid breaking the sinomacs install 2018-04-03
;(use-package redo+
;   :ensure t
;   :bind* (("C-z" . undo)
; 	 ("C-S-z" . redo)))

;; Enable recently-opened files menu
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("[.]recentf" "[.]bm-repository$"
			   "[.]bmk$" "[.]abbrev_defs"
			   "[.]elc$" "ido.last" "autoloads.el"
			   "sinomacs-help.txt"))
;; Save list when used, in case of crashes
(defadvice recentf-open-files (after sinomacs-recentf-advice activate)
  (recentf-save-list))

;; Enable font-lock (syntax highlighting) in modes which support it
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; show matching and mismatching brackets etc
(setq show-paren-delay 0)
(show-paren-mode t)

;; Completion
(setq dabbrev-check-all-buffers t)
;; (use-package company
;;   :ensure t
;;   :diminish company-mode
;;   :config (progn
;; 	    (global-company-mode)
;; 	    (setq company-idle-delay nil))
;;   :bind* (("<f3>" . company-complete)
;; 	  :map company-active-map
;; 	  ("<escape>" . company-abort)))

;; Ido and ibuffer for buffer switching
(ido-mode 'buffer)
(setq ido-use-virtual-buffers t)
;; Ignore non-user files in ido
(defun easymacs-ido-ignore (name)
  "Ignore all non-user (a.k.a. *starred*) buffers except *eshell*."
  (and (string-match "^\*" name)
       (not (string= name "*eshell*"))))
;(setq ido-ignore-buffers '("\\` " easymacs-ido-ignore))

(require 'ibuffer)
(require 'ibuf-ext)
;; Simplified ibuffer display
(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 0 -1 :left :elide))))
;; Make ibuffer refresh after every command
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; imenu
(defun imenu-or-not ()
  "Try to add an imenu when we visit a file, catch and nil if
the mode doesn't support imenu."
  (condition-case nil
      (imenu-add-menubar-index)
    (error nil)))
(add-to-list 'find-file-hooks 'imenu-or-not)
(setq imenu-max-items 50
      imenu-scanning-message nil
      imenu-auto-rescan t)

;; (use-package drag-stuff
;;   :ensure t
;;   :diminish 'drag-stuff-mode
;;   :config (progn (drag-stuff-global-mode 1)
;;                  (drag-stuff-define-keys)))

;; Programming tools
(add-hook 'prog-mode-hook 'linum-mode)


(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :bind* ("<C-f6>" . magit-status)
  :config (setq magit-diff-refine-hunk 'all))
;; To finish magit sub-editor
(eval-after-load "with-editor"
    '(define-key with-editor-mode-map (kbd "<f12>") 'with-editor-finish))
(use-package git-gutter-fringe
  :ensure t
  :diminish 'git-gutter-mode
  :bind* (("<M-f6>" . git-gutter:next-hunk)
	  ("<S-M-f6>" . git-gutter:previous-hunk)))
;; Not sure why this doesn't work with autoloads
(require 'git-gutter-fringe)
(global-git-gutter-mode 1)
(setq git-gutter:update-interval 0)

(defun easymacs-git-wdiff ()
  (interactive)
  (let ((inhibit-read-only t)
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix)
        (git-command (read-string "Git command: "
                                  "git diff --color-words HEAD")))
    (shell-command git-command "*git-wdiff*")
    (switch-to-buffer "*git-wdiff*")
    (delete-other-windows)
    (ansi-color-apply-on-region (point-min) (point-max))
    (visual-line-mode 1)
    (diff-mode)
    (read-only-mode)))
(bind-key* (kbd "<C-S-f6>") 'easymacs-git-wdiff)


;; Visible bookmarks

(use-package bm
         :ensure t
         :demand t
         :init
         (setq bm-restore-repository-on-load t)
         :config
         (setq bm-highlight-style 'bm-highlight-only-fringe)
         (setq bm-cycle-all-buffers t)
         (setq bm-repository-file (expand-file-name "~/.emacs.d/bm-repository"))
         (setq-default bm-buffer-persistence t)
         (add-hook' after-init-hook 'bm-repository-load)
         (add-hook 'find-file-hooks 'bm-buffer-restore)
         (add-hook 'kill-buffer-hook #'bm-buffer-save)
         (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))
         (add-hook 'after-save-hook #'bm-buffer-save)
         (add-hook 'find-file-hooks   #'bm-buffer-restore)
         (add-hook 'after-revert-hook #'bm-buffer-restore)
         (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
         (global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
         (global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
         (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
         :bind (("<f5>" . bm-next)
                ("S-<f5>" . bm-previous)
                ("M-<f5>" . bm-show-all)
                ("C-<f5>" . bm-toggle)))


;; Folding for fold-dwim
(setq hs-isearch-open t)
(setq hs-hide-comments-when-hiding-all t)
(setq hs-allow-nesting t)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'outline-minor-mode)
(add-hook 'text-mode-hook #'outline-minor-mode)
(diminish 'hs-minor-mode)
(diminish 'outline-minor-mode)

(use-package browse-kill-ring
  :ensure t
  :bind* ("<C-S-v>" . browse-kill-ring))

;; Improve Scrolling Behaviour

;; smooth-scrolling.el keeps several lines of context visible when the cursor nears the top or bottom of the screen, and when the cursor hits that limit it scrolls by a single line rather than jumping by a page.

 (use-package smooth-scrolling
   :ensure t
   :diminish 'smooth-scrolling-mode
   :config (progn (smooth-scrolling-mode 1)
                  (setq smooth-scroll-margin 5)))

;; smooth-scroll.el has a very different purpose: it implements a scrolling motion when paging up and down.  But this does not work well with the reversible paging below, and it is really just a visual effect; so it is not enabled here.  But the functions to scroll without moving the cursor are useful.

(use-package smooth-scroll
  :ensure t
  :diminish 'smooth-scroll-mode
  :config (smooth-scroll-mode -1)
  :bind* (("<C-down>"  . scroll-up-1)
          ("<C-up>"    . scroll-down-1)
          ("<C-left>"  . scroll-right-1)
          ("<C-right>" . scroll-left-1)))

;; Code adapted from Emacswiki/Stack Overflow to implement reversible paging.  Paging down and then back up puts you back in the same spot where you started.

(defun sfp-page-down (&optional arg)
  (interactive "^P")
  (setq this-command 'next-line)
  (let ((smooth-scrolling-mode nil))
    (next-line
     (- (window-text-height)
        next-screen-context-lines))))
(put 'sfp-page-down 'isearch-scroll t)
(put 'sfp-page-down 'CUA 'move)

(defun sfp-page-up (&optional arg)
  (interactive "^P")
  (setq this-command 'previous-line)
  (let ((smooth-scrolling-mode nil))
    (previous-line
     (- (window-text-height)
        next-screen-context-lines))))

(put 'sfp-page-up 'isearch-scroll t)
(put 'sfp-page-up 'CUA 'move)

(global-set-key [next] 'sfp-page-down)
(global-set-key [prior] 'sfp-page-up)

(defun sinomacs-update-sinomacs ()
  "This function only works when Sinomacs has been cloned from the GitHub repository."
  (interactive)
  (let ((default-directory sinomacs-dir))
    (magit-pull-from-upstream (magit-pull-arguments))))

;;; Mandoku
(use-package gh
  :ensure t)
(use-package github-clone
  :init (setq github-clone-url-slot :clone-url)
  :ensure t)
(use-package mandoku
  :init ;(setq mandoku-base-dir "~/krp/")
  (setq mandoku-do-remote t)
  (setq mandoku-string-limit 10)
  ;(setq mandoku-ngram-n 4)
  (setq mandoku-index-display-limit 2000)
  (setq mandoku-repositories-alist
	'(("KR" "http://www.kanripo.org/api/v1.0") ))
  (autoload 'mandoku-view-mode "mandoku" nil t)
  (setq org-startup-folded 'showall)
  :config
  (mandoku-initialize)
  (define-key mandoku-view-mode-map (kbd "C-c i")  'mandoku-open-image-at-page)
  (define-key mandoku-view-mode-map (kbd "C-c d")  'mandoku-get-remote-text-now)
  (define-key mandoku-view-mode-map (kbd "C-<f8>")  'org-store-link)
  (define-key mandoku-view-mode-map (kbd "<f8>")  'mandoku-annot-annotate)
  :bind (("<f6>" . mandoku-search-text)
	 ("<f7>"  . mandoku-search-titles)
	 ("S-<f6>" . mandoku-search-user-text)
	 ("S-<f7>" . mandoku-show-catalog)
	 ("C-c p" . mandoku-link-insert-link))
  )

;;; isearch yank (courtesy of Sacha Chua )
(defun sinomacs-isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))
(define-key isearch-mode-map (kbd "C-x") 'sinomacs-isearch-yank-current-word)

;;; helm, bib
(use-package helm)
(use-package helm-wordnet)
(use-package helm-bibtex
  :config
  (setq
   bibtex-completion-pdf-field "File")
  )


;;; Markdown
(use-package markdown-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :bind (:map markdown-mode-map
	      ("<f10>" . markdown-other-window)
	      ("<f11>" . markdown-preview)
	      ("<f12>" . markdown-live-preview-mode)))
;;; Global key-bindings
;(bind-key* [escape] 'keyboard-escape-quit)
;(bind-key* (kbd "<S-escape>") 'delete-other-windows)
(bind-key* (kbd "C-`") 'other-frame)
(bind-key* (kbd "C-a") 'mark-whole-buffer)
(bind-key* (kbd "C-s") 'save-buffer)
(bind-key* (kbd "C-n") '(lambda () (interactive)
				 (let ((last-nonmenu-event nil))
				   (call-interactively 'find-file))))
(bind-key* (kbd "C-S-n") '(lambda () (interactive)
			    (find-file-other-frame
			     (concat sinomacs-dir "sinomacs-help.txt"))))
(bind-key* (kbd "C-o") '(lambda () (interactive)
				 (let ((last-nonmenu-event nil))
				   (call-interactively 'find-file-existing))))
(bind-key* (kbd "C-q") 'save-buffers-kill-emacs)
(bind-key* (kbd "C-w") 'easymacs-kill-some-buffers)
(bind-key* (kbd "S-C-w") 'delete-frame)
;(bind-key* (kbd "M-w") 'easymacs-kill-some-buffers)

(bind-key* (kbd "<end>") 'end-of-visual-line)
(bind-key* (kbd "<home>") 'beginning-of-visual-line)

;;; Function keys

;; F1
(bind-key* (kbd "<f1>") '(lambda () (interactive)
			    (find-file
			     (concat sinomacs-dir "sinomacs-help.txt"))
			    (goto-char (point-min))
			    ))
(bind-key* (kbd "<S-f1>") 'ido-switch-buffer)
(bind-key* (kbd "<C-f1>") 'ibuffer)
(bind-key* (kbd "<S-C-f1>") '(lambda () (interactive) (find-file "")))
(bind-key* (kbd "<M-f1>") 'recentf-open-files)
(bind-key* (kbd "<S-M-f1>") 'ffap)

;; F2
;; first: remove unwanted keybindings
(defhydra hydra-search ()
; we want to avoid using "c" here since that is used in ctext dic buffer
      "Search"
      ("o" occur  "List search term in this file\n" :exit t)
      ("g" sinomacs-grep "List search term in this directory\n" :exit t)
      ("c" sinomacs-get-pinyin-for-char "Lookup pinyin for character, add to clipboard.\n" :exit t)
      ("d" sinomacs-ctext-dict-region "Lookup search term in Ctext.org dictionary\n" :exit t)
      ("f" sinomacs-chise-find "Lookup character in CHISE IDS\n" :exit t)
      )
(defhydra hydra-zoom ()
      "zoom"
      ("+" text-scale-increase "in")
      ("-" text-scale-decrease "out"))
(bind-key* (kbd "<f2>") 'hydra-search/body)
(bind-key* (kbd "<S-f2>") 'hydra-zoom/body) 
;(bind-key* (kbd "<C-f2>") 'sinomacs-ctext-dict-region)
(bind-key* (kbd "<M-f2>")
	   '(lambda () (interactive)
	      (eww (concat "http://www.chise.org/ids-find?components="
			   (read-string "Search in CHISE IDS find for: "(char-to-string (char-after)))))))
(bind-key* (kbd "<S-M-f2>")
	   '(lambda () (interactive)
	      (eww (concat "http://moby-thesaurus.org/search?q="
				  (substring-no-properties
				    (thing-at-point 'word))))))

;; F3 is company-complete (defined above)
(bind-key* (kbd "<f3>") 'sinomacs-bibl-helm)
(bind-key* (kbd "<S-f3>") 'helm-bibtex)
(bind-key* (kbd "<M-f3>") 'mandoku-show-catalog)
(bind-key* (kbd "<C-f3>")
	   '(lambda () (interactive)
	      (eww (concat "http://kanji.zinbun.kyoto-u.ac.jp/kanseki?query="
			   (read-string "Search in Kanseki Database for: ")))))
(bind-key* (kbd "<C-S-f3>") 'sinomacs-ddbc-authority-search-person)
(bind-key* (kbd "<M-S-f3>") 'sinomacs-ddbc-authority-search-place)

;; F4
(bind-key* (kbd "<f4>") 'other-window)
(bind-key* (kbd "<S-f4>") 'delete-other-windows)
(bind-key* (kbd "<C-f4>") 'split-window-below)
(bind-key* (kbd "<C-S-f4>") 'split-window-right)
(bind-key* (kbd "<M-f4>") 'save-buffers-kill-emacs)

;; F5
;; F5 bookmark binding see above
;; We want M-F5 and M-S-F5 to be overridden
;(bind-key (kbd "<M-f5>") 'next-error)
;(bind-key (kbd "<M-S-f5>") 'previous-error)

;; F6
;; C-F6 is magit-status, defined above
;; (S-)M-F6 is git-gutter:next-hunk and previous-hunk
;; (bind-key* (kbd "<f6>")
;; 	   '(lambda () (interactive)
;; 	      (if (string= (buffer-name) "*eshell*")
;; 		  (switch-to-buffer (other-buffer (current-buffer)))
;; 		(easymacs-eshell))))

;; F7
;; (bind-key* (kbd "<M-f7>") 'fold-dwim-hide-all)
;; (bind-key* (kbd "<S-M-f7>") 'fold-dwim-show-all)
;; (bind-key* (kbd "<C-f7>") 'outline-next-visible-heading)
;; (bind-key* (kbd "<S-C-f7>") 'outline-previous-visible-heading)

;; F8 


;; F12
(bind-key* (kbd "<f12>")  'org-agenda)

;;; Load the lisp files in sinomacs-dir

(ignore-errors 
    (mapc 'load (directory-files (concat sinomacs-dir "lisp") 't "^[^#].*el$")))


;;; Show help screen at startup

;; Workaround for a frame-related mac bug
(find-file-other-frame
 (concat sinomacs-dir "sinomacs-help.txt"))
(delete-other-frames)
(goto-char (point-min))
(cd (expand-file-name "~/"))
(defun kill-unkillable-buffer ()
  (message "%s" "Error: you cannot close this file.")
  nil)
(make-local-variable 'kill-buffer-query-functions)
(add-hook 'kill-buffer-query-functions 'kill-unkillable-buffer)
(read-only-mode)
(view-mode)

;;; Load the remains of the Easymacs package
(require 'easymacs)

;;; Sample for demonstration.  Remove this if not needed anymore
(load (concat sinomacs-dir "sample/sinomacs-sample"))

(if (fboundp 'hydra-tls/body)
    (bind-key* (kbd "<f8>") 'hydra-tls/body))


(provide 'sinomacs)
;; sinomacs.el ends here
