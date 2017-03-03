;;; sinomacs-sample.el --- the settings here are for demonstrating some of the features of sinomacs

(setq sinomacs-sample-dir (concat sinomacs-dir "sample/"))


(setq 	org-directory sinomacs-sample-dir
	org-default-notes-file (concat sinomacs-sample-dir "sinomacs-notes.org"))
(if (boundp 'org-agenda-files)
    (add-to-list 'org-agenda-files org-default-notes-file)
  (setq org-agenda-files (list org-default-notes-file)))

;; path to the bibliography
(setq  bibtex-completion-bibliography
   (concat sinomacs-sample-dir "sinomacs.bib"))

;; (bind-key* (kbd "<f7>") '(lambda () (interactive)
;; 			   (find-file-other-frame
;; 			    (concat sinomacs-sample-dir "sinomacs.org"))))

;;; sinomacs-sample.el
