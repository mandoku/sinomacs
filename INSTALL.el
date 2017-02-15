;;; INSTALLING SINOMACS

;; In order to install Sinomacs, all you need to do are these three
;; steps (if you are looking at this file within Emacs, then you have
;; already done the first two):
;; 
;; 1) Make sure that the sinomacs.zip file is extracted somewhere on
;;    your hard drive.
;;
;; 2) Start Emacs, and open up this file.  From the "File" menu,
;;    choose "Open File ..." Then look for INSTALL.el in the folder
;;    you just extracted.
;;
;; 3) In the "Emacs-Lisp" menu above, select the command called
;;    "Evaluate Buffer".
;;
;; That's all there is to it.  From now on, Easymacs will start up
;; automatically whenever you run Emacs.
;;
;; --------------------------------------------------------------











;; ---------------------------------------------------------------

;; This code simply creates or modifies an emacs configuration file in
;; the folder that Emacs regards as your Home folder.  If a
;; configuration file already exists, then it is modified to pre-pend a
;; line to it that looks like this:
;;
;; (load "path/to/sinomacs-folder/sinomacs.el")


;; Emacs code below

(let* ((sinomacs-dir (file-name-directory (or load-file-name
                                              buffer-file-name)))
       (sinomacs-file (concat sinomacs-dir "sinomacs.el"))
       (load-line (concat "(load \"" sinomacs-file "\")"))
       (init-file (or user-init-file "~/_emacs.el"))
       ;; Evade find-file advice
       (init-buffer (find-file-noselect init-file)))
  
  
  (with-current-buffer init-buffer
    (goto-char (point-min))
    (while (re-search-forward
           (concat "^(load \".*sinomacs\\.el\")$") nil t)
      (beginning-of-line)
      (insert ";")
      (forward-line))
    (goto-char (point-min))
    (if (boundp 'mandoku-base-dir)
        (insert "(setq mandoku-base-dir \"" mandoku-base-dir "\")\n"))
    (insert (concat ";; Load Sinomacs\n" load-line "\n;;\n"))
    (save-buffer))
  (kill-buffer nil)
  (unless (and (boundp 'sinomacs-loaded) sinomacs-loaded)
    (load sinomacs-file))
)  
