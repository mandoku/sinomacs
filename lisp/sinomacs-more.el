;;; sinomacs-more --- Additional miscelleaneous routines for Sinomacs

(defun sinomacs-get-pinyin-for-char ()
  "This will call helm-charinfo and return the pinyin of the selected character."
  (interactive)
  (let ((sel (split-string (helm-charinfo))))
    (kill-new (car sel))
    (kill-new (cadr sel))))

(defun sinomacs-get-pinyin (string &optional beg end)
  "Get pinyin for the selected region or for a string provided. If called with the universal argument, provide all pinyin readings."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'word)))
       (list nil (car bds) (cdr bds)))))
  (let ((all (if (equal current-prefix-arg '(4)) t nil))
	workOnStringP inputStr )
    (setq workOnStringP (if string t nil))
    (setq inputStr
          (if workOnStringP
	      string
	    (if (and beg end)
		(buffer-substring-no-properties beg end)
	      (read-string "Enter the character(s) you are looking up pinyin for: "))))
    (kill-new (helm-charinfo-get-pinyin inputStr all))
    (unless workOnStringP
      (message (concat "\"" (car kill-ring-yank-pointer) "\" copied to clipboard")))))

(defun sinomacs-grep (string &optional beg end)
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'word)))
       (list nil (car bds) (cdr bds)))))
  (let (workOnStringP inputStr)
    (setq workOnStringP (if string t nil))
    (setq inputStr
          (if workOnStringP string
            (read-string "Search in current directory for: ")))
    (sinomacs-grep--intern inputStr
                           (file-name-directory
                            (or (buffer-file-name)
                                default-directory))
                           )))

(defun sinomacs-grep--intern (string grepdir &optional beg end)
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8))
    (grep (concat grep-program " --color -r -n -e \""  string "\" " grepdir))))



;;(set-window-margins win width (cdr (window-margins win)))))

