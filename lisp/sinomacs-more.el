
(defun sinomacs-get-char ())

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

