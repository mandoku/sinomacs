
(defun sinomacs-get-char ())


(defun sinomacs-grep (string &optional beg end)
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'word)))
       (list nil (car bds) (cdr bds)))))
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	workOnStringP inputStr)
    (setq workOnStringP (if string t nil))
    (setq inputStr
	  (read-string "Search in current directory for: "
		       (if workOnStringP string (buffer-substring-no-properties beg end))))
    (grep (concat grep-program " --color -r -n -e \""  inputStr "\" " (file-name-directory
								       (or (buffer-file-name)
									   default-directory))))))

    
