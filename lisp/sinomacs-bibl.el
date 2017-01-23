;;; sources for texts

(defun sinomacs-bibl-read-catalogs ()
  (dolist (file sinomacs-bibl-catalog-files)
    (when (file-exists-p file)
      (with-temp-buffer
	(let ((coding-system-for-read 'utf-8)
	      (lcnt 0)
	      tvol textid)
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (forward-line 1)
	    (incf lcnt)
	    (if (looking-at "*")
		
	    (while (re-search-forward "^\\([A-z0-9]+\\)	\\([^	]+\\)	\\([^	
]+\\)" nil t)
	  (puthash (match-string 1) (match-string 3) mandoku-titles)
	  (puthash (match-string 1) (match-string 2) mandoku-textdates)
	  (if (< (length (match-string 1)) 6)
	      (puthash (match-string 1) (match-string 3) mandoku-subcolls))
	      
	  )))))
)))

(defun sinomacs-bibl-helm-candidates ()
  "Helm source for text lists"
  (let (l x)
    ;; first lets add the mandoku titles
    (maphash (lambda (k v)
	       (push (concat k " " (replace-regexp-in-string "-" " " v)) l)
	       ) mandoku-titles)
    ;; now process the other catalogs
    
    (sort l 'string-lessp)
    ))

(defun sinomacs-bibl-helm ()
  (interactive)
  (let ((sinomacs-bibl-helm-source
      '((name . "Texts")
        (candidates . sinomacs-bibl-helm-candidates)
	;; (persistent-action . (lambda (candidate)
	;; 			    (message-box
	;; 			     (mapconcat 'identity (tls-helm-tree-candidates candidate) "\n")
	;; 			     )))
        (action . (("Open" . (lambda (candidate)
			       (let ((c (substring candidate 0 2)))
				 (cond ((equal c "KR")
					(org-open-link-from-string (concat "mandoku:" (car (split-string candidate " ")))))
					((equal c "XX")
					 (find-file "/tmp/testb.txt"))
;			       (find-file
;				(concat mandoku-tls-lexicon-path "concepts/" mandoku-tls-concept ".org") t)
			       (message "%s" candidate))))))
		)))
	(fallback-source '((name . "Create new concept")
                          (dummy)
                          (action . mandoku-tls-create-new-annot)) ))

    (helm :sources '(sinomacs-bibl-helm-source fallback-source))))

