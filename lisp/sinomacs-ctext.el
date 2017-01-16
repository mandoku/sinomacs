;;

(defun sinomacs-ctext-dict-region (&optional beg end)
  (interactive "r")
  (if (region-active-p)
      (sinomacs-ctext-dict (buffer-substring beg end))
    (sinomacs-ctext-dict (format "%c" (following-char)))
    (copy-to-register "0" beg end)
    ))

(global-set-key (kbd "C-c c") 'sinomacs-ctext-dict-region)

(defun sinomacs-ctext-dict  (search)
  (eww-browse-url (concat "http://ctext.org/dictionary.pl?if=en&char=" search))
  (switch-to-buffer "*eww*"))


(eval-after-load "eww"
  '(progn (define-key eww-mode-map "c" 'sinomacs-eww-ctext-dict-position)
	  (define-key eww-mode-map "i" 'sinomacs-eww-isbn-helper)
	   (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))


(defun sinomacs-ctext-dict-eww-position ()
  (interactive)
  (search-forward "Chinese Text Project " nil t)
  (forward-line 2)
  (beginning-of-line)
  (set-window-start (selected-window) (point))
 )    

(defun sinomacs-isbn-to-bibtex-lead (isbn)
  "Search lead.to for ISBN bibtex entry.
You have to copy the entry if it is on the page to your bibtex
file."
  (interactive "sISBN: ")
  (eww-browse-url
   (format
    "http://lead.to/amazon/en/?key=%s+&si=all&op=bt&bn=&so=sa&ht=jp"
    isbn))
  (switch-to-buffer "*eww*"))
  
(defun sinomacs-eww-isbn-helper ()
  (interactive)
  (search-forward "@")
  (set-mark-command)
  (search-forward "---")
  (beginning-of-line)
  )
		 

(provide 'sinomacs-ctext)
;; sinomacs-ctext.el ends here
