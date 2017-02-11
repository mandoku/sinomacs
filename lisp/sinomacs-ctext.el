;;; sinomacs-ctext.el --- Some functions to interact with ctext.org


(defun sinomacs-ctext-dict-region (&optional beg end)
  (interactive "r")
  (if (region-active-p)
      (sinomacs-ctext-dict (buffer-substring beg end))
    (sinomacs-ctext-dict (format "%c" (following-char)))
    (copy-to-register "0" beg end)
    ))

(defun sinomacs-ctext-dict  (search)
  (eww-browse-url (concat "http://ctext.org/dictionary.pl?if=en&char=" search))
  (switch-to-buffer "*eww*"))


(defun sinomacs-ctext-pre-qin-search  (search)
  (eww-browse-url (concat "http://ctext.org/pre-qin-and-han?searchu=" search))
  (switch-to-buffer "*eww*"))

(defun sinomacs-ctext-post-han-search  (search)
  (eww-browse-url (concat "http://ctext.org/post-han?searchu=" search))
  (switch-to-buffer "*eww*"))



(eval-after-load "eww"
  '(progn (define-key eww-mode-map "c" 'sinomacs-ctext-dict-eww-position)
	  (define-key eww-mode-map "i" 'sinomacs-eww-isbn-helper)
	   (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))

(defun sinomacs-ctext-dict-eww-advice (orig-fun &rest args)
  (when (string-match "ctext.org" eww-current-url)
    (with-current-buffer (get-buffer "*eww*")
      (search-forward "Chinese Text Project " nil t)
      (forward-line 2)
      (beginning-of-line)
      (set-window-start (selected-window) (point)))))
(eval-after-load "eww"
  (advice-add 'eww-display-html :after  #'sinomacs-ctext-dict-eww-advice))

(defun sinomacs-ctext-dict-eww-position ()
  (interactive)
  (search-forward "Chinese Text Project " nil t)
  (forward-line 2)
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

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

;; other dictionaries
(defun sinomacs-chise-find ()
  (interactive)
  (eww (concat "http://www.chise.org/ids-find?components="
               (read-string "Search in CHISE IDS find for: "(char-to-string (char-after))))))



(provide 'sinomacs-ctext)
;; sinomacs-ctext.el ends here
