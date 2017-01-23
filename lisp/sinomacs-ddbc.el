;; query the ddbc authority files

(defvar sinomacs-ddbc-authority-url "http://authority.ddbc.edu.tw/webwidget/getAuthorityData.php?id=" )
(defvar sinomacs-ddbc-end-of-headers "\n\n")
(defvar sinomacs-ddbc-record "~/org/places.org")
(defvar sinomacs-ddbc-googlemaps-url "https://maps.google.com/maps?&hl=en&z=6")

(defun sinomacs-ddbc-authority-search-person (beg end)
  (interactive "r")
   (sinomacs-ddbc-authority-search-internal "person" (buffer-substring-no-properties beg end)))

(defun sinomacs-ddbc-authority-search-place (beg end)
  (interactive "r")
   (sinomacs-ddbc-authority-search-internal "place" (buffer-substring-no-properties beg end)))

(defun sinomacs-ddbc-authority-search-internal (type search)
  (sinomacs-ddbc-authority-display-result
   search
   (sinomacs-ddbc-authority-search type search)))


(defun sinomacs-ddbc-authority-display-result (search res)
  (let ((result-buffer (get-buffer-create "*Query Result*")))
    (set-buffer result-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "* " search "\n")
    (dolist (data res)
      (insert "** ")
      ;; lets first get the name of this entry as a 2nd level heading
      (insert (sinomacs-ddbc-result-heading type (cdr data)))
      (dolist (ddbc (cdr data))
	(insert (sinomacs-ddbc-result-value "*** " ddbc))))
    (sinomacs-ddbc-mode)
    (org-overview)
    (hide-sublevels 2)
    (switch-to-buffer-other-window result-buffer t)))


(defun sinomacs-ddbc-authority-search (type search)
"Search the authority database at DDBC for names of persons or places.
type is the type of search, e.g. either 'person' or 'place'
search is the search string, the name of the person or place or its ID code in the DDBC authority.
Returns a a-list with the parsed results."
(with-current-buffer
    (url-retrieve-synchronously (concat sinomacs-ddbc-authority-url search "&type=" type))
  (goto-char (point-min))
  (search-forward sinomacs-ddbc-end-of-headers)
  (json-read)))


(defun sinomacs-ddbc-result-heading (type data)
  (concat
   (cdr (assoc 'name  (cdr data)))
   (if (equal type "person")
      (concat  " " (substring (format "%s" (cdr (assoc 'bornDateBegin  (cdr data)))) 1 6)
      (substring (format "%s" (cdr (assoc 'diedDateBegin  (cdr data)))) 1 5)))
   " ("
   (cdr (assoc 'dynasty  (cdr data)))
   ")\n:PROPERTIES:\n:CUSTOM_ID: "
   (cdr (assoc 'authorityID  (cdr data)))
   (if (cdr (assoc 'lat data))
       (concat "\n:lat: " (cdr (assoc 'lat (cdr data)))))
   (if (cdr (assoc 'long data))
       (concat "\n:long: " (cdr (assoc 'long (cdr data)))))
   "\n:END:\n"  ))

(defun sinomacs-ddbc-result-value (level data)
  (let ((key (car data))
	(value (cdr data)))
    (if (> (length value) 0)
	(concat level (prin1-to-string key) " " (prin1-to-string value) "\n")
      "")))


(defun sinomacs-ddbc-goto-latlong (pom)
  (interactive "d")
  (sinomacs-ddbc-to-google-map (org-entry-get pom "lat") (org-entry-get pom "long")))
(defun sinomacs-ddbc-to-google-map (lat long)
  (browse-url (concat sinomacs-ddbc-googlemaps-url "&q=" lat "," long)))
(defun sinomacs-ddbc-save-subtree ()
  (interactive)
  (org-copy-subtree)
  (with-current-buffer (find-file-noselect sinomacs-ddbc-record nil nil)
    (goto-char (point-max))
    (yank)
    (save-buffer)
    (message (format "Copied to file %s and saved" sinomacs-ddbc-record))))

(define-derived-mode sinomacs-ddbc-mode org-mode "sinomacs-ddbc-mode"
  "a mode to view dictionary files
  \\{sinomacs-ddbc-mode-map}"
  (setq case-fold-search nil)
  (set (make-local-variable 'org-startup-folded) 'overview)
  (read-only-mode)
;  (view-mode)
)

(define-key sinomacs-ddbc-mode-map
  "s" 'sinomacs-ddbc-save-subtree)
(define-key sinomacs-ddbc-mode-map
  "v" 'sinomacs-ddbc-goto-latlong)
