;; (source: http://emacs.stackexchange.com/a/22174/93)
(defun sinomacs-package-update (package &optional version)
  "Update a package from the package archives.
If VERSION is nil, update the package to the most recent version
available.  Otherwise, VERSION should be a version string, or a
list of the type returned by `version-to-list'. The package will
be updated only if the currently installed version is less than
the version specified, even if a newer version is available."
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents))
  (let* ((current (cadr (assoc package package-alist)))
         (current-version (if current (package-desc-version current) '(-1)))
         (pkg-desc (cadr (assoc package package-archive-contents)))
         (pkg-version (and pkg-desc (package-desc-version pkg-desc)))
         (target-version (or (and (stringp version) (version-to-list version))
                             version
                             pkg-version)))
    (when (version-list-< current-version target-version)
      (if (null pkg-desc)
        (error "Package `%s' not found in package archives" package))
      (if (version-list-< pkg-version target-version)
        (error "A suitable version of `%s' is not available" package))
      (package-install pkg-desc)
      (if current
          (package-delete current)))
    nil))
