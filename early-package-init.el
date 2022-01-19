;;; early-package-init.el
;;
;;  This file should be loaded from early-init.el before
;;  `package-initialize' is called by emacs.  It contains a function
;;  which must be called *within* `package-initialize' when
;;  `package-initialize' runs for the first time. See
;;  `early-package-init---load-descriptor-and-activate'
;;
;;  The functions defined in this file operate on a variable
;;  `early-package-init-alist' which must be set elsewhere, again,
;;  before `package-initialize' runs.  This variable sets the paths
;;  and the names of /the packages/ which should be be exposed to
;;  package.el through early-package-init.el.
;;
;;  /The packages/ also have to be specified in
;; `package-pinned-packages' so package.el won't update them.

(require 'package)

(cl-defun early-package-init--generate-files
				    ($pkg-dir
				     &optional
				     $pkg-name-string
				     ($pkg-ver "0")
				     ($pkg-summary "No Summary")
				     ($pkg-requirements nil)
				     &key
				     (single-file-p t)
				     force-autoload-generation
				     force-pkg-file-generation)
  "Helper function.
Generate the autoload and package \"pkg.el\" files for the
package named $PKG-NAME-STRING in directory $PKG-DIR. If
$PKG-NAME is null infer the name from $PKG-DIR.

If SINGLE-FILE-P is nil then the autoloads are generated for all
files in the directory.  But the point of the game is to have
multiple packages in the same directory"
  (unless $pkg-name-string
    (setq $pkg-name-string
	  (string-trim-right (package--description-file $pkg-dir)
			     "-pkg\\.el")))
  (let ($pkg-desc $pkg-autoloads-file $pkg-desc-file)
    (setq $pkg-desc
	  (package-desc-from-define $pkg-name-string $pkg-ver $pkg-summary
				    $pkg-requirements))
    (setf (package-desc-dir $pkg-desc) $pkg-dir)
    (setq $pkg-autoloads-file
	  (expand-file-name (format "%s-autoloads.el" (package-desc-name
						       $pkg-desc))
			    (package-desc-dir $pkg-desc)))
    (when (or force-autoload-generation
	      (not (file-exists-p $pkg-autoloads-file)))
      (if single-file-p
	  (progn
	    (package-autoload-ensure-default-file $pkg-autoloads-file)
	    (update-file-autoloads (expand-file-name
				    (format "%s.el" (package-desc-name
						     $pkg-desc))
				    (package-desc-dir $pkg-desc))
				   t
				   $pkg-autoloads-file))
	(package-generate-autoloads (package-desc-name $pkg-desc)
				    (package-desc-dir $pkg-desc))))
    (setq $pkg-desc-file
	  (expand-file-name (format "%s-pkg.el" (package-desc-name $pkg-desc))
			    (package-desc-dir $pkg-desc)))
    (when (or force-pkg-file-generation
	      (not (file-exists-p $pkg-desc-file)))
      (package-generate-description-file $pkg-desc $pkg-desc-file))
    (cl-values (list $pkg-name-string $pkg-desc-file $pkg-autoloads-file))))


(defvar early-package-init--activate-p nil)

(cl-defun early-package-init---load-descriptor-and-activate
    ($pkg-dir &optional $pkg-name-string &key
	      (activate-p early-package-init--activate-p))
  "Load the package description file in $PKG-DIR for the package
named $PKG-NAME-STRING.  Note that THE $PKG-NAME-STRING can be
different from the basename of $PKG-DIR and multiple packages can
reside in the same $PKG-DIR.

Create a new `package-desc' object, add it to `package-alist',
if `activate-p' is non-NIL activate it - put it in
`package-activated-list' and return it. The package need not be
selected in `package-selected-list'

This function has to be called *in* `package-initialize' *after*
it uncaringly resets `package-alist' and *before* it calls
`package-load-all-descriptors'. So it can only be called in a
before-method of `package-load-all-descriptors.'"
  (let (($pkg-file nil) (signed-file (concat $pkg-dir ".signed")))
    (if (null $pkg-name-string)
	(setq $pkg-file
	      (expand-file-name (package--description-file $pkg-dir)
				$pkg-dir))
      (setq $pkg-file
	    (expand-file-name (format "%s-pkg.el" $pkg-name-string) $pkg-dir)))
    (when (file-exists-p $pkg-file)
      (with-temp-buffer
        (insert-file-contents $pkg-file)
        (goto-char (point-min))
        (let ($pkg-desc)
	  (setq $pkg-desc
		(or (package-process-define-package
		     (read (current-buffer)))
		    (error "Can't find define-package in %s" $pkg-file)))
          (setf (package-desc-dir $pkg-desc) (expand-file-name $pkg-dir))
          (if (file-exists-p signed-file)
              (setf (package-desc-signed $pkg-desc) t))
	  (when $pkg-name-string
	    (cl-assert (equal $pkg-name-string
			      (symbol-name(package-desc-name $pkg-desc)))))
	  (when activate-p
	    (package-activate-1 $pkg-desc))
          $pkg-desc)))))

(defvar early-package-init-alist nil)

(require 'advice)
(defadvice package-load-all-descriptors (before my-package-initialize-local-packages activate)
  (cl-loop for (dir name) in early-package-init-alist
	   do (early-package-init---load-descriptor-and-activate dir name)))


(provide 'early-package-init)

;;; early-package-init.el ends here
