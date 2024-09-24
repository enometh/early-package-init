;;  -*- lexical-binding: t; -*-
;;
;; if this file is loaded at some point during it will start tracking
;; various events in an ad-hoc manner in the *track-events* buffer
;; using the (now obsoleted in emacs 29) advice.el facility.

;; this file claims the "track*-events-" namespace and also stomps on
;; "verbose-COMMAND-advice" namespace for the name when using
;; defadvice, for various commands that it tracks.

;; note this file has to be fully compiled and the and compiled file
;; has to be loaded if it has to be loaded via early-init.el. this is
;; necessary to avoid various subsequent eager macro oexpansion
;; failures. calling `byte-recompile-file' and `load' (without
;; specifying an extension) on this file in early-init.el should be
;; sufficient

(require 'cl-macs)

(when nil(cl-defmacro tracking-events (&body body) `(progn ,@body)))

(defvar track-events-latest-first nil
  "If non-NIL latest events appear at the top of the *track-events* buffer.")

(when nil(setq tracking-events-latest-first t))

(defun track-events-depth (symbol)
  (or (get symbol 'track-events-depth) 0))

(gv-define-setter track-events-depth (store symbol)
  `(setf (get ,symbol 'track-events-depth) ,store))

(defun track-events-pad (symbol)
  (make-string (1- (track-events-depth symbol)) 32))

(cl-defmacro tracking-events (&body body)
  `(let ((messages-buffer-name (buffer-name
				(get-buffer-create "*track events*")))
	 (message-log-max t))
     (cl-letf (((symbol-function #'message)
		(lambda (format-string &rest args)
		  (with-current-buffer messages-buffer-name
		    (goto-char (if track-events-latest-first
				   (point-min)
				 (point-max)))
		    (insert (apply #'format format-string args) "\n")))))
       ,@body)))


(when nil
(tracking-events  (message "barf"))
(loop for i in '(load load-hack require) collect (track-events-depth i))
(loop for i in '(load load-hack require) do (setf (track-events-depth i) 0))
  )


;;(ad-unadvise 'kill-buffer)
(defadvice kill-buffer (before
 			verbose-kill-buffer-advice
 			(&optional buffer-or-name)
 			activate)
  (let* ((buf (or buffer-or-name (current-buffer)))
	 (name (cl-etypecase buf
		 (string buf)
		 (buffer (or (buffer-name buf)
			     (format "%s" buf))))))
    (when (not (string-match "^\\( \\*temp\\| \\*wallpaper\\| \\*mm\\| \\*load\\)" name))
      (tracking-events
       (message "killing buffer %s" name)))))

;madhu 240615
;;(ad-unadvise 'kill-buffer)

(defadvice load (around
		 verbose-load-advice
		 (file &optional noerror nomessage nosuffix must-suffice)
		 activate)
  (cl-incf (track-events-depth 'load))
  (tracking-events
   (message "%sload %s" (track-events-pad 'load) file))
  (unwind-protect ad-do-it
    (cl-decf (track-events-depth 'load))))

;;(ad-unadvise 'delete-file)
(defadvice delete-file (before
 			verbose-delete-file-advice
 			(filename &optional trash)
 			activate)
  (tracking-events
   (message "deleting file %s" filename)))

;;(ad-unadvise 'rename-file)
(defadvice rename-file (before
 			verbose-rename-file-advice
 			(file newname &optional ok-if-already-exists)
 			activate)
  (tracking-events
   (message "renaming file %S" (list file newname ok-if-already-exists))))

;; (ad-unadvise 'load-hack)
(defadvice load-hack (around
		      verbose-load-hack-advice
		      (&optional pkg)
		      activate)
  (cl-incf (track-events-depth 'load-hack))
  (let ((arg-available-at-start-p (not (null pkg))))
    (tracking-events
     (message "%sload-hack %s" (track-events-pad 'load-hack)
	      (if arg-available-at-start-p pkg "(unavailable)")))
    (unwind-protect (progn ad-do-it)
      (cl-decf (track-events-depth 'load-hack)))))

;; (ad-unadvise 'desktop-create-buffer)
(defadvice desktop-create-buffer (before
				  verbose-desktop-create-buffer-advice
				  (file-version buffer-filename &rest rest)
				  activate)
  (tracking-events
   (message "desktop-create-buffer %s" buffer-filename)))

(defvar track-events-require-stack nil
  "set to t to disable some fancy heuristics ")

;; (ad-unadvise 'require)
(defadvice require (around
		    verbose-require-advice
		    (feature &optional filename noerror)
		    activate)
  (cl-incf (track-events-depth 'require))
  (unless (atom track-events-require-stack)
    (push feature track-events-require-stack))
  (let ((present-p (featurep feature)))
    (tracking-events
     (message "%srequire %s%s" (track-events-pad 'require)
	      feature (if present-p  " (provided)" "")))
    (unwind-protect ad-do-it
       (cl-decf (track-events-depth 'require))
       (unless (atom track-events-require-stack)
	 (unless (eql (car track-events-require-stack) ad-return-value)
	   (tracking-events
	    (message "require %s failed" feature)))
	 (pop track-events-require-stack)))))
