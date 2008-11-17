; esvn-mode: pennyante SVN mode for emacs
; version 0.1.3

(defvar esvn-mode nil)
(make-variable-buffer-local 'esvn-mode)
(defvar esvn-mode-map nil "Bindings for esvn mode.")

(defvar esvn-svn-command "svn"
  "*Command name to use when calling the version control system.")
(make-variable-buffer-local 'esvn-svn-command)

(defgroup esvn-group nil "Group for esvn customization stuff."
  :version "21.4.20" :prefix 'esvn-)

(defcustom esvn-default-commit-message "" 
  "*Default commit message."
  :type '(string) :group 'esvn-group)
(defcustom esvn-default-add-message "" 
  "*Default commit message if adding a file.

If empty, try esvn-default-commit-message."
  :type '(string) :group 'esvn-group)
(defcustom esvn-default-autocommit-message ""
  "*Default commit message if autocommitting (committing without user intervention).

If empty, try esvn-default-commit-message."
  :type '(string) :group 'esvn-group)
(defcustom esvn-default-autocommit-add-message ""
  "*Default commit message if adding a file while autocommitting.

If empty, try esvn-default-add-message, esvn-default-autocommit-message, and
esvn-default-commit-message in that order."
  :type '(string) :group 'esvn-group)

(make-variable-buffer-local 'esvn-default-commit-message)
(make-variable-buffer-local 'esvn-default-autocommit-message)
(make-variable-buffer-local 'esvn-default-add-message)
(make-variable-buffer-local 'esvn-default-autocommit-add-message)
(defvar esvn-buffer-file-status nil
  "Status of file associated with a buffer.

This can be the symbol :new, which means \"not in SVN\"; the symbol
:committed, which means \"in SVN\"; or nil, which means \"unknown\".")
(make-variable-buffer-local 'esvn-buffer-file-status)

(setq esvn-error-types 
      '((esvn-stat-failed . "svn stat failed")
	(esvn-bad-commit-message . "Bad commit message")))

(if (fboundp 'define-error)
    (progn (mapcar (lambda (elem) (define-error (car elem) (cdr elem)))
		esvn-error-types)
	   (defun esvn-error (symbol string)
	     (error symbol string)))
  (progn (mapcar (lambda (elem) (set (car elem) (cdr elem)))
	      esvn-error-types)
	 (defun esvn-error (symbol string)
	   (error "%s: %s" (eval symbol) string))))

(defun esvn-new-file ()
  "Function to mark a new file as \"new\", for later adding."
  (setq esvn-buffer-file-status :new))

(add-hook 'find-file-not-found-hooks 'esvn-new-file)

(when (not esvn-mode-map)
  (setq esvn-mode-map (make-sparse-keymap))
  ;(define-key esvn-mode-map [?\C-c ?\C-d] 'esvn-diff)
  (define-key esvn-mode-map [?\C-c ?\C-c] 'esvn-commit))

(or (assq 'esvn-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(esvn-mode " E") minor-mode-alist)))

(or (assq 'esvn-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (push (cons 'esvn-mode esvn-mode-map) minor-mode-map-alist)))

(defun esvn-mode (&optional arg)
  (interactive "P")
  (setq esvn-mode
	(if (null arg) (not esvn-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun esvn-default-message (adding autocommit)
  "Get the default message to commit this file."
  (let* ((choices (cons esvn-default-commit-message '()))
	 (choices (if autocommit (cons esvn-default-autocommit-message choices)
		    choices))
	 (choices (if adding (cons esvn-default-add-message choices) choices))
	 (choices (if (and adding autocommit) 
		      (cons esvn-default-autocommit-add-message choices) 
		    choices)))
    (find "" choices :test-not 'string=)))

(defun esvn-get-commit-message (adding autocommit)
  "Get a commit message from the user.

If adding a file for the first time, use esvn-default-add-message
as the default. If not adding, or if that variable is empty,
use esvn-default-commit-message."
  (let* ((defmsg (esvn-default-message adding autocommit))
	 (prompt
	  (if (not (string= defmsg ""))
	      (format "Commit message (default %s):" defmsg)
	    "Commit message:"))
	 (response (read-from-minibuffer (concat prompt " "))))
    (if (string= response "")
	defmsg
      response)))

(defun esvn-execvp (&rest args)
  "Simulate C's execvp() function.

Quote each argument seperately, join with spaces and call shell-command-to-string to run in a shell."
  (let ((cmd (mapconcat 'shell-quote-argument args " ")))
    (shell-command-to-string cmd)))

(defun esvn-get-buffer-file-status ()
  "Find out whether the file corresponding to the buffer is in SVN.

On success, returns :new or :committed."
  (let* ((svnout (esvn-execvp esvn-svn-command "stat" buffer-file-name))
	 (fields (split-string svnout)))
    (when (< (length fields) 1) ; no stat output -- file not there?
      (esvn-error 'esvn-stat-failed "file not there"))

    (let ((status (car fields))
	  (file (car (cdr fields))))

      (cond ((string= status "?") :new)
	    ((string= status "A") :committed) ; no need to add
	    ((string= status "M") :committed)))))

(defun last-line (string)
  (car (last (split-string string "\n" t))))

(defun esvn-commit (autocommit)
  "Commit the local buffer.

If autocommit is true, automatically commit using the default message (either 
esvn-default-add-message or esvn-default-commit-message)."
  (interactive "P")
  (save-buffer)
  (when (not esvn-buffer-file-status)
    (setq esvn-buffer-file-status (esvn-get-buffer-file-status)))
  (let ((adding (eq esvn-buffer-file-status :new)))
    (when adding
      (message (esvn-execvp esvn-svn-command "add" buffer-file-name)))
    (let ((response (if autocommit (esvn-default-message adding autocommit)
		      (esvn-get-commit-message adding autocommit))))
      (when (string= response "")
	(esvn-error 'esvn-bad-commit-message "No message given"))
      (let ((output (esvn-execvp esvn-svn-command "commit" "-m" response buffer-file-name)))
	(if (= (length output) 0)
	    (message "Committed %s" buffer-file-name)
	  (message (last-line output))))
      (setq esvn-buffer-file-status :committed))))

(defun esvn-save-or-autocommit (save)
  "Save or autocommit, based on prefix arg. Prefix arg means save; otherwise, autocommit."
  (interactive "P")
  (if save (save-buffer) (esvn-commit t)))

(provide 'esvn)
