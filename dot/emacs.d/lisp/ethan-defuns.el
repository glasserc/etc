;; These are taken from the Emacs Starter Kit
(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;; I don't like this exactly, but I want to write something like this
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (unless paredit-mode
    (set (make-local-variable 'paredit-space-delimiter-chars)
         (list ?\"))
    (paredit-mode 1)))

(defun toggle-fullscreen ()
  (interactive)
  ;; TODO: this only works for X. patches welcome for other OSes.
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;;; My own defuns
;; Used in org-mode snippets week-rec and week-comm
(defun begin-of-week ()
  "Return the date corresponding to the Monday of the current week as a string."
  (let ((monday-time (apply 'day-of-week-helper 1 (decode-time (current-time)))))
    (format-time-string "%Y-%m-%d" monday-time)))

(defun day-of-week-helper (desired-dow seconds minutes hour day month year dow dst zone)
  "Return a time corresponding to the given DOW in the given decoded time.

I guess this is how you do list unpacking in emacs-lisp."
  (encode-time seconds minutes hour (+ desired-dow (- day dow)) month year zone))

(defun end-of-week ()
  "Return the date corresponding to the Friday of the current week as a string."
  (let ((friday-time (apply 'day-of-week-helper 5 (decode-time (current-time)))))
    (format-time-string "%Y-%m-%d" friday-time)))

(provide 'ethan-defuns)
