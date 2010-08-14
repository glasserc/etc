; Handy function when I just did a git reset or something that touched
; a bunch of files at once
(defun revert-all ()
  "Revert all unchanged buffers."
  (interactive)
  (let ((buffers (buffer-list)))
    (filter (lambda (buffer)
              (if (not (buffer-modified-p buffer))
                  (progn
                    (condition-case revert-error
                        (save-excursion
                          (save-window-excursion
                            (switch-to-buffer buffer)
                            (revert-buffer t t t)))
                      (error (message "Reverting %s failed: %s" buffer revert-error))))
                )) buffers)
    ))

;;; travelogue
(defun find-blog-entry-for-now (base)
  (let* ((target (concat base (format-time-string "%Y/%m/%d/%T.rst")))
        (directory (file-name-directory target)))
    (make-directory directory t)
    (find-file target)))

(setq travelogue-location (expand-file-name "~/src/travelogue/posts/"))
(defun travelogue-now ()
  (interactive)
  (find-blog-entry-for-now travelogue-location))

(setq cameroon-location (expand-file-name "~/src/cameroon/posts/"))
(defun cameroon-now ()
  (interactive)
  (find-blog-entry-for-now cameroon-location))

(setq journal-location (expand-file-name "~/writing/journal/"))
(defun journal-today ()
  (interactive)
  (let* ((now (current-time))
         (decoded (decode-time now))
         (hr (caddr decoded))
         (appropriate-time (if (< hr 5) ; not yet 5 AM; it's still yesterday
                               (progn
                                 (setf (cadddr decoded) (- (cadddr decoded) 1))
                                 decoded)
                             decoded))
         (filename (concat journal-location
                           (format-time-string "%Y-%m-%d"
                                               (apply 'encode-time decoded)))))
    (find-file filename)))

;; Useful editing keybindings:
;; count-words (M-=, replaces default count-lines-region)
(defun count-words (&optional begin end)
  "Runs wc on region (if active) or otherwise the whole buffer."
  (interactive
   (list
    (if (and transient-mark-mode mark-active)
        (region-beginning)
      (point-min))
    (if (and transient-mark-mode mark-active)
        (region-end)
      (point-max))))
  (shell-command-on-region begin end "wc"))

;; scroll-up-one, M-down, maybe I should get rid of this, but I got used
;; to it when I was using XEmacs
(defun scroll-up-one (arg)
  (interactive "p")
  (scroll-up 1))

;; scroll-down-one, ditto
(defun scroll-down-one (arg)
  (interactive "p")
  (scroll-down 1))

;; used in text-mode-hook
(defun turn-on-visual-line-mode ()
  (visual-line-mode 1))

(provide 'ethan-defuns)
