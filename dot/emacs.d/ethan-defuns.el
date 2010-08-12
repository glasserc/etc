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

(provide 'ethan-defuns)
