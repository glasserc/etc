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
(bind-key "M-=" 'count-words)

;; Used in org-mode snippets week-rec and week-comm
(defun begin-of-week ()
  "Return the date corresponding to the Monday of the current week as a string."
  (let ((monday-time (apply 'day-of-week-helper 1 (decode-time (current-time)))))
    (format-time-string "%Y-%m-%d" monday-time)))

(defun end-of-week ()
  "Return the date corresponding to the Friday of the current week as a string."
  (let ((friday-time (apply 'day-of-week-helper 5 (decode-time (current-time)))))
    (format-time-string "%Y-%m-%d" friday-time)))

(defun day-of-week-helper (desired-dow seconds minutes hour day month year dow dst zone)
  "Return a time corresponding to the given DOW in the given decoded time.

I guess this is how you do list unpacking in emacs-lisp."
  (encode-time seconds minutes hour (+ desired-dow (- day dow)) month year zone))

;; Some modes
(setq auto-mode-alist
      (append
       '(
         ("/writing/journal/"     . journal-mode)  ;; defined in ethan-esvn
         ("/writing/"             . text-mode)
         ("\\.mdwn$"             . mdwn-mode)
         ) auto-mode-alist))

;; text mode
(add-to-list 'text-mode-hook 'visual-line-mode)
(eval-after-load "simple" '(diminish 'visual-line-mode))
(add-to-list 'text-mode-hook 'turn-on-flyspell)
(eval-after-load "flyspell" '(diminish 'flyspell-mode))


(require 'ethan-org)
(require 'ethan-rst)

(provide 'ethan-writing)
