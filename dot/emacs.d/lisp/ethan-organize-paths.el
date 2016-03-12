;; Organize all the paths that emacs packages use to crap up .emacs.d
;; I should probably be able to make this introspect or something
(let ((tmp "~/.emacs.d/cache/"))
  (setq
   oddmuse-directory (concat tmp "oddmuse")
   save-place-file (concat tmp "places")
   tramp-persistency-file-name (concat tmp "tramp")
   ido-save-directory-list-file (concat tmp "ido.last")
   bookmark-default-file (concat tmp "emacs.bmk")
   recentf-save-file (concat tmp "recentf")
   auto-save-list-file-prefix (concat tmp "auto-save-list/saves-")
   url-cookie-file (concat tmp "url/cookies")
   pcache-directory (concat tmp "pcache")
   ;; Don't clutter up directories with files~
   backup-directory-alist `(("." . ,(concat tmp "backups")))
   tramp-backup-directory-alist backup-directory-alist))

;; overrride the default function....
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

;; Don't clutter up directories with #files#
(let ((auto-save-directory (emacs-d "autosaves/")))
  (unless (file-directory-p auto-save-directory)
    (make-directory auto-save-directory))
  (add-to-list 'auto-save-file-name-transforms
               `(".*" ,(expand-file-name auto-save-directory) t)
               'append))

(provide 'ethan-organize-paths)
