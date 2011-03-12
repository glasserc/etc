;; Organize all the paths that emacs packages use to crap up .emacs.d
;; I should probably be able to make this introspect or something
(let ((tmp "~/.emacs.d/cache/"))
  (custom-set-variables
   (list 'oddmuse-directory (concat tmp "oddmuse"))
   (list 'save-place-file (concat tmp "places"))
   (list 'tramp-persistency-file-name (concat tmp "tramp"))
   (list 'ido-save-directory-list-file (concat tmp "ido.last"))
   (list 'bookmark-default-file (concat tmp "emacs.bmk"))
   (list 'recentf-save-file (concat tmp "recentf"))
   (list 'auto-save-list-file-prefix (concat tmp "auto-save-list/saves-")))
  (setq *cheat-directory* (concat tmp "cheat")
        *cheat-sheets-cache-file* (concat tmp "cheat/sheets")))

;; overrride the default function....
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (emacs-d "backups")))))
