;; Organize all the paths that emacs packages use to crap up .emacs.d.
;; FIXME: Move all of these to no-littering.
(let ((tmp "~/.emacs.d/cache/"))
  (setq
   url-cookie-file (concat tmp "url/cookies")
   srecode-map-save-file (emacs-d "cache/srecode-map.el")))

(provide 'ethan-organize-paths)
