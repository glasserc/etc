(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(xterm-mouse-mode t)

;; I should probably be able to make this introspect or something
(let ((tmp "~/.emacs.d/cache/"))
  (custom-set-variables
   (list 'oddmuse-directory (concat tmp "oddmuse"))
   (list 'save-place-file (concat tmp "places"))
   (list 'tramp-persistency-file-name (concat tmp "tramp"))
   (list 'ido-save-directory-list-file (concat tmp "ido.last"))
   (list 'bookmark-default-file (concat tmp "emacs.bmk"))
   (list 'recentf-save-file (concat tmp "recentf"))))

(provide 'ethan-misc)
