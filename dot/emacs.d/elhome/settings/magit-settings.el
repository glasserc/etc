;;; magit
; Weirdness on OS X -- PATH doesn't get set or something when running emacs
(if (file-exists-p "/usr/local/git/bin/git")
    (setq magit-git-executable "/usr/local/git/bin/git"))
;;; end magit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-rewrite-inclusive nil))
