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

;; Ido: don't ignore project.git, but ignore .git itself.
(let ((vcs-extensions '(".svn/" ".hg/" ".git/" ".bzr/")))
  ;; remove .git/ from completion-ignored-extensions, because it matches endings
  (mapc '(lambda (extension)
           (setq completion-ignored-extensions
                 (remove extension completion-ignored-extensions)))
        vcs-extensions)
  (setq ido-ignore-files
        (append
         ;; But do ignore files that are just .git, .hg, .svn, etc.
         ;; generate regexes that are ^.git, etc.
         (mapcar '(lambda (arg) (concat "^" arg)) vcs-extensions)
         ido-ignore-files)))
;;;

;;; magit -- I still use this
; Weirdness on OS X -- PATH doesn't get set or something when running emacs
(if (file-exists-p "/usr/local/git/bin/git")
    (setq magit-git-executable "/usr/local/git/bin/git"))
;;; end magit

(provide 'ethan-misc)
