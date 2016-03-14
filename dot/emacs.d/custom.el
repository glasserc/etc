(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 0)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(css-indent-level 2)
 '(css-indent-offset 2)
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(display-buffer-reuse-frames t)
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function
   (quote split-window-horizontally))
 '(ediff-window-setup-function
   (quote ediff-setup-windows-plain))
 '(eshell-cmpl-autolist t)
 '(eshell-cmpl-cycle-completions nil)
 '(ido-max-directory-size 36864)
 '(imenu-auto-rescan-maxout 150000)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initsplit-pretty-print t)
 '(mail-envelope-from
   (quote header))
 '(mail-host-address "betacantrips.com")
 '(mail-specify-envelope-from t)
 '(mode-require-final-newline nil)
 '(mouse-yank-at-point t)
 '(rm-blacklist
;; Some of these can be defined specially -- see for example
;; flyspell-mode-line-string and undo-tree-mode-lighter -- but keep
;; them all here for consistency.
   (quote
    (" hl-p" " Rbow" " yas" " Undo-Tree" " WLR" " Paredit" " Fill" " Isearch" " me" " ElDoc" " Fly" " Wrap" " Ind" " Doc" " MRev")))
 '(safe-local-variable-values
   (quote
    ((org-time-clocksum-format :hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
     (require-final-newline)
     (nxml-child-indent . 4))))
 '(send-mail-function
   (quote sendmail-send-it))
 '(sendmail-program "/usr/bin/msmtp")
 '(sql-postgres-login-params
   (quote
    ((user :default "ethan")
     (database :default "ethan")
     port server)))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style
   (quote reverse)
   nil
   (uniquify))
 '(uniquify-separator "/")
 '(virtualenv-root "~/Jobs/Pave/core/.local_venv/")
 '(visible-bell t)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added
   ((t
     (:inherit diff-changed :foreground "green"))))
 '(diff-removed
   ((t
     (:inherit diff-changed :foreground "red"))))
 '(hl-line
   ((t
     (:background "#444444")))))
