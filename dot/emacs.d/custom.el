(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources
   '("secrets:Login" "~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
 '(aw-scope 'frame)
 '(bookmark-save-flag 0)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(css-indent-offset 2)
 '(display-buffer-reuse-frames t)
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eshell-cmpl-autolist t)
 '(eshell-cmpl-cycle-completions nil)
 '(imenu-auto-rescan-maxout 150000)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initsplit-pretty-print t)
 '(mail-envelope-from 'header)
 '(mail-host-address "betacantrips.com")
 '(mail-specify-envelope-from t)
 '(mode-require-final-newline nil)
 '(safe-local-variable-values
   '((lsp-haskell-server-path . "static-ls")
     (haskell-compiler-type . cabal)
     (haskell-indentation-layout-offset . 2)
     (haskell-indentation-ifte-offset . 2)
     (haskell-indentation-starter-offset . 2)
     (haskell-indentation-left-offset . 2)
     (org-time-clocksum-format :hours "%d" :require-hours t :minutes
                               ":%02d" :require-minutes t)
     (require-final-newline) (nxml-child-indent . 4)
     (haskell-compile-cabal-build-command . "make build")
     (haskell-compile-cabal-build-command
      . "env GHC_OPTIONS=-fno-code make build")))
 '(select-enable-clipboard t)
 '(send-mail-function 'sendmail-send-it)
 '(sendmail-program "/usr/bin/msmtp")
 '(sql-postgres-login-params
   '((user :default "ethan")
     (database :default "ethan")
     port server))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style 'reverse nil (uniquify))
 '(uniquify-separator "/")
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(hl-line ((t (:background "gainsboro"))))
 '(org-scheduled ((t (:foreground "dark blue"))))
 '(org-scheduled-today ((t (:foreground "blue")))))
