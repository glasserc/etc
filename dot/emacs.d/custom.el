(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(display-buffer-reuse-frames t)
 '(ido-max-directory-size 36864)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-agenda-restore-windows-after-quit t)
 '(org-archive-mark-done nil)
 '(org-capture-templates (quote (("t" "todo" entry (file+headline "~/src/org-files/incoming.org" "* New") "* TODO %?
%u
%a"))))
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "DETAILS")))
 '(org-refile-targets (quote ((org-agenda-files :level . 1))))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(uniquify-separator "/")
 '(visible-bell t)
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red")))))
