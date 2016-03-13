(use-package elpy
  :config
  (csetq elpy-rpc-backend "jedi")
  (elpy-enable))

;; pymacs/ropemacs. I seem to recall this being a pain in the neck to
;; set up so I hardly ever use it any more.
(use-package pymacs :ensure nil
  :config
  (setq python-ropemacs-setup nil)
  (add-hook 'python-mode-hook 'ropemacs-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (if (not python-ropemacs-setup)
                  (progn
                    (pymacs-load "ropemacs" "rope-")
                    (setq python-ropemacs-setup t)
                    (define-key ropemacs-local-keymap
                      (kbd "M-/") (key-binding (kbd "M-/")))
                    )))))

(provide 'ethan-python)
