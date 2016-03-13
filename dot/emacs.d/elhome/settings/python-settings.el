;;; pymacs, ropemacs
(if (require 'pymacs nil t)
    (progn
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
  (progn ; else
    (message "WARNING: pymacs missing -- ropemacs absent")))
