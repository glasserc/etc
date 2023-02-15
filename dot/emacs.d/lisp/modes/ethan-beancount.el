(setq beancount-emacs-path
      (expand-file-name "~/tmp/software/beancount-mode/emacs"))

(use-package beancount
  :ensure nil
  :load-path (beancount-emacs-path)
  :mode ("\\.beancount\\'" . beancount-mode))

(provide 'ethan-beancount)
