(setq beancount-emacs-path
      (expand-file-name "~/tmp/software/beancount/editors/emacs"))

(use-package beancount
  :ensure nil
  :load-path (beancount-emacs-path)
  :mode ("\\.beancount\\'" . beancount-mode))

(provide 'ethan-beancount)
