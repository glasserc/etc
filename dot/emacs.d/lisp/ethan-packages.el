(require 'diminish)
(require 'bind-key)

(eval-when-compile
  (require 'ethan-custom))

;; Hide some modeline lighters.
(use-package delight
  :config
  (delight
    '((emacs-lisp-mode "EL" :major)
      (js-mode "JS" :major))))

(use-package ethan-wspace
  :load-path "lisp/ethan-wspace.git/lisp"
  :config (global-ethan-wspace-mode 1))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package persistent-scratch
  :config
  (csetq persistent-scratch-save-file (emacs-d "scratch.el"))
  (persistent-scratch-setup-default))

(use-package oddmuse   ;; oddmuse is the wiki engine powering EmacsWiki
  :config
  ;; Get around the emacswiki spam protection
  (add-hook 'oddmuse-mode-hook
            (lambda ()
              (unless (string-match "question" oddmuse-post)
                (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post))))))

(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode))

(use-package jinja2-mode)
(use-package yaml-mode :mode "\\.ya?ml\\'")

(use-package multiple-cursors
  :config
  (csetq mc/list-file (emacs-d "mc-lists.el"))
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-c C->" . mc/unmark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/unmark-previous-like-this)
   ("C-%" . mc/mark-all-dwim)))

(use-package yasnippet
  :diminish yas-global-mode
  :config
  (csetq yas/wrap-around-region t)
  (csetq yas/prompt-functions '(yas/x-prompt yas/ido-prompt))

  (yas-global-mode)
  ;; This takes a long time
  ;;(yas/load-directory (concat yasnippet-directory "/snippets"))
  )

(use-package find-file-in-project)

(use-package scratch)
(use-package gist)
(use-package moz)
(use-package wgrep)

(use-package beancount
  :load-path "/home/ethan/tmp/software/beancount/editors/emacs"
  :mode "\\.beancount$")

(provide 'ethan-packages)
