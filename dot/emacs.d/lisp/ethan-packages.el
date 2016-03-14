(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; I think GNU ELPA has an `org` package, but this one may be more up
;; to date?
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Work around built-in org-mode so we can load from ELPA.
;; First, remove the built-in org directory from the load-path.
;; Thanks to
;; http://stackoverflow.com/questions/20603578/emacs-does-not-see-new-installation-of-org-mode/20616703#20616703.
;; Without this, use-package will try to require org and succeed.
(eval-when-compile
  (require 'cl))
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
;; Second, trick emacs into forgetting about the fact that org is
;; a "built-in" package by removing it from package--builtins.
;; Without this, package will refuse to install org, since it's
;; "already installed".
;; package--builtins is only initialized when a query needs it.
(package-built-in-p 'org)   ;; prime package--builtins
(setq package--builtins (assq-delete-all 'org package--builtins))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

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

;; (use-package rich-minority
;;   :config
;;   (rich-minority-mode 1))

(provide 'ethan-packages)
