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

(use-package rainbow-mode
  :ensure t
  :config
  (defun turn-on-rainbow-mode ()
    (rainbow-mode 1))
  (add-hook 'prog-mode-hook 'turn-on-rainbow-mode)
  ;; FIXME: why does css-mode not inherit from prog-mode???
  (add-hook 'css-mode-hook 'turn-on-rainbow-mode))

(use-package idle-highlight-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package sml-modeline
  :ensure t
  :config
  (sml-modeline-mode))

(use-package org
  :ensure t
  :pin org)

(use-package haml-mode
  :ensure t)

(use-package haskell-mode :ensure t)

(use-package oddmuse   ;; oddmuse is the wiki engine powering EmacsWiki
  :ensure t)

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-mode))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package jinja2-mode :ensure t)
(use-package ruby-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package less-css-mode :ensure t)
(use-package inf-ruby :ensure t)
(use-package ruby-electric :ensure t)
(use-package paredit :ensure t)
(use-package multiple-cursors :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

(use-package java-snippets
  :ensure t
  :config (java-snippets-initialize))
(use-package javadoc-lookup :ensure t)
(use-package find-file-in-project :ensure t)

;; Hide some modeline lighters.
(use-package delight
  :ensure t
  :config
  (delight
    '((emacs-lisp-mode "EL" :major)
      (js-mode "JS" :major))))

(use-package scratch :ensure t)
(use-package gist :ensure t)
(use-package moz :ensure t)
(use-package wgrep :ensure t)
(use-package magit :ensure t)
(use-package git-timemachine :ensure t)
(use-package elhome
  :ensure t
  :config (elhome-init))

(use-package swiper :ensure t)
(use-package counsel
  :ensure t
  :config
  ;; Make counsel behave a little bit more like ido.
  ;;
  ;; RET should enter directories without ending the find-file.  C-j
  ;; can be preserved to open a directory in dired, in case that's
  ;; necessary.
  (define-key counsel-find-file-map (kbd "C-j") 'ivy-done)
  (define-key counsel-find-file-map (kbd "RET") 'ivy-alt-done))
(use-package avy :ensure t)
(use-package ace-window :ensure t)

;; By default, use-package "loads" or "requires" themes, which causes
;; them to take effect immediately. We don't want that; we just want
;; them to be available for use with load-theme.
(use-package calmer-forest-theme :ensure t :defer t)
(use-package afternoon-theme :ensure t :defer t)
(use-package underwater-theme :ensure t :defer t)
(use-package lush-theme :ensure t :defer t)
(use-package warm-night-theme :ensure t :defer t)
(use-package dark-krystal-theme :ensure t :defer t)

(load-theme 'lush t)

;; CEDET/semantic/malabar stuff
;(require 'semantic)
;(require 'cedet)
;(load "semantic/loaddefs.el")
;(semantic-mode 1)

(use-package rich-minority
  :ensure t
  :config
  (rich-minority-mode 1))

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
;(semantic-mode 1)
;(require 'malabar-mode)
;(add-to-list 'auto-mode-alist '("\\.java" . malabar-mode))

;; Doesn't cause problems
;; (setq elpy-default-minor-modes (remove 'eldoc-etheteh-mode
;;                                        (remove 'auto-complete-mode
;;                                                (remove 'flymake-mode
;;                                                        (remove 'highlight-indentation-mode
;;                                                                elpy-default-minor-modes)))))


;; Causes problems
;; (setq elpy-default-minor-modes (remove 'eldoc-etheteh-mode
;;                                        (remove 'auto-complete-mode-thethethes
;;                                                (remove 'flymake-mode-etsheszhtes
;;                                                        (remove 'highlight-indentation-mode
;;                                                                elpy-default-minor-modes)))))

;; Doesn't cause problems
;; (setq elpy-default-minor-modes (remove 'eldoc-etheteh-mode
;;                                        (remove 'auto-complete-mode-thethethes
;;                                                (remove 'flymake-mode
;;                                                        (remove 'highlight-indentation-mode
;;                                                                elpy-default-minor-modes)))))
;; (setq elpy-default-minor-modes (remove 'flymake-mode elpy-default-minor-modes))

(provide 'ethan-packages)
