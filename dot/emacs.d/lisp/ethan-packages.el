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

(use-package rainbow-mode
  :config
  (defun turn-on-rainbow-mode ()
    (rainbow-mode 1))
  (add-hook 'prog-mode-hook 'turn-on-rainbow-mode)
  ;; FIXME: why does css-mode not inherit from prog-mode???
  (add-hook 'css-mode-hook 'turn-on-rainbow-mode))

(use-package idle-highlight-mode
  :config
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package sml-modeline
  :config
  (sml-modeline-mode))

(use-package org
  :pin org)

(use-package haml-mode)

(use-package haskell-mode)

(use-package oddmuse   ;; oddmuse is the wiki engine powering EmacsWiki
)

(use-package whole-line-or-region
  :config
  (whole-line-or-region-mode))

(use-package elpy
  :config
  (elpy-enable))

(use-package jinja2-mode)
(use-package ruby-mode)
(use-package yaml-mode)
(use-package less-css-mode)
(use-package inf-ruby)
(use-package ruby-electric)
(use-package paredit
  :config
  (dolist (x '(scheme emacs-lisp lisp clojure))
  (when window-system
    (font-lock-add-keywords
     (intern (concat (symbol-name x) "-mode"))
     '(("(\\|)" . 'esk-paren-face))))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit)))
(use-package multiple-cursors)

(use-package yasnippet
  :config (yas-global-mode))

(use-package find-file-in-project :ensure t)

;; Hide some modeline lighters.
(use-package delight
  :config
  (delight
    '((emacs-lisp-mode "EL" :major)
      (js-mode "JS" :major))))

(use-package scratch)
(use-package gist)
(use-package moz)
(use-package wgrep)
(use-package magit)
(use-package git-timemachine)
(use-package elhome
  :config (elhome-init))

(use-package swiper)
(use-package counsel
  :config
  ;; Make counsel behave a little bit more like ido.
  ;;
  ;; RET should enter directories without ending the find-file.  C-j
  ;; can be preserved to open a directory in dired, in case that's
  ;; necessary.
  (define-key counsel-find-file-map (kbd "C-j") 'ivy-done)
  (define-key counsel-find-file-map (kbd "RET") 'ivy-alt-done))
(use-package avy
  :config
  (csetq avy-background t)
  (csetq avy-keys '(97 111 101 117 105 100 104 116 115)))
(use-package ace-window)

;; By default, use-package "loads" or "requires" themes, which causes
;; them to take effect immediately. We don't want that; we just want
;; them to be available for use with load-theme.
(use-package calmer-forest-theme :defer t)
(use-package afternoon-theme :defer t)
(use-package underwater-theme :defer t)
(use-package lush-theme :defer t)
(use-package warm-night-theme :defer t)
(use-package dark-krystal-theme :defer t)

(load-theme 'lush t)

;; CEDET/semantic/malabar stuff
;(require 'semantic)
;(require 'cedet)
;(load "semantic/loaddefs.el")
;(semantic-mode 1)

(use-package rich-minority
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
