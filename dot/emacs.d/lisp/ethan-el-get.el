(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

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
(use-package elhome
  :ensure t
  :config (elhome-init))

;; It sucks that we can't put these in organize-paths, but I want them
;; to work even in the first call to el-get
(setq bcc-cache-directory (emacs-d "cache/byte-cache"))
(setq url-cookie-file (emacs-d "cache/url/cookies"))

;; CEDET/semantic/malabar stuff
;(require 'semantic)
;(require 'cedet)
;(load "semantic/loaddefs.el")
;(semantic-mode 1)

(setq auto-install-elpa-packages
      '(rich-minority
        calmer-forest-theme
        afternoon-theme
        underwater-theme
        lush-theme
        warm-night-theme
        dark-krystal-theme
        swiper
        counsel
        avy
        ace-window
        git-timemachine))
(dolist (package auto-install-elpa-packages)
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'lush t)
(rich-minority-mode 1)

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

(provide 'ethan-el-get)
