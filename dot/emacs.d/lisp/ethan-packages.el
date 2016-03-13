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
  (csetq rainbow-html-colors-major-mode-list
         '(html-mode css-mode php-mode nxml-mode xml-mode less-css-mode))
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
  :pin org
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)))

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package haskell-mode
  :config
  (csetq haskell-compile-cabal-build-command "cd %s && cabal build --ghc-option=-ferror-spans && hlint src/")
  (csetq haskell-indent-spaces 4)
  (csetq haskell-indentation-cycle-warn nil)
  (csetq haskell-indentation-ifte-offset 4)
  (csetq haskell-indentation-layout-offset 4)
  (csetq haskell-indentation-left-offset 4)
  (csetq haskell-indentation-starter-offset 4)
  (csetq haskell-indentation-where-post-offset 2)
  (csetq haskell-indentation-where-pre-offset 2)
  (csetq haskell-mode-hook
         '(imenu-add-menubar-index
           turn-on-eldoc-mode
           turn-on-haskell-decl-scan
           turn-on-haskell-doc
           turn-on-haskell-indentation
           subword-mode))
;; ghc-mod isn't quite ready for prime time, it seems. Plus all its
;; bindings shadow existing keybindings..
;; (let
;;     ((ghc-insert-key "\e\C-t")
;;      (ghc-toggle-key "\C-c\ec")
;;      (ghc-sort-key "\C-c\es")
;;      (ghc-type-key "\C-c\et")
;;      (ghc-info-key "\C-c\ei"))
;;   (ghc-init))
  :bind
  (:map haskell-mode-map
   ("C-," . haskell-move-nested-left)
   ("C-." . haskell-move-nested-right)
   ("C-c C-c" . haskell-compile)
   ("C-c C-z" . haskell-interactive-switch)
   ("C-c C-l" . haskell-process-load-file)
   ("C-c C-b" . haskell-interactive-switch)
   ("C-c C-t" . haskell-process-do-type)
   ("C-c C-i" . haskell-process-do-info)
   ("M-." . haskell-mode-jump-to-def)
   :map haskell-cabal-mode-map
   ("C-c C-c" . haskell-compile)))

(use-package oddmuse   ;; oddmuse is the wiki engine powering EmacsWiki
)

(use-package whole-line-or-region
  :config
  (whole-line-or-region-mode))

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

(use-package jinja2-mode)
(use-package ruby-mode)
(use-package yaml-mode :mode "\\.ya?ml\\'")
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
     (intern (concat (symbol-name x) "-mode-hook")) 'paredit-mode)))
(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-c C->" . mc/unmark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/unmark-previous-like-this)
   ("C-%" . mc/mark-all-dwim)))

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
(use-package magit
  ;; So good!
  :bind
  (("C-x g" . magit-status)))
(use-package git-timemachine)
(use-package elhome
  :config (elhome-init))

(use-package swiper
  ;; Use swiper searches by default.
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)
   ("C-x f" . ivy-recentf)  ;; although see ivy-use-virtual-buffers
   ;; This is almost always a typo.
   ("C-x C-b" . ivy-switch-buffer))
  :config
  (csetq ivy-extra-directories nil)
  (ivy-mode))
(use-package counsel
  :config
  ;; Make counsel behave a little bit more like ido.
  ;;
  ;; RET should enter directories without ending the find-file.  C-j
  ;; can be preserved to open a directory in dired, in case that's
  ;; necessary.
  (counsel-mode t)
  (define-key counsel-find-file-map (kbd "C-j") 'ivy-done)
  (define-key counsel-find-file-map (kbd "RET") 'ivy-alt-done))

;; Avy, which replaces ace-jump-mode
(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ([remap goto-line] . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ([remap other-window] . ace-window))
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
