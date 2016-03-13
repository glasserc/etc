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
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package idle-highlight-mode
  :config
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package sml-modeline
  :config
  (sml-modeline-mode))

(use-package persistent-scratch
  :config
  (csetq persistent-scratch-save-file (emacs-d "scratch.el"))
  (persistent-scratch-setup-default))

(use-package org
  :pin org
  :config
  ;; Visual-line mode wraps tags because they're followed by "...".
  ;; This hack leaves one extra space after the tags, so that they don't
  ;; wrap.
  ;; P.S. This is totally a hack.
  (setq org-tags-column -76)

  ;; Make windmove work in org-mode: (from org-mode manual)
  ;; I'm not sure this is the way I want to fix this, since I hardly
  ;; ever use priorities on tasks, so maybe I just want to always use
  ;; windmove unconditionally?
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (run-at-time "00:59" 3600 'org-save-all-org-buffers)


  ;; Org-Babel: embed code into your org files and then execute.
  ;; I use this just enough that I may as well require it.
  (require 'ob-python)
  ;; Fix weirdness with org-mode and yasnippet (both use TAB)
  ;; yasnippet config from org-mode mailing list
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (defun org-and-yasnippet-compatibility ()
    ;; yasnippet (using the new org-cycle hooks)
    (make-variable-buffer-local 'yas/trigger-key)
    (setq yas/trigger-key [tab])
    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
    (define-key yas/keymap [tab] 'yas/next-field))

  (add-hook 'org-mode-hook
            'org-and-yasnippet-compatibility)

  ;; RST export for orgtbl
  (defun orgtbl-to-rst-line (line)
    (apply 'format (cons *org-rst-lfmt* line)))

  (defun orgtbl-to-rst (table params)
    "Convert the Orgtbl mode TABLE to ReStructuredText."
    (let* ((hline (concat
                   "+-"
                   (mapconcat (lambda (width) (apply 'string (make-list width ?-)))
                              org-table-last-column-widths "-+-")
                   "-+"))
           (*org-rst-lfmt* (concat
                            "| "
                            (mapconcat (lambda (width) (format "%%-%ss" width))
                                       org-table-last-column-widths " | ")
                            " |"))
           (params2
            (list
             :tstart hline
             :hline hline
             :lfmt 'orgtbl-to-rst-line
             )))
      (orgtbl-to-generic table (org-combine-plists params2 params))))

  ;; Compute all org files.
  ;; These represent our refile targets.
  (setq org-directory "~/src/org-files")  ; not in custom because we use
                                        ; its value
  (setq org-all-org-files (directory-files org-directory t ".org$" t))
  ;; Agenda files are a specific subset of these:
  ;; - todo.org: generic shit-to-do
  ;; - incoming.org: shit I saw and wanted to deal with later
  ;; - writing.org: things to write about
  ;; - contacts.org: notes about people, including reminders to interact
  ;; with them.
  ;; - coding.org: things to hack on, mostly non-urgent

  ;; Also present are:
  ;; - music.org: albums to listen to, etc.
  ;; - house.org: clocked hours spent around the house
  ;; - someday.org: mulch pile for stuff I'd like to mess around with
  ;; someday
  ;; - purchases.org: similar, but things I'd like to buy
  ;;
  ;; The unifying theme here is that these files represent non-urgent
  ;; things -- stuff I can pull up or let mulch at will.  Agenda files
  ;; are "what I need to work on".

  ;; Not in customize: computed automatically.
  ;; Note that this probably isn't used, since my capture templates
  ;; specify files.
  (setq org-default-notes-file (concat org-directory "/incoming.org"))
  (csetq org-agenda-files
         '("~/src/org-files/coding.org"
           "~/src/org-files/contacts.org"
           "/home/ethan/src/org-files/writing.org"
           "/home/ethan/src/org-files/incoming.org"
           "/home/ethan/src/org-files/todo.org"))
  (csetq org-agenda-restore-windows-after-quit t)
  (csetq org-archive-mark-done nil)
  (csetq org-capture-templates
         '(("t" "todo" entry
            (file+headline "~/src/org-files/incoming.org" "New")
            "* TODO %?
%u
%(and )")
           ("b" "Book to purchase" entry
            (file+headline "~/src/org-files/purchases.org" "Books")
            "* TODO %?
%x")))
  (csetq org-clock-history-length 10)
  (csetq org-clock-into-drawer t)
  (csetq org-completion-use-ido t)
  (csetq org-drawers
         '("PROPERTIES" "CLOCK" "DETAILS"))
  (csetq org-footnote-section nil)
  (csetq org-goto-interface 'outline-path-completion)
  (csetq org-log-done 'time)
  (csetq org-outline-path-complete-in-steps nil)
  (csetq org-refile-targets '((org-all-org-files :level . 1)))
  (csetq org-tag-alist
         '((:startgroup)
           ("@work" . 119)
           ("@home" . 104)
           (:endgroup)
           ("internet" . 105)
           ("SOMEDAY" . 115)))
  (csetq org-todo-keywords
         '((sequence "TODO(t)" "BLOCKING" "WORKING" "|" "DELEGATED(D)" "DONE(d)" "WONTFIX(W)")))
;; customize?
;;org-enforce-todo-dependencies
;;org-track-ordered-property-with-tag
;;org-agenda-dim-blocked-tasks   ; Except, I want "BLOCKING" tasks to match too
;;org-log-into-drawer
;; org-tag-alist  -- I'm not sure about "SOMEDAY"
;; org-outline-path-complete-in-steps

;; M-<up> and M-<down> are shadowed by my personal scroll up/down
;; function.  (M-S-<up> and M-S-<down> behave almost exactly the same
;; way)
; M-/ is my dabbrev-command -- should bind it to org-complete, and org-completion-fallback-command

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

(use-package jinja2-mode)
(use-package ruby-mode)
(use-package yaml-mode :mode "\\.ya?ml\\'")
(use-package less-css-mode)
(use-package inf-ruby)
(use-package ruby-electric)

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
;; I don't use CEDET, but these were settings I found useful
;(require 'semantic)
;(require 'cedet)
;(load "semantic/loaddefs.el")
;(semantic-mode 1)
;(global-ede-mode t)
; (semantic-load-enable-excessive-code-helpers)
; (require 'semantic-ia)
; (setq semanticdb-default-save-directory (emacs-d "cache/semanticdb"))
;; Some kind of weird bug with exuberent-ctags in python-mode?
;;(semantic-load-enable-primary-exuberent-ctags-support)
;;(require 'semantic-gcc)

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
