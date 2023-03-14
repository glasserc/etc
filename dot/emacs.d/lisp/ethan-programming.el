(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
;; not the mode -- see minor-mode-alist
(eval-after-load "simple" '(diminish 'auto-fill-function))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

;; Cosmetic
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'local-comment-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'pretty-lambdas)
(add-hook 'prog-mode-hook 'add-watchwords)

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (csetq rainbow-html-colors-major-mode-list
         '(html-mode css-mode php-mode nxml-mode xml-mode less-css-mode))
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package idle-highlight-mode
  :config
  (add-hook 'prog-mode-hook 'idle-highlight-mode)
  (eval-after-load "hi-lock" '(diminish 'hi-lock-mode)))

(defun run-prog-mode-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'prog-mode-hook))

;; FIXME: take this out if CSS ever starts being a programming language
(add-hook 'css-mode-hook 'run-prog-mode-hook)

(use-package saveplace
  :config
  (defun turn-on-save-place-mode ()
    (setq save-place t))
  (add-hook 'prog-mode-hook 'turn-on-save-place-mode))

;; So good!
(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

;; On a new machine, you'll probably want to set up the token using
;; something like:
;; (secrets-create-item "Login" "Github API key for forge" "some-token-from-github" :host "api.github.com" :user "glasserc^forge")
;; See
;; https://magit.vc/manual/ghub/Creating-a-Token.html#Creating-a-Token
;; for some more information.
;; Afterwards, you can just edit the entry using the password manager.
;; You can see what is retrieved using e.g.:
;; (auth-source-search :host "api.github.com" :user "glasserc^forge")
;;
;; Different versions of forge, magithub, etc. have used different
;; logic to look up the appropriate secret. In particular, they have
;; changed the `:user` value to be `glasserc^magithub`,
;; `glasserc^ghub`, `glasserc^forge`, etc.
;;
;; These commands have been helpful for diagnosing and debugging it.
;;
;; (secrets-search-items "Login" :host "api.github.com")
;; (secrets-get-secret "Login" "Github API key for Magithub")
;; (secrets-get-attributes "Login" "Github API key for Magithub")
;; (secrets-create-item "Login" "Github API key for Forge"
;;   (secrets-get-secret "Login" "Github API key for Magithub")
;;     :host "api.github.com" :user "glasserc^forge")

(use-package forge
  :after magit
  :ensure t)

(use-package git-timemachine)
(use-package ruby-mode
  :custom
  (ruby-align-chained-calls t "Conform to Teachable style")
  (ruby-insert-encoding-magic-comment nil "This should be fixed in 28.1"))
(use-package inf-ruby)
(use-package ruby-electric)
(use-package less-css-mode)
(use-package haml-mode
  :mode "\\.haml\\'")
(use-package wilt
  :commands wilt-mode
  :init
  (add-hook 'prog-mode-hook 'wilt-mode))
(use-package fish-mode
  :mode "\\.fish\\'"
  :config
  (defun indent-four-spaces ()
    (setq tab-width 4))
  (add-hook 'fish-mode-hook
            'indent-four-spaces))
(use-package envrc)

(use-package sql-indent)
(use-package sql
  :config
  ;; These are taken from master and presumably will show up in emacs
  ;; 29. The defaults in emacs 28 don't support DBs with dashes in
  ;; the names.
  ;; I also think the prompt-regexp shouldn't have the dash in it so
  ;; it understands continuations correctly, but that's just a hunch,
  ;; haven't tested exhaustively.
  (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  (sql-set-product-feature 'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-'(][#>] "))

;; lorem-ipsum. Although it isn't the only time, mostly I use this
;; when programming.
(use-package lorem-ipsum
  :commands lorem-ipsum-insert-paragraphs
  :config
  (defalias 'lorem 'lorem-ipsum-insert-paragraphs))

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (unless paredit-mode
    (set (make-local-variable 'paredit-space-delimiter-chars)
         (list ?\"))
    (paredit-mode 1)))

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
;(semantic-mode 1)
;(require 'malabar-mode)
;(add-to-list 'auto-mode-alist '("\\.java" . malabar-mode))
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))

(bind-key "M-/" 'hippie-expand)

(setq diff-switches "-u")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package go-mode
  :ensure t
  :config
  (defun allow-tabs ()
    (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))
  (add-hook 'go-mode-hook #'allow-tabs))

;(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js-mode))

(use-package nix-mode
  :ensure t
  :mode "\\.nix$")

;; Seems like this is the standard in the JS world
(csetq js-indent-level 2)

(use-package elm-mode
  :ensure t)

(require 'ethan-java)
(require 'ethan-lisp)
(require 'ethan-python)
(require 'ethan-haskell)
(require 'ethan-beancount)   ;; not really a mode, but whatever

(provide 'ethan-programming)
