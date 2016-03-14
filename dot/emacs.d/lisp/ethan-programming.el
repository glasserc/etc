(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

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

(use-package magit
  ;; So good!
  :bind
  (("C-x g" . magit-status)))
(use-package git-timemachine)
(use-package ruby-mode)
(use-package inf-ruby)
(use-package ruby-electric)
(use-package less-css-mode)
(use-package haml-mode
  :mode "\\.haml\\'")


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


(require 'ethan-java)
(require 'ethan-lisp)
(require 'ethan-python)

(provide 'ethan-programming)
