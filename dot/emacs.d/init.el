;; Doing this early to avoid visual anomalies (like in emacs-starter-kit)
(tool-bar-mode -1)
;(menu-bar-mode -1)  ; This could be useful (should install Lacarte instead)
;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)


;;(setq default-font "Terminus-13")  ;; seems to look better on darker bgs
(setq default-font "DejaVuSansMono 11")
(if (>= emacs-major-version 23)
    (progn (set-frame-font default-font t)
           (add-to-list 'default-frame-alist (cons 'font default-font))))
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(xterm-mouse-mode t)


(defun emacs-d (filename)
  (concat user-emacs-directory filename))

(add-to-list 'load-path (emacs-d "lisp"))
(add-to-list 'load-path (emacs-d "lisp/modes"))
(add-to-list 'load-path (expand-file-name  "~/local/share/emacs/site-lisp/"))
(add-to-list 'load-path (expand-file-name  "~/.local/share/emacs/site-lisp/"))

(require 'ethan-organize-paths)

(setq autoload-file (emacs-d "loaddefs.el"))
(require 'ethan-packages)
;;; customize stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;; customize stuff

(setq my-credentials-file (expand-file-name "~/.config/secrets/secrets.el"))

(defun lookup-secret (name-of-secret)
  "Look up a secret from a secrets.el file kept in `my-credentials-file'.

This lets you keep some important values in a different repository."
  ;; See https://github.com/jorgenschaefer/circe/wiki/Configuration
  (with-temp-buffer
    (insert-file-contents-literally my-credentials-file)
    (plist-get (read (buffer-string)) name-of-secret)))

(require 'ethan-appearance)
(require 'ethan-esvn)
(require 'ethan-navigation)
(require 'ethan-programming)
(require 'ethan-writing)
(require 'ethan-communication)
(require 'ethan-bindings)
