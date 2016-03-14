;; Doing this early to avoid visual anomalies (like in emacs-starter-kit)
(tool-bar-mode -1)
;(menu-bar-mode -1)  ; This could be useful (should install Lacarte instead)
;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)


(setq default-font "Terminus-13")
(if (>= emacs-major-version 23)
    (progn (set-frame-font default-font t)
           (add-to-list 'default-frame-alist (cons 'font default-font))))


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

(require 'ethan-appearance)
(require 'ethan-misc)
(require 'ethan-esvn)
(require 'ethan-navigation)
(require 'ethan-programming)
(require 'ethan-writing)
(require 'ethan-communication)
(require 'ethan-bindings)
(require 'ethan-enabled-commands)   ;; FIXME: merge with bindings?
