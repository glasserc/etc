;; Doing this early to avoid visual anomalies (like in emacs-starter-kit)
(tool-bar-mode -1)
;(menu-bar-mode -1)  ; This could be useful (should install Lacarte instead)

(setq default-font "Terminus-13")
(if (>= emacs-major-version 23)
    (progn (set-frame-font default-font t)
           (add-to-list 'default-frame-alist (cons 'font default-font))))


(defun emacs-d (filename)
  (concat user-emacs-directory filename))

(add-to-list 'load-path (emacs-d "lisp"))
(add-to-list 'load-path (emacs-d "lisp/ethan-wspace.git/lisp"))
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

(require 'ethan-defuns)
(require 'ethan-misc)
(require 'ethan-esvn)
(require 'ethan-navigation)
(require 'ethan-programming)
(require 'ethan-communication)
(require 'ethan-java)
(require 'ethan-lisp)
(require 'ethan-rst)
