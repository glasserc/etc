;; Doing this early to avoid visual anomalies (like in emacs-starter-kit)
(tool-bar-mode -1)
;(menu-bar-mode -1)  ; This could be useful (should install Lacarte instead)

;;; Emacs 23 font hacking
(setq default-emacs-font "Terminus-13")

(if (>= emacs-major-version 23)
    (progn (set-frame-font default-emacs-font t)
           (add-to-list 'default-frame-alist (cons 'font default-emacs-font))))
;;; end Emacs 23 font hacking


(defun emacs-d (filename)
  (concat user-emacs-directory filename))

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (expand-file-name  "~/local/share/emacs/site-lisp"))

(setq autoload-file (emacs-d "loaddefs.el"))
(require 'ethan-el-get)
;;; customize stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;; customize stuff

(require 'ethan-defuns)
(require 'ethan-misc)
(require 'ethan-esvn)
(require 'ethan-lisp)

(regen-autoloads)
