;; Doing this early to avoid visual anomalies (like in emacs-starter-kit)
(tool-bar-mode -1)
;(menu-bar-mode -1)  ; This could be useful (should install Lacarte instead)

;;; Emacs 23 font hacking
(setq default-emacs-font "Terminus-13")

(if (>= emacs-major-version 23)
    (progn (set-frame-font default-emacs-font t)
           (add-to-list 'default-frame-alist (cons 'font default-emacs-font))))
;;; end Emacs 23 font hacking


(setq emacs-d (expand-file-name "~/.emacs.d/"))

(defun emacs-d (filename)
  (concat emacs-d filename))

(add-to-list 'load-path emacs-d)
(add-to-list 'load-path (emacs-d "src"))  ; my elisp hacks
; FIXME: should I have a separate repo for emacs-lisp stuff?
; For example, ~/src/emacs-lisp (which I do have on sundance). Not sure.

(add-to-list 'load-path (emacs-d "contrib/org-mode.git/lisp"))
(add-to-list 'load-path (emacs-d "/elpa-to-submit"))
;; Everything in contrib, maybe?
(add-to-list 'load-path (emacs-d "contrib/rainbow.git"))
(add-to-list 'load-path (emacs-d "contrib/cedet-1.0"))
(add-to-list 'load-path (emacs-d "contrib/offlineimap-el.git"))
(add-to-list 'load-path (emacs-d "contrib/ethan-wspace.git/lisp"))
(add-to-list 'load-path (emacs-d "contrib/magit.git"))
(add-to-list 'load-path (expand-file-name  "~/local/share/emacs/site-lisp"))

(setq autoload-file (emacs-d "loaddefs.el"))
(setq package-user-dir (emacs-d "elpa"))
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(load
 (expand-file-name "~/.emacs.d/elpa/package.el"))
(when (featurep 'package)
  (package-initialize))
(require 'ethan-elpa)

(require 'ethan-el-get)
;;; customize stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;; customize stuff

(require 'ethan-defuns)
(require 'ethan-misc)
(require 'ethan-bindings)
(require 'ethan-registers)
;(require 'ethan-cedet)
(require 'ethan-esvn)
(require 'ethan-org)
(require 'ethan-rst)
(require 'ethan-python)
(require 'ethan-php)
(require 'ethan-haml)
(require 'ethan-haskell)
(require 'ethan-js)
(require 'ethan-lisp)
(require 'ethan-notmuch)

(regen-autoloads)
