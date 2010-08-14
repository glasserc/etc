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
(add-to-list 'load-path (emacs-d "packages"))
(add-to-list 'load-path (emacs-d "src"))  ; my elisp hacks
; FIXME: should I have a separate repo for emacs-lisp stuff?
; For example, ~/src/emacs-lisp (which I do have on sundance). Not sure.

(add-to-list 'load-path (emacs-d "packages/org-mode"))
(add-to-list 'load-path (emacs-d "packages/ethan-wspace"))

(if (not (functionp 'debian-run-directories))
    (require 'debian-startup))

(debian-run-directories (emacs-d "startup"))

;;; customize stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;; customize stuff

(require 'ethan-defuns)
(require 'ethan-misc)
(require 'ethan-bindings)
(require 'ethan-esvn)
(require 'ethan-org)
(require 'ethan-python)
(require 'ethan-php)
(require 'ethan-haml)
(require 'ethan-java)
(require 'ethan-haskell)
(require 'ethan-js)
