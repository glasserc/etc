;; Doing this early to avoid visual anomalies (like in emacs-starter-kit)

;; This is done in ethan-packages, but package.el likes to add this here
;;(package-initialize)

(tool-bar-mode -1)
;(menu-bar-mode -1)  ; This could be useful (should install Lacarte instead)
;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)


;;(setq default-font "Terminus-13")  ;; seems to look better on darker bgs
(setq default-font "DejaVuSansMono 11")
(if (>= emacs-major-version 23)
    (progn (set-frame-font default-font t)
           (add-to-list 'default-frame-alist (cons 'font default-font))))
;; Seems emacs does some initialization when it starts based on the
;; current screen scaling parameters. If you then move to a different
;; scale, you may need to mess with these parameters to get to
;; something readable.
;;(set-face-attribute 'default (selected-frame) :height 70);
;;(set-frame-parameter nil 'scroll-bar-width 16)
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

;; Try to relocate eln-cache in .emacs.d/var instead of .emacs.
;; Taken from README of https://github.com/emacscollective/no-littering
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(setq autoload-file (emacs-d "loaddefs.el"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Work around built-in org-mode so we can load from ELPA.
;; First, remove the built-in org directory from the load-path.
;; Thanks to
;; http://stackoverflow.com/questions/20603578/emacs-does-not-see-new-installation-of-org-mode/20616703#20616703.
;; Without this, use-package will try to require org and succeed.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
;; Second, trick emacs into forgetting about the fact that org is
;; a "built-in" package by removing it from package--builtins.
;; Without this, package will refuse to install org, since it's
;; "already installed".
;; package--builtins is only initialized when a query needs it.
(package-built-in-p 'org)   ;; prime package--builtins
(setq package--builtins (assq-delete-all 'org package--builtins))
(setq package--builtin-versions (assq-delete-all 'org package--builtin-versions))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package no-littering
  :config
  (no-littering-theme-backups)
  ;; Don't clutter up directories with #files#
  (let ((lock-file-directory (no-littering-expand-var-file-name "lock-files/")))
    (unless (file-directory-p lock-file-directory)
      (make-directory lock-file-directory))
    (setq lock-file-name-transforms
          `((".*" ,lock-file-directory t)))))

(use-package diminish)

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
