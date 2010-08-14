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

;(iswitchb-mode 1) ;; subsumed by ido-mode
(require 'ido)
(ido-mode 1)

(tool-bar-mode -1)
;(menu-bar-mode -1)  ; This could be useful (should install Lacarte instead)

;;; redo
(require 'redo)
(define-key global-map (kbd "M-_") 'redo)
;;; end redo

;;; color theme
(require 'color-theme)
(setq color-theme-is-global t)
(if (functionp 'color-theme-initialize)
    (color-theme-initialize))
(color-theme-charcoal-black)
;;; end color theme

;;; Emacs 23 font hacking
(setq default-emacs-font "Terminus-13")

(if (>= emacs-major-version 23)
    (progn (set-frame-font default-emacs-font t)
           (add-to-list 'default-frame-alist (cons 'font default-emacs-font))))
;;; end Emacs 23 font hacking

;;; wspace -- both displaying, and editing
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;;; rst face customizations
; FIXME: ugly colors
(require 'rst)

(set-face-background 'rst-level-1-face "#565656")
(set-face-background 'rst-level-2-face "#4d4d4d")
(set-face-background 'rst-level-3-face "#464646")
(set-face-background 'rst-level-4-face "#3d3d3d")
(set-face-background 'rst-level-5-face "#363636")

(setq auto-mode-alist
      (cons '("\\.rst$" . rst-mode)
            auto-mode-alist))

;;; elide-head
(require 'elide-head)

(setq elide-head-headers-to-hide
      (append
       '(("Copyright (C) 2008 10gen Inc\\." . "If not, see <http")   ; AGPL
         ("Copyright (C) 2008 10gen Inc\\." . "under the License\\.") ; APL
         )
       elide-head-headers-to-hide))

(add-hook 'find-file-hook 'elide-head)
;;; end elide-head

;;; desktop-mode config
; I don't expect to ever use it, but it's nice to
; have.  Save session with desktop-save, then read using desktop-read.
; desktop-save doesn't overwrite it's previous save.. not sure why or
; how to fix. Maybe desktop-save-mode?
(set-default 'desktop-path (list (expand-file-name "~/.emacs.d/")))
;;; end desktop-mode

;;; generic editing keybindings
(require 'ethan-defuns)
(require 'ethan-misc)
(defun count-words (&optional begin end)
  "Runs wc on region (if active) or otherwise the whole buffer."
  (interactive
   (list
    (if (and transient-mark-mode mark-active)
        (region-beginning)
      (point-min))
    (if (and transient-mark-mode mark-active)
        (region-end)
      (point-max))))
  (shell-command-on-region begin end "wc"))

(defun scroll-up-one (arg)
  (interactive "p")
  (scroll-up 1))

(defun scroll-down-one (arg)
  (interactive "p")
  (scroll-down 1))

; At present, emacs does not define bindings for these keys
(define-key global-map (kbd "M-<up>") 'scroll-down-one)
(define-key global-map (kbd "M-<down>") 'scroll-up-one)
;(define-key global-map (kbd "M-g") 'goto-line)
(define-key esc-map "=" 'count-words)

; In theory these are reserved for major modes, but I like the python-mode
; bindings, so I'm making them global.
(global-set-key [?\C-c ?>] 'increase-left-margin)
(global-set-key [?\C-c ?<] 'decrease-left-margin)

;; Revert automatically. This might cause a lot of network traffic
;; when used with tramp?
(global-auto-revert-mode 1)

;;; end generic editing keybindings

;;; java-mode-indent-annotations
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)
;;; end java-mode-indent-annotations

;;; customize stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;; customize stuff

;;; other programming language modes
(require 'css-mode)
(setq auto-mode-alist
      (cons '("\\.css$" . css-mode)
            auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.js$" . js2-mode)
            auto-mode-alist))

;; text mode
(defun turn-on-visual-line-mode ()
  (visual-line-mode 1))

(add-to-list 'text-mode-hook 'turn-on-visual-line-mode)
;;; end programming language modes
;;; esvn-mode
(require 'esvn)

(define-derived-mode journal-mode text-mode "Journal"
  "Major mode for editing Ethan's diary."
  (setq esvn-default-commit-message "Entry.")
  (setq esvn-default-autocommit-message "autocommit")
  (setq esvn-default-add-message "New day.")
  (esvn-mode)
  (local-set-key [?\C-x ?\C-s] 'esvn-save-or-autocommit))

(defun enable-esvn ()
  (message (buffer-file-name))
  (when (string-match "writing/" (buffer-file-name))
    (setq esvn-default-commit-message "Edit.")
    (setq esvn-default-add-message "Begin.")
    (setq require-final-newline t)
    (setq esvn-svn-command "hg")
    (esvn-mode t)
    (when (string-match "journal/" (buffer-file-name))
      (local-set-key [?\C-x ?\C-s] 'esvn-save-or-autocommit))))

;(add-hook 'find-file-not-found-hooks 'enable-esvn)
(add-hook 'find-file-hooks 'enable-esvn)

(setq auto-mode-alist
      (append
       '(
         ("writing/journal/"     . journal-mode)
         ("writing/"             . text-mode)
         ) auto-mode-alist))
;;; esvn-mode

(setq auto-mode-alist
      (append
       '(
         ("\\.mdwn$"             . mdwn-mode)
         ) auto-mode-alist))

;;; yasnippet

(setq yasnippet-directory
      (car
       (last
        ; FIXME: not sorted, since I don't want to parse version numbers
        (directory-files
         (expand-file-name "~/.emacs.d/packages/")
         t "^yasnippet-[0-9]+\\.[0-9]+\.[0-9]+"))))
(add-to-list 'load-path yasnippet-directory)

(require 'yasnippet)
(yas/initialize)
;; This takes a long time
(yas/load-directory (concat yasnippet-directory "/snippets"))
(yas/load-directory (emacs-d "my-snippets"))

;;; haskell-mode
(add-hook 'haskell-mode-hook (lambda () (require 'inf-haskell)))

;;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;;; abbrev
(setq abbrev-file-name (emacs-d "abbrev_defs.el"))
(read-abbrev-file abbrev-file-name t)
(setq abbrev-mode t)   ; not really sure about this... RST has some abbrevs too

;;; pymacs, ropemacs
(if (require 'pymacs nil t)
    (progn
      (setq python-ropemacs-setup nil)
      (add-hook 'python-mode-hook 'ropemacs-mode)
      (add-hook 'python-mode-hook
                (lambda ()
                  (if (not python-ropemacs-setup)
                      (progn
                        (pymacs-load "ropemacs" "rope-")
                        (setq python-ropemacs-setup t)
                        (define-key ropemacs-local-keymap
                          (kbd "M-/") (key-binding (kbd "M-/")))
                        )))))
  (progn ; else
    (message "WARNING: pymacs missing -- ropemacs absent")))

;;; dvc
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/dvc-2009-02-05"))
;(require 'dvc-autoloads)

(require 'ethan-org)
(require 'ethan-php)
(require 'ethan-haml)
