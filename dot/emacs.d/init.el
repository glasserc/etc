(defun emacs-d (filename)
  (concat (expand-file-name "~/.emacs.d/") filename))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/src"))  ; my elisp hacks
; FIXME: should I have a separate repo for emacs-lisp stuff?
; For example, ~/src/emacs-lisp (which I do have on sundance). Not sure.

(add-to-list 'load-path (emacs-d "packages/org-mode"))

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
    (progn (set-frame-font default-emacs-font)
           (add-to-list 'default-frame-alist (cons 'font default-emacs-font))))
;;; end Emacs 23 font hacking

;;; wspace -- both displaying, and editing
(require 'ethan-wspace)

;;; rst face customizations
; FIXME: ugly colors
(require 'rst)

(set-face-background 'rst-level-1-face "#565656")
(set-face-background 'rst-level-2-face "#4d4d4d")
(set-face-background 'rst-level-3-face "#464646")
(set-face-background 'rst-level-4-face "#3d3d3d")
(set-face-background 'rst-level-5-face "#363636")

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

; Handy function when I just did a git reset or something that touched
; a bunch of files at once
(defun revert-all ()
  "Revert all unchanged buffers."
  (interactive)
  (let ((buffers (buffer-list)))
    (filter (lambda (buffer)
              (if (not (buffer-modified-p buffer))
                  (progn
                    (condition-case revert-error
                        (save-excursion
                          (save-window-excursion
                            (switch-to-buffer buffer)
                            (revert-buffer t t t)))
                      (error (message "Reverting %s failed: %s" buffer revert-error))))
                )) buffers)
    ))


;;; end generic editing keybindings

;;; java-mode-indent-annotations
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)
;;; end java-mode-indent-annotations

;;; customize stuff
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(display-buffer-reuse-frames t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/src/org-files/ivillage.org")))
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "DETAILS")))
 '(require-final-newline ask)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(uniquify-separator "/")
 '(visible-bell t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red")))))
;;; customize stuff

;;; other programming language modes
(require 'css-mode)
(setq auto-mode-alist
      (cons '("\\.css$" . css-mode)
            auto-mode-alist))

;(setq auto-mode-alist
;      (cons '("\\.jxp$" . jsp-mode)
;            auto-mode-alist))


(setq auto-mode-alist
      (cons '("\\.js$" . js2-mode)
            auto-mode-alist))



(require 'multi-mode)
(defun jsp-mode () (interactive)
  (multi-mode 1
              'html-mode
              '("<%--" indented-text-mode)
              '("{" javascript-mode)
              '("<%@" html-mode)
              '("<%=" html-mode)
              '("<%" java-mode)
              '("%>" html-mode)
              )
  )

;;; end programming language modes

;;; jxp-mode stuff.
;;;
;;; This is a symlink to a file that might get updated and on some
;;; machines doesn't even exist, so we wrap it in a condition-case (in
;;; case it doesn't exist here).
(condition-case jxp-error
    (progn (require 'jxp-mode)
           (require 'mmm-mode)

           (setq mmm-global-mode 'maybe)
           (mmm-add-mode-ext-class 'html-mode "\\.jxp\\'" 'html-jxp)
           (mmm-add-mode-ext-class 'html-mode "\\.jxp\\'" 'html-js)
           (mmm-add-classes
            '((html-jxp
               :submode jxp-mode
               :face mmm-declaration-submode-face
               :front "<%=?"
               :back "%>"
               :include-front t
               :include-back t
               )))
           (mmm-add-classes
            '((html-js
               :submode jxp-mode
               :face mmm-output-submode-face
               :front "<script[^>]*>"
               :back "</script>"
               :include-front t
               :include-back t
               )))
           (autoload 'jxp-mode "jxp-mode" "JXP editing mode" t)
           (add-to-list 'auto-mode-alist '("\\.jxp\\'" . html-mode))
           ;; mmm-mode conveniently displays jxp code as white-on-white unless the highlight level is set to 0
           (setq mmm-submode-decoration-level 0))
  (error (message "Failed to load jxp mode: %s" jxp-error)))
; FIXME: fall back on JSP mode?
;;; end jxp-mode stuff


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
(yas/load-directory (concat yasnippet-directory "/snippets"))
(yas/load-directory (emacs-d "my-snippets"))

;;; haskell-mode
(add-hook 'haskell-mode-hook (lambda () (require 'inf-haskell)))

;;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

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

;;; git -- I never use this any more since git-cola, but hey!
;(require 'git)
;;; end git

;;; dvc
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/dvc-2009-02-05"))
;(require 'dvc-autoloads)

;;; magit -- I still use this
; Weirdness on OS X -- PATH doesn't get set or something when running emacs
(if (file-exists-p "/usr/local/git/bin/git")
    (setq magit-git-executable "/usr/local/git/bin/git"))
;;; end magit

;;; org-mode
;(require 'org-install)
(require 'org)
; FIXME: this doesn't seem necessary on sundance, but does on colt -- why?
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("/TODO$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-todo-keywords '((sequence "TODO(t)" "BLOCKING" "WORKING" "|" "DELEGATED(D)" "DONE(d)" "WONTFIX(W)")))
(setq org-use-fast-todo-selection t)
; customize?
;org-enforce-todo-dependencies
;org-track-ordered-property-with-tag
;org-agenda-dim-blocked-tasks   ; Except, I want "BLOCKING" tasks to match too
;org-log-into-drawer
;org-tag-alist

; Get off my movement keys!
; (M-S-<up> and M-S-<down> behave almost exactly the same way)
(define-key org-mode-map [(meta up)]    nil)
(define-key org-mode-map [(meta down)]  nil)
; I use this for esvn-mode.
; FIXME: org-mode + esvn mode needs work.
; C-c C-c should work outside of esvn mode?
; (Maybe esvn-mode should use a different key?)
; FIXME: taking this out -- all org stuff happens in another repo, not using
; esvn. This is a very commonly used binding in org mode
;(define-key org-mode-map "\C-c\C-c" nil)
(setq org-completion-use-ido t)

; Not sure about this -- just stole it to try to get yasnippet working with
; org mode. Looks like yasnippet falls back to the org keybinding if
; it can't do anything?
; FIXME: maybe not define-key here, but just re-set the keybindings somewhere
; before calling yas/initialize?
(add-hook 'org-mode-hook
          (lambda ()
            (org-set-local 'yas/trigger-key [tab])
            ; FIXME: this wasn't needed in yasnippet-0.5.7,
            ; maybe not needed in emacs22 -- file a bug report?
            (define-key yas/minor-mode-map [tab] 'yas/expand)
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

; M-/ is my dabbrev-command -- should bind it to org-complete, and org-completion-fallback-command
;;; remember
(org-remember-insinuate)
(setq org-directory "~/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)

;;; php-mode
(add-hook 'php-mode-hook (lambda ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  ))

;;; haml-mode
(autoload 'haml-mode "haml-mode" "Mode for editing HAML code." t)
(autoload 'sass-mode "sass-mode" "Mode for editing SASS code." t)

(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

