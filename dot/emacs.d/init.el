(setq emacs-d (expand-file-name "~/.emacs.d/"))

(defun emacs-d (filename)
  (concat emacs-d filename))

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
(load "~/.emacs.d/ethan-defuns.el")
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

;;; customize explicit sets

(require 'cl)
(defun remove-all (needles haystack)
  (remove* needles haystack :test '(lambda (needles elt)
                                     (member elt needles))))

(let ((vcs-extensions '(".svn/" ".hg/" ".git/" ".bzr/")))
  ;; don't ignore project.git
  (setq completion-ignored-extensions
        (remove-all vcs-extensions completion-ignored-extensions))
  (setq ido-ignore-files
        (append
         ;; But do ignore files that are just .git, .hg, .svn, etc.
         ;; generate regexes that are ^.git, etc.
         (mapcar '(lambda (arg) (concat "^" arg)) vcs-extensions)
         ido-ignore-files)))
;;;

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

;; I should probably be able to make this introspect or something
(let ((tmp "~/.emacs.d/cache/"))
  (custom-set-variables
   (list 'oddmuse-directory (concat tmp "oddmuse"))
   (list 'save-place-file (concat tmp "places"))
   (list 'tramp-persistency-file-name (concat tmp "tramp"))
   (list 'ido-save-directory-list-file (concat tmp "ido.last"))
   (list 'bookmark-default-file (concat tmp "emacs.bmk"))
   (list 'recentf-save-file (concat tmp "recentf"))))

(require 'yasnippet)
(yas/initialize)
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
(setq org-directory "~/src/org-files")
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-clock-into-drawer t)
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

; I keep someday.org files, can't include those. Maybe a better
; approach is needed?
(setq all-org-files (directory-files org-directory t ".org$" t))
(setq org-agenda-files (filter
                        (lambda (filename)
                          (not (or (string-match "someday.org$" filename)
                                   (string-match "feeds.org$" filename))))
                        all-org-files))

(setq org-todo-keywords '((sequence "TODO(t)" "BLOCKING" "WORKING" "|" "DELEGATED(D)" "DONE(d)" "WONTFIX(W)")))
(setq org-use-fast-todo-selection t)

;; Still messing around with this.
(load (concat org-directory "/feeds-list.el"))

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
(setq org-tag-alist '((:startgroup)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      (:endgroup)
                      ("SOMEDAY" . ?s)  ; Not sure about this
                      ))

; yasnippet config from org-mode mailing list
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
         (lambda ()
           ;; yasnippet (using the new org-cycle hooks)
           (make-variable-buffer-local 'yas/trigger-key)
           (setq yas/trigger-key [tab])
           (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
           (define-key yas/keymap [tab] 'yas/next-field)))

; M-/ is my dabbrev-command -- should bind it to org-complete, and org-completion-fallback-command
;;; org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; RST export for orgtbl
(defun orgtbl-to-rst-line (line)
  (apply 'format (cons *org-rst-lfmt* line)))

(defun orgtbl-to-rst (table params)
  "Convert the Orgtbl mode TABLE to ReStructuredText."
  (let* ((hline (concat
                 "+-"
                 (mapconcat (lambda (width) (apply 'string (make-list width ?-)))
                            org-table-last-column-widths "-+-")
                 "-+"))
         (*org-rst-lfmt* (concat
                          "| "
                          (mapconcat (lambda (width) (format "%%-%ss" width))
                                     org-table-last-column-widths " | ")
                          " |"))
         (params2
          (list
           :tstart hline
           :hline hline
           :lfmt 'orgtbl-to-rst-line
           )))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

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
