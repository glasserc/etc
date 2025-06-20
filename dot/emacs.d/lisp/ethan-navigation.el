;; -*- lexical-binding: t -*-
(require 'thingatpt)

;; Set up registers. Stolen from Emacs-Starter-Kit.

;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.
(dolist (r `((?i (file . ,(emacs-d "init.el")))
             (?b (file . ,(emacs-d "ethan-bindings.el")))
             (?r (file . ,(emacs-d "ethan-registers.el")))
             (?t (file . ,(expand-file-name "~/src/org-files/todo.org")))))
  (set-register (car r) (cadr r)))

;; File finding
(bind-keys
 ("C-x M-f" . ido-find-file-other-window)
 ("C-x C-M-f" . find-file-in-project)
 ("C-c y" . bury-buffer)
 ("C-c r" . revert-buffer)
 ("M-`" . file-cache-minibuffer-complete)

 ;; This mode is very rarely useful.
 ("C-x C-S-b" . ibuffer)

 ;; Window switching. (C-x o goes to the next window)
 ("C-x O" . (lambda () (interactive) (other-window -1))) ;; back one
 ("C-x C-o" . (lambda () (interactive) (other-window 2)))) ;; forward two
(windmove-default-keybindings) ;; Shift+direction

(use-package seq :defer t)
(use-package crux
  ;; crux has a bunch of useful commands, not entirely specific to
  ;; navigation.

  :config
  ;; Once loaded, crux-reopen-as-root is added to find-file-hook, so
  ;; files will be automatically opened as root.  I prefer files only
  ;; open as root when I ask them to be opened as root. This is the
  ;; same reason I don't do "sudo sh"...
  (remove-hook 'find-file-hook 'crux-reopen-as-root)

  :commands
  crux-sudo-edit crux-insert-date
  crux-rename-file-and-buffer
  crux-delete-file-and-buffer
  :bind (("C-x C-h" . crux-view-url)
         ("C-^" . crux-top-join-lines)
         ("C-c e" . crux-eval-and-replace)
         ("M-o" . crux-smart-open-line)))

;; Personal customization
;; scroll-up-one, M-down, maybe I should get rid of this, but I got used
;; to it when I was using XEmacs
;; At present, emacs does not define bindings for these keys,
;; but various modes do: org mode, paredit, etc
(defun scroll-up-one (arg)
  (interactive "p")
  (scroll-up 1))

;; scroll-down-one, ditto
(defun scroll-down-one (arg)
  (interactive "p")
  (scroll-down 1))

(bind-keys*
 ("M-<up>" . scroll-down-one)
 ("M-<down>" . scroll-up-one))

;; disable C-z on X11 sessions
(when window-system
  (global-unset-key "\C-z"))

;; disable C-x C-c because I keep hitting it by accident and I hardly
;; ever close emacs. Normally it's save-buffers-kill-terminal.
(global-unset-key (kbd "C-x C-c"))

(use-package swiper
  ;; Use swiper searches by default.
  :diminish ivy-mode
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("C-r" . counsel-grep-or-swiper)
   ("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c f" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)
   ;; This is almost always a typo.
   ("C-x C-b" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<next>" . ivy-scroll-up-command)
   ("<prior>" . ivy-scroll-down-command))
  :config
  ;; setup recentf so that it adds its hook to find-file.
  ;; This lets ivy-use-virtual-buffers work.
  (csetq recentf-max-saved-items 100)
  (csetq recentf-max-menu-items 15)
  (csetq ivy-use-virtual-buffers t)
  ;; I never remember C-M-j to select what I've typed.
  (csetq ivy-use-selectable-prompt t)
  (recentf-mode 1)

  (csetq ivy-extra-directories nil)
  (ivy-mode))
(use-package counsel :demand t
  :diminish counsel-mode
  :config
  ;; I'm not sure counsel needs this to use counsel-imenu
  (set-default 'imenu-auto-rescan t)
  (counsel-mode t)

  :bind
  (("C-x C-i" . counsel-imenu)
   ;; Make counsel behave a little bit more like ido.
   ;;
   ;; RET should enter directories without ending the find-file.  C-j
   ;; can be preserved to open a directory in dired, in case that's
   ;; necessary.
   :map counsel-find-file-map
   ("C-j" . ivy-done)
   ("RET" . ivy-alt-done)))

;; Avy, which replaces ace-jump-mode
(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ([remap goto-line] . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ([remap other-window] . ace-window))
  :config
  (csetq avy-background t)
  (csetq avy-keys '(97 111 101 117 105 100 104 116 115)))
(use-package ace-window)

(put 'set-goal-column 'disabled nil)

;; Ido: don't ignore project.git, but ignore .git itself.
;; FIXME: figure out a way to do this for counsel/ivy.
(let ((vcs-extensions '(".svn/" ".hg/" ".git/" ".bzr/")))
  ;; remove .git/ from completion-ignored-extensions, because it matches endings
  (mapc #'(lambda (extension)
           (setq completion-ignored-extensions
                 (remove extension completion-ignored-extensions)))
        vcs-extensions)
  ;; (setq ido-ignore-files
  ;;       (append
  ;;        ;; But do ignore files that are just .git, .hg, .svn, etc.
  ;;        ;; generate regexes that are ^.git, etc.
  ;;        (mapcar #'(lambda (arg) (concat "^" arg)) vcs-extensions)
  ;;        ido-ignore-files))
  )
;;;

(winner-mode 1)

;; Revert automatically. Only reverts nonmodified files. This might cause
;; a lot of network traffic when used with tramp?
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1))

;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=49211
;; and friends. Maybe fixed in GTK 4?
;;(pgtk-use-im-context nil)
;;
;; I think this affected me most when using C->, but that seems to
;; work now, so I'm commenting it out for now. The reported bug is
;; about S-SPC, which I think is still affected, but C-S-SPC seems to
;; be translated correctly, and I also don't think I have any bindings
;; for this key in any mode?

(provide 'ethan-navigation)
