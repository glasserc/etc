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
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)

;; This mode is very rarely useful.
(global-set-key (kbd "C-x C-S-b") 'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'view-url)

;; Personal customization
; At present, emacs does not define bindings for these keys,
; but various modes do: org mode, paredit, etc
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
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)
   ("C-x f" . ivy-recentf)  ;; although see ivy-use-virtual-buffers
   ;; This is almost always a typo.
   ("C-x C-b" . ivy-switch-buffer))
  :config
  (csetq ivy-extra-directories nil)
  (ivy-mode))
(use-package counsel
  :config
  ;; Make counsel behave a little bit more like ido.
  ;;
  ;; RET should enter directories without ending the find-file.  C-j
  ;; can be preserved to open a directory in dired, in case that's
  ;; necessary.
  (counsel-mode t)
  (define-key counsel-find-file-map (kbd "C-j") 'ivy-done)
  (define-key counsel-find-file-map (kbd "RET") 'ivy-alt-done))

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

(provide 'ethan-navigation)
