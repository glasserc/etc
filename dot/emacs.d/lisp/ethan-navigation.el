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
(define-key me-minor-mode-map (kbd "M-<up>") 'scroll-down-one)
(define-key me-minor-mode-map (kbd "M-<down>") 'scroll-up-one)

;; disable C-z on X11 sessions
(when window-system
  (global-unset-key "\C-z"))

;; disable C-x C-c because I keep hitting it by accident and I hardly
;; ever close emacs. Normally it's save-buffers-kill-terminal.
(global-unset-key (kbd "C-x C-c"))

(provide 'ethan-navigation)
