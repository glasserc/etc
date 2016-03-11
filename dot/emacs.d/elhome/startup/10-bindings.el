;; Taken from the Emacs Starter Kit
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Use swiper searches by default.
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

;; Jump to a definition in the current file. (This is awesome.)
;; In org-mode, you should probably use org-goto instead.
(global-set-key (kbd "C-x C-i") 'imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'ivy-recentf) ;; although see ivy-use-virtual-buffers
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
;; This is almost always a typo.
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
;; (define-key counsel-find-file-map (kbd "C-j") 'ivy-done)
;; (define-key counsel-find-file-map (kbd "RET") 'ivy-alt-done)

;; This mode is very rarely useful.
(global-set-key (kbd "C-x C-S-b") 'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)
;; For debugging Emacs modes
(global-set-key (kbd "C-c p") 'message-point)

;; So good!
(global-set-key (kbd "C-x g") 'magit-status)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Org
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Personal customization
; At present, emacs does not define bindings for these keys,
; but various modes do: org mode, paredit, etc
(define-key me-minor-mode-map (kbd "M-<up>") 'scroll-down-one)
(define-key me-minor-mode-map (kbd "M-<down>") 'scroll-up-one)
; This shadows previous count-lines-region
(define-key esc-map "=" 'count-words)

;; In theory these are reserved for major modes, but I like the python-mode
;; bindings (python-indent-shift-left and
;; python-indent-shift-right) and want to use them in other
;; places. Unfortunately, those bindings are only good in
;; python-mode. increase-left-margin and decrease-left-margin are OK
;; substitutes in other contexts, so make them global. They reflow the
;; text, so let python-mode override them.
(global-set-key [?\C-c ?>] 'increase-left-margin)
(global-set-key [?\C-c ?<] 'decrease-left-margin)

;;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; disable C-z on X11 sessions
(when window-system
  (global-unset-key "\C-z"))

;; I'm not sure about this because maybe there's a better way to
;; accomplish it, but this is how every other program on the system
;; does compose keys, so..
;; N.B. this might not be necessary with recent Emacs
(define-key key-translation-map [Multi_key]
  (lookup-key key-translation-map (kbd "C-x 8")))

;; disable C-x C-c because I keep hitting it by accident and I hardly
;; ever close emacs. Normally it's save-buffers-kill-terminal.
(global-unset-key (kbd "C-x C-c"))

;; Somehow I thought org-mode conflicted with winner; here's some
;; alternate keybindings.
;;(define-key me-minor-mode-map (kbd "C-c C-_") 'winner-undo)
;;(define-key me-minor-mode-map (kbd "C-c M-_") 'winner-redo)

;; Multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-%") 'mc/mark-all-dwim)


;; Avy, which replaces ace-jump-mode
(define-key me-minor-mode-map (kbd "C-:") 'avy-goto-char)
(define-key me-minor-mode-map (kbd "C-'") 'avy-goto-char-2)
(define-key me-minor-mode-map (vector 'remap 'goto-line) 'avy-goto-line)
(define-key me-minor-mode-map (kbd "M-g w") 'avy-goto-word-1)
(define-key me-minor-mode-map (kbd "M-g e") 'avy-goto-word-0)
(define-key me-minor-mode-map (vector 'remap 'other-window) 'ace-window)
