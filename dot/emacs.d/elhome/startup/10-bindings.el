;; Taken from the Emacs Starter Kit
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Help should search more than just commands.
(global-set-key [remap apropos-command] 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)


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
