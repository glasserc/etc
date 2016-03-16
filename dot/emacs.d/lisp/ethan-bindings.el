;; Taken from the Emacs Starter Kit
(bind-key "C-x \\" 'align-regexp)

;; Start eshell or switch to it if it's active.
(bind-key "C-x m" 'eshell)

;; Start a new eshell even if one is active.
(bind-key "C-x M" (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(bind-key "C-x M-m" 'shell)

;; If you want to be able to M-x without meta (phones, etc)
(bind-key "C-x C-m" 'execute-extended-command)

;; Help should search more than just commands.
(bind-key [remap apropos-command] 'apropos)

;; In theory these are reserved for major modes, but I like the python-mode
;; bindings (python-indent-shift-left and
;; python-indent-shift-right) and want to use them in other
;; places. Unfortunately, those bindings are only good in
;; python-mode. increase-left-margin and decrease-left-margin are OK
;; substitutes in other contexts, so make them global. They reflow the
;; text, so let python-mode override them.
(bind-key [?\C-c ?>] 'increase-left-margin)
(bind-key [?\C-c ?<] 'decrease-left-margin)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'ethan-bindings)
