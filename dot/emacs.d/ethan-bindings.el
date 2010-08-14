; At present, emacs does not define bindings for these keys
(define-key global-map (kbd "M-<up>") 'scroll-down-one)
(define-key global-map (kbd "M-<down>") 'scroll-up-one)
;(define-key global-map (kbd "M-g") 'goto-line)
(define-key esc-map "=" 'count-words)

; In theory these are reserved for major modes, but I like the python-mode
; bindings, so I'm making them global.
(global-set-key [?\C-c ?>] 'increase-left-margin)
(global-set-key [?\C-c ?<] 'decrease-left-margin)

;;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'ethan-bindings)
