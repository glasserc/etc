;;; haml-mode
(autoload 'sass-mode "sass-mode" "Mode for editing SASS code." t)

(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(provide 'ethan-haml)
