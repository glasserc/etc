;; keep a copy of el-get-install.el somewhere and exec it if this directory doesn't exist
(unless (file-exists-p "~/.emacs.d/el-get/el-get")
  (save-excursion
    (find-file-literally "~/.emacs.d/el-get-install.el")
    (end-of-buffer)
    (eval-last-sexp)))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-sources
      '(iedit rst-mode java-mode-indent-annotations haml-mode
              (:name yasnippet
                     :after
                     (lambda () (yas/load-directory (emacs-d "my-snippets"))))
              (:name elhome
               :after elhome-init)))

(el-get)

(provide 'ethan-el-get)
