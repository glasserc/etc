;; CEDET is pretty neat
(load "common/cedet.el")
;(global-ede-mode t)
(semantic-load-enable-excessive-code-helpers)
(require 'semantic-ia)
(setq semanticdb-default-save-directory (emacs-d "cache/semanticdb"))
;; Some kind of weird bug with exuberent-ctags in python-mode?
;;(semantic-load-enable-primary-exuberent-ctags-support)

;;(require 'semantic-gcc)

(provide 'ethan-cedet)
