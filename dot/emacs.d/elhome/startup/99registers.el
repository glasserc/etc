;;; ethan-registers.el --- Set up registers. Stolen from Emacs-Starter-Kit.

;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

(dolist (r `((?i (file . ,(emacs-d "init.el")))
             (?b (file . ,(emacs-d "ethan-bindings.el")))
             (?r (file . ,(emacs-d "ethan-registers.el")))
             (?t (file . ,(expand-file-name "~/src/org-files/todo.org")))))
  (set-register (car r) (cadr r)))

(provide 'ethan-registers)
