;;; Lisp customizations, wholly stolen from the
;;; Emacs Starter Kit

;;; This should be loaded by default, right? I don't use clojure-mode
;;; or scheme-mode, so this might be broken in those cases.

(bind-key "TAB" 'lisp-complete-symbol read-expression-map)
(bind-keys
 :map lisp-mode-shared-map
 ("C-c l" . "lambda")
 ("RET" . reindent-then-newline-and-indent)
 ("C-\\" . lisp-complete-symbol)
 ("C-c v" . eval-buffer))

(defface esk-paren-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; For debugging Emacs modes
(defun message-point ()
  (interactive)
  (message "%s" (point)))
(bind-key "C-c p" 'message-point)

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;;; Clojure

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(use-package paredit
  :config
  (dolist (x '(scheme emacs-lisp lisp clojure))
    (when window-system
      (font-lock-add-keywords
       (intern (concat (symbol-name x) "-mode"))
       '(("(\\|)" . 'esk-paren-face))))
    (add-hook
     (intern (concat (symbol-name x) "-mode-hook")) 'paredit-mode)))

(provide 'ethan-lisp)
