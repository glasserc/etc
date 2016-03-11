(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

;; This isn't strictly needed here, but needs to happen before we open
;; a .el file (e.g. by el-get)
(defun turn-on-paredit ()
  (paredit-mode t))


(add-hook 'prog-mode-hook 'local-comment-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'pretty-lambdas)
(add-hook 'prog-mode-hook 'add-watchwords)
(add-hook 'prog-mode-hook 'idle-highlight)

(defun run-prog-mode-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'prog-mode-hook))

;; FIXME: take this out if CSS ever starts being a programming language
(add-hook 'css-mode-hook 'run-prog-mode-hook)

;; Cosmetic

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
