;; ethan-esvn: setup for my crappy esvn.el package
;;
;; This has probably been obsoleted in any number of ways in reality,
;; and the original code badly needs cleanup, but I still use it, and
;; it has to go somewhere..

;; So wait, why do I need both a major-mode and a function to enable
;; esvn-mode?

(require 'esvn)
(define-derived-mode journal-mode text-mode "Journal"
  "Major mode for editing Ethan's diary."
  (set (make-local-variable 'esvn-default-commit-message) "Entry.")
  (set (make-local-variable 'esvn-default-autocommit-message) "autocommit")
  (set (make-local-variable 'esvn-default-add-message) "New day.")
  (esvn-mode t)
  (local-set-key [?\C-x ?\C-s] 'esvn-save-or-autocommit))

(defun enable-esvn ()
  (message (buffer-file-name))
  (when (string-match "writing/" (buffer-file-name))
    (set (make-local-variable 'esvn-default-commit-message) "Edit.")
    (set (make-local-variable 'esvn-default-add-message) "Begin.")
    (set (make-local-variable 'esvn-svn-command) "hg")
    (esvn-mode t)))

;(add-hook 'find-file-not-found-hooks 'enable-esvn)
(add-hook 'find-file-hooks 'enable-esvn)

(setq auto-mode-alist
      (cons
       '("writing/journal/"     . journal-mode)
         auto-mode-alist))

(provide 'ethan-esvn)
