;; save scratch: adapted from
;; http://xxtjaxx.homelinux.net/snippet/SaveYourScratch/, which has
;; been down for a while
(defun save-scratch ()
  (switch-to-buffer (get-buffer "*scratch*"))
  (write-file "~/.emacs.d/scratch.el" nil)
  )
(add-hook 'kill-emacs-hook 'save-scratch)
(when (file-exists-p "~/.emacs.d/scratch.el")
    (setq initial-scratch-message (shell-command-to-string "cat ~/.emacs.d/scratch.el")))
