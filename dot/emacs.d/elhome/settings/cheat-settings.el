(setq cheat-executable (expand-file-name "~/.gem/ruby/1.8/bin/cheat"))

;; Just make the cheat-executable path be configurable
(defun cheat-command (&rest rest)
  "Run the cheat command with the given arguments, display the output."
  (interactive "sArguments for cheat: \n")
  (let* ((cmd (string-join " " rest))
         (buffer (get-buffer-create
                  (concat "*Cheat: " cmd "*"))))
;;         (message "Running: %s" (concat cheat-executable " " cmd))
    (shell-command (concat cheat-executable " " cmd) buffer)))
(defun cheat-command-to-string (&rest rest)
  "Run the cheat command with the given arguments and return the output as a
  string.  Display nothing."
  (shell-command-to-string (concat cheat-executable " " (string-join " " rest))))
