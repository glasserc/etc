(setq twittering-use-master-password t
      twittering-timer-interval 300      ; Update your timeline each 300 seconds (5 minutes)
      twittering-url-show-status nil     ; Keeps the echo area from showing all the http processes
      )
(defun turn-on-twittering-icon-mode ()
  (twittering-icon-mode 1))

(add-to-list 'twittering-mode-hook 'turn-on-twittering-icon-mode)
