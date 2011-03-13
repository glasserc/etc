(setq notmuch-search-oldest-first nil)

;; This should probably go in offlineimap-settings
(setq offlineimap-enable-mode-line-p
      '(member
        major-mode
        '(offlineimap-mode notmuch-hello-mode)))
