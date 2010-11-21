(setq notmuch-search-oldest-first nil)
(require 'notmuch)
(setq offlineimap-enable-mode-line-p
                        '(member
                         major-mode
                         '(offlineimap-mode notmuch-hello-mode)))

(provide 'ethan-notmuch)
