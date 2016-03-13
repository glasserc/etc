(setq notmuch-search-oldest-first nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-poll-script "~/etc/mail/offlineimap-poll")
 '(notmuch-saved-searches
   (quote
    (("now" . "tag:inbox and tag:unread")
     ("inbox" . "tag:inbox")
     ("unread" . "tag:unread")
     ("confirms " . "tag:confirms and tag:unread")
     ("patch-queue-review" . "tag:notmuch::patch and not tag:notmuch::pushed and not tag:notmuch::obsolete and not tag:notmuch::stale and not tag:notmuch::wontfix and (tag:notmuch::moreinfo or tag:notmuch::needs-review)")
     ("needs-review" . "tag:notmuch::patch and not tag:notmuch::pushed and not tag:notmuch::obsolete and not tag:notmuch::stale and not tag:notmuch::wontfix and not notmuch::moreinfo and tag:notmuch::needs-review")
     ("notmuch" . "list:notmuch")
     ("coderdojo" . "tag:coderdojo")
     ("ledger" . "tag:ledger")
     ("couchdb" . "tag:couchdb")
     ("lists" . "tag:lists"))))
 '(notmuch-search-oldest-first nil))
