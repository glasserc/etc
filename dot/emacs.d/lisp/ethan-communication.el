(eval-when-compile
  (require 'ethan-custom))

(use-package notmuch :ensure nil
  :config
  (setq notmuch-search-oldest-first nil)
  (csetq notmuch-poll-script "~/etc/mail/offlineimap-poll")
  (csetq notmuch-saved-searches
         '(("now" . "tag:inbox and tag:unread")
           ("inbox" . "tag:inbox")
           ("unread" . "tag:unread")
           ("confirms " . "tag:confirms and tag:unread")
           ("patch-queue-review" . "tag:notmuch::patch and not tag:notmuch::pushed and not tag:notmuch::obsolete and not tag:notmuch::stale and not tag:notmuch::wontfix and (tag:notmuch::moreinfo or tag:notmuch::needs-review)")
           ("needs-review" . "tag:notmuch::patch and not tag:notmuch::pushed and not tag:notmuch::obsolete and not tag:notmuch::stale and not tag:notmuch::wontfix and not notmuch::moreinfo and tag:notmuch::needs-review")
           ("notmuch" . "list:notmuch")
           ("coderdojo" . "tag:coderdojo")
           ("ledger" . "tag:ledger")
           ("couchdb" . "tag:couchdb")
           ("lists" . "tag:lists")))
  (csetq notmuch-search-oldest-first nil))

(provide 'ethan-communication)
