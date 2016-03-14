(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(xterm-mouse-mode t)

;; Revert automatically. Only reverts nonmodified files. This might cause
;; a lot of network traffic when used with tramp?
(global-auto-revert-mode 1)

;; I don't think I'll actually use this, but it doesn't hurt
(setq recentf-max-saved-items 100
      recentf-max-menu-items 15)
(recentf-mode 1)

;; I don't use imenu, but it's a better default
(set-default 'imenu-auto-rescan t)

;;; abbrev
(setq abbrev-file-name (emacs-d "abbrev_defs.el"))
(read-abbrev-file abbrev-file-name t)
(setq abbrev-mode t)   ; not really sure about this... RST has some abbrevs too

;;; elide-head
(require 'elide-head)

(setq elide-head-headers-to-hide
      (append
       '(("Copyright (C) 2008 10gen Inc\\." . "If not, see <http")   ; AGPL
         ("Copyright (C) 2008 10gen Inc\\." . "under the License\\.") ; APL
         )
       elide-head-headers-to-hide))

(add-hook 'find-file-hook 'elide-head)
;;; end elide-head

(provide 'ethan-misc)
