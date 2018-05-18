(use-package org
  :ensure org-plus-contrib
  :pin org
  :config
  ;; Visual-line mode wraps tags because they're followed by "...".
  ;; This hack leaves one extra space after the tags, so that they don't
  ;; wrap.
  ;; P.S. This is totally a hack.
  (setq org-tags-column -76)

  ;; Make windmove work in org-mode: (from org-mode manual)
  ;; I'm not sure this is the way I want to fix this, since I hardly
  ;; ever use priorities on tasks, so maybe I just want to always use
  ;; windmove unconditionally?
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (run-at-time "00:59" 3600 'org-save-all-org-buffers)


  ;; Org-Babel: embed code into your org files and then execute.
  ;; I use this just enough that I may as well require it.
  (require 'ob-python)

  ;; Fix weirdness with org-mode and yasnippet (both use TAB)
  ;; yasnippet config from org-mode mailing list
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (defun org-and-yasnippet-compatibility ()
    ;; yasnippet (using the new org-cycle hooks)
    (make-variable-buffer-local 'yas/trigger-key)
    (setq yas/trigger-key [tab])
    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
    (define-key yas/keymap [tab] 'yas/next-field))

  (add-hook 'org-mode-hook
            'org-and-yasnippet-compatibility)

  ;; RST export for orgtbl
  (defun orgtbl-to-rst-line (line)
    (apply 'format (cons *org-rst-lfmt* line)))

  (defun orgtbl-to-rst (table params)
    "Convert the Orgtbl mode TABLE to ReStructuredText."
    (let* ((hline (concat
                   "+-"
                   (mapconcat (lambda (width) (apply 'string (make-list width ?-)))
                              org-table-last-column-widths "-+-")
                   "-+"))
           (*org-rst-lfmt* (concat
                            "| "
                            (mapconcat (lambda (width) (format "%%-%ss" width))
                                       org-table-last-column-widths " | ")
                            " |"))
           (params2
            (list
             :tstart hline
             :hline hline
             :lfmt 'orgtbl-to-rst-line
             )))
      (orgtbl-to-generic table (org-combine-plists params2 params))))

  ;; I don't tend to indent text inside headlines.
  (csetq org-adapt-indentation nil)
  ;; Org files are mostly in my personal org files repository,
  ;; ~/src/org-files. This repository has autocommit.git set up so as
  ;; to use git as a transport layer to synchronize everything across
  ;; my machines.
  (csetq org-directory "~/src/org-files")
  (defun org-personal-org-files-make-absolute (filename)
    (concat org-directory "/" filename))
  (setq org-personal-org-files (directory-files org-directory nil ".org$" t))

  ;; Files currently in this directory are, in decreasing order of importance:
  ;; - todo.org: generic shit-to-do
  ;; - incoming.org: capture target
  ;; - notes.org: random "yellow sticky notes" that aren't associated
  ;; with any specific action
  ;; - reminders.org: actions that have a timeframe associated with
  ;; them and are not really relevant until then
  ;; - writing.org: things to write about
  ;; - coding.org: things to hack on
  ;; - someday.org: mulch pile for stuff I'd like to mess around with
  ;; someday
  ;; - contacts.org: notes about people, including reminders to interact
  ;; with them
  ;; - purchases.org: things I want to spend money on, but only a
  ;; little at a time
  ;; - music.org: similar, but focused on music, and with some notes
  ;; about bands whose music I won't buy because it's owned by a major
  ;; label

  ;; contacts.org, purchases.org, and music.org are all files that are
  ;; only relevant in specific contexts -- I review contacts.org
  ;; periodically to make sure I'm keeping my more distant social
  ;; relationships, and purchases.org and music.org are both
  ;; "wishlists"/backlogs of stuff I want to buy when I have the budget.
  (setq org-default-notes-file (org-personal-org-files-make-absolute "incoming.org"))
  (setq org-personal-org-files-not-for-agenda
        '("contacts.org" "purchases.org" "music.org"))
  (setq org-personal-org-files-for-agenda
        (seq-remove
         #'(lambda (filename) (member filename org-personal-org-files-not-for-agenda))
         org-personal-org-files))

  (defvar org-employer-org-files
    (file-expand-wildcards "~/Jobs/*/org-files/*.org")
    "Org files specific to an employer.")
  (defvar org-project-org-files
    (file-expand-wildcards "~/src/*/org-files/*.org")
    "Org files specific to a project.")

  (setq org-agenda-files
        (append
         org-employer-org-files
         org-project-org-files
         (seq-map #'org-personal-org-files-make-absolute org-personal-org-files-for-agenda)
         ))
  (setq org-all-org-files
        (append
         org-employer-org-files
         org-project-org-files
         (seq-map #'org-personal-org-files-make-absolute org-personal-org-files)
         ))
  (csetq org-refile-targets '((org-all-org-files :level . 1)
                              (nil :level . 1)))

  (csetq org-enforce-todo-dependencies nil)
  ;;(csetq org-agenda-dim-blocked-tasks t)
  (csetq org-agenda-restore-windows-after-quit t)
  (defun org-agenda-music-amount-in-past (entry)
    "Calculate how far in the past `entry' is.

The org docstrings say that priorities take deadlines and
timestamps into account, but they don't as far as I can tell. Try
to remedy this by bringing entries that are in the past
towards the top of a category.

Returns the number of days in the past that the entry is."
    (let*
        ((entry-marker (get-text-property 1 'org-marker entry))
         (entry-time (org-entry-get entry-marker "SCHEDULED"))
         (entry-time-future-distance
          (when entry-time
            (-
             (time-to-days (apply #'encode-time (org-parse-time-string entry-time)))
             (time-to-days (current-time))))))
      (if (and entry-time (< entry-time-future-distance 0))
          (- entry-time-future-distance)
        0)))
  (defun org-agenda-compare-music (a b)
    (let* ((a-prio (org-agenda-music-amount-in-past a))
           (b-prio (org-agenda-music-amount-in-past b)))
      (cond
       ((< a-prio b-prio) -1)
       ((> a-prio b-prio) 1))
      ))
  (defun org-agenda-music-is-overdue-flag (txt)
    "Add a little ! notifier for tags that have been bumped due to being overdue."
    (if (< 0 (org-agenda-music-amount-in-past txt))
        "! " ""))
  (defun org-agenda-music-skip-one-breadcrumb ()
    "Get breadcrumbs but skip the first, treating it as a \"category\"."
    (org-with-point-at (org-get-at-bol 'org-marker)
      (let* ((s (org-display-outline-path nil nil "___" t))
             (path (unless (eq "" s) (split-string s "___")))
             (skipped (when path (cdr path)))
             (new-path (when skipped (string-join skipped " - "))))
        (if new-path
            (concat new-path " - ")
          ""))))
  (csetq org-agenda-custom-commands
         `(("r" . "Review certain files")
           ("rc" "Review contacts" agenda ""
            ((org-agenda-files (list (org-personal-org-files-make-absolute "contacts.org")))))
           ("rm" "Review music" todo ""
            ((org-agenda-files (list (org-personal-org-files-make-absolute "music.org")))
             (org-agenda-sorting-strategy
              '(todo-state-down
                user-defined-down
                ))
             (org-agenda-cmp-user-defined #'org-agenda-compare-music)
             ;; Inserting the breadcrumb before the item is a little
             ;; distracting. Instead we should list it on the right
             ;; side as additional context or something else.
             (org-agenda-prefix-format '((todo . " %i %-12:c%(org-agenda-music-skip-one-breadcrumb)%(org-agenda-music-is-overdue-flag txt)")))
             ))))
  (csetq org-capture-templates
         '(("t" "todo" entry
            (file+headline "~/src/org-files/incoming.org" "New")
            "* TODO %?
%u
%a")
           ("b" "Book to purchase" entry
            (file+headline "~/src/org-files/purchases.org" "Books")
            "* TODO %?
%x")))
  (csetq org-clock-history-length 10)
  (csetq org-clock-into-drawer t)
  (csetq org-completion-use-ido t)
  (csetq org-drawers
         '("PROPERTIES" "CLOCK" "DETAILS"))
  (csetq org-footnote-section nil)
  (csetq org-goto-interface 'outline-path-completion)
  (csetq org-log-done 'time)
  (csetq org-outline-path-complete-in-steps nil)
  (csetq org-tag-alist
         '((:startgroup)
           ("@work" . 119)
           ("@home" . 104)
           (:endgroup)
           ("internet" . 105)
           ("SOMEDAY" . 115)))
  (csetq org-todo-keywords
         '((sequence "TODO(t)" "BLOCKING" "WORKING" "|" "DELEGATED(D)" "DONE(d)" "WONTFIX(W)")))
;; customize?
;;org-enforce-todo-dependencies
;;org-track-ordered-property-with-tag
;;org-agenda-dim-blocked-tasks   ; Except, I want "BLOCKING" tasks to match too
;;org-log-into-drawer
;; org-tag-alist  -- I'm not sure about "SOMEDAY"
;; org-outline-path-complete-in-steps

;; M-<up> and M-<down> are shadowed by my personal scroll up/down
;; function.  (M-S-<up> and M-S-<down> behave almost exactly the same
;; way)
; M-/ is my dabbrev-command -- should bind it to org-complete, and org-completion-fallback-command

  ;; org-contacts: manage contacts as org items
  ;; This basically just provides the org-contacts function, which
  ;; isn't as useful as an org agenda view
  (require 'org-contacts)

  (csetq org-contacts-files
         '("/home/ethan/src/org-files/contacts.org"))
  (csetq org-contacts-matcher "-historical")

  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)))

(provide 'ethan-org)
