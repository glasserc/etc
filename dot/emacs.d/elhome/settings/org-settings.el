(message "Running org-settings")
;; Visual-line mode wraps tags because they're followed by "...".
;; This hack leaves one extra space after the tags, so that they don't
;; wrap.
;; P.S. This is totally a hack.
(setq org-tags-column -76)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Compute all org files.
;; These represent our refile targets.
(setq org-directory "~/src/org-files")  ; not in custom because we use
                                        ; its value
(setq org-all-org-files (directory-files org-directory t ".org$" t))
;; Agenda files are a specific subset of these:
;; - todo.org: generic shit-to-do
;; - incoming.org: shit I saw and wanted to deal with later
;; - writing.org: things to write about
;; - contacts.org: notes about people, including reminders to interact
;; with them.
;; - coding.org: things to hack on, mostly non-urgent

;; Also present are:
;; - music.org: albums to listen to, etc.
;; - house.org: clocked hours spent around the house
;; - someday.org: mulch pile for stuff I'd like to mess around with
;; someday
;; - purchases.org: similar, but things I'd like to buy
;;
;; The unifying theme here is that these files represent non-urgent
;; things -- stuff I can pull up or let mulch at will.  Agenda files
;; are "what I need to work on".

;; Not in customize: computed automatically.
;; Note that this probably isn't used, since my capture templates
;; specify files.
(setq org-default-notes-file (concat org-directory "/incoming.org"))

;; Make windmove work in org-mode: (from org-mode manual)
;; I'm not sure this is the way I want to fix this, since I hardly
;; ever use priorities on tasks, so maybe I just want to always use
;; windmove unconditionally?
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;; Org-Babel: embed code into your org files and then execute.
;; I use this just enough that I may as well require it.
(require 'ob-python)
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

(add-to-list 'Info-default-directory-list (expand-file-name "~/.emacs.d/elhome/site-lisp/upstream/org-mode.git/doc"))
;; Force refresh of Info-directory-list, since it might have been
;; generated before this addition.
(setq Info-directory-list nil)
(info-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/src/org-files/coding.org" "~/src/org-files/contacts.org" "/home/ethan/src/org-files/writing.org" "/home/ethan/src/org-files/incoming.org" "/home/ethan/src/org-files/todo.org")))
 '(org-agenda-restore-windows-after-quit t)
 '(org-archive-mark-done nil)
 '(org-capture-templates
   (quote
    (("t" "todo" entry
      (file+headline "~/src/org-files/incoming.org" "New")
      "* TODO %?
%u
%a")
     ("b" "Book to purchase" entry
      (file+headline "~/src/org-files/purchases.org" "Books")
      "* TODO %?
%x"))))
 '(org-clock-history-length 10)
 '(org-clock-into-drawer t)
 '(org-completion-use-ido t)
 '(org-drawers
   (quote
    ("PROPERTIES" "CLOCK" "DETAILS")))
 '(org-footnote-section nil)
 '(org-log-done
   (quote time))
 '(org-refile-targets
   (quote
    ((org-all-org-files :level . 1))))
 '(org-tag-alist
   (quote
    ((:startgroup)
     ("@work" . 119)
     ("@home" . 104)
     (:endgroup)
     ("internet" . 105)
     ("SOMEDAY" . 115))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "BLOCKING" "WORKING" "|" "DELEGATED(D)" "DONE(d)" "WONTFIX(W)")))))
