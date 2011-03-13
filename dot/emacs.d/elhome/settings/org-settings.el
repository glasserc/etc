(message "Running org-settings")
(setq org-directory "~/src/org-files")
(setq org-log-done t)
(setq org-clock-into-drawer t)
(setq org-footnote-section nil)
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Automatically compute the list of files to use in the agenda.
;; I keep someday.org files, can't include those. Maybe a better
;; approach is needed?
(setq all-org-files (directory-files org-directory t ".org$" t))
(let
    ((org-dir (expand-file-name org-directory)))
     (setq org-agenda-files (remove
                             (concat org-dir "/someday.org")
                             (remove
                              (concat org-dir "/feeds.org")
                         all-org-files))))

(setq org-todo-keywords '((sequence "TODO(t)" "BLOCKING" "WORKING" "|" "DELEGATED(D)" "DONE(d)" "WONTFIX(W)")))
(setq org-use-fast-todo-selection t)

;; Still messing around with this.
(load (concat org-directory "/feeds-list.el"))

;; customize?
;;org-enforce-todo-dependencies
;;org-track-ordered-property-with-tag
;;org-agenda-dim-blocked-tasks   ; Except, I want "BLOCKING" tasks to match too
;;org-log-into-drawer
;;org-tag-alist

;; M-<up> and M-<down> are shadowed by my personal scroll up/down
;; function.  (M-S-<up> and M-S-<down> behave almost exactly the same
;; way)
; M-/ is my dabbrev-command -- should bind it to org-complete, and org-completion-fallback-command

(setq org-completion-use-ido t)
(setq org-tag-alist '((:startgroup)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      (:endgroup)
                      ("SOMEDAY" . ?s)  ; Not sure about this
                      ))

(setq org-default-notes-file (concat org-directory "/notes.org"))

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
