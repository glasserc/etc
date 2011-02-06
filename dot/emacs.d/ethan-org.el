;;; org-mode configs
(require 'org)
; FIXME: this doesn't seem necessary on sundance, but does on colt -- why?
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("/TODO$" . org-mode))
(setq org-directory "~/src/org-files")
(setq org-log-done t)
(setq org-clock-into-drawer t)
(setq org-footnote-section nil)
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

; I keep someday.org files, can't include those. Maybe a better
; approach is needed?
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

; customize?
;org-enforce-todo-dependencies
;org-track-ordered-property-with-tag
;org-agenda-dim-blocked-tasks   ; Except, I want "BLOCKING" tasks to match too
;org-log-into-drawer
;org-tag-alist

; Get off my movement keys!
; (M-S-<up> and M-S-<down> behave almost exactly the same way)
(define-key org-mode-map [(meta up)]    nil)
(define-key org-mode-map [(meta down)]  nil)
; I use this for esvn-mode.
; FIXME: org-mode + esvn mode needs work.
; C-c C-c should work outside of esvn mode?
; (Maybe esvn-mode should use a different key?)
; FIXME: taking this out -- all org stuff happens in another repo, not using
; esvn. This is a very commonly used binding in org mode
;(define-key org-mode-map "\C-c\C-c" nil)
(setq org-completion-use-ido t)
(setq org-tag-alist '((:startgroup)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      (:endgroup)
                      ("SOMEDAY" . ?s)  ; Not sure about this
                      ))

; yasnippet config from org-mode mailing list
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
         (lambda ()
           ;; yasnippet (using the new org-cycle hooks)
           (make-variable-buffer-local 'yas/trigger-key)
           (setq yas/trigger-key [tab])
           (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
           (define-key yas/keymap [tab] 'yas/next-field)))

; M-/ is my dabbrev-command -- should bind it to org-complete, and org-completion-fallback-command
;;; org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

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


(provide 'ethan-org)
