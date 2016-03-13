(message "Running org-settings")
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

(add-to-list 'Info-default-directory-list (expand-file-name "~/.emacs.d/elhome/site-lisp/upstream/org-mode.git/doc"))
