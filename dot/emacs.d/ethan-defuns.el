;; These are taken from the Emacs Starter Kit
(require 'thingatpt)
(require 'imenu)

;; Network

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;; Buffer-related

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; Other

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0)
  ;; TODO: remove elpa-to-submit once everything's submitted.
  (byte-recompile-directory (user-emacs-directory "elpa-to-submit/") 0))

(defun regen-autoloads (&optional force-regen)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (let ((autoload-dir (emacs-d "/elpa-to-submit"))
        (generated-autoload-file autoload-file))
    (when (or force-regen
              (not (file-exists-p autoload-file))
              (some (lambda (f) (file-newer-than-file-p f autoload-file))
                    (directory-files autoload-dir t "\\.el$")))
      (message "Updating autoloads...")
      (let (emacs-lisp-mode-hook)
        (update-directory-autoloads autoload-dir))))
  (load autoload-file))

;; I don't like this exactly, but I want to write something like this
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun turn-on-paredit ()
  (paredit-mode t))

;; This is never used
(defun turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (unless paredit-mode
    (set (make-local-variable 'paredit-space-delimiter-chars)
         (list ?\"))
    (paredit-mode 1)))

(defun message-point ()
  (interactive)
  (message "%s" (point)))

(defun toggle-fullscreen ()
  (interactive)
  ;; TODO: this only works for X. patches welcome for other OSes.
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;;; travelogue
(defun find-blog-entry-for-now (base)
  (let* ((target (concat base (format-time-string "%Y/%m/%d/%T.rst")))
        (directory (file-name-directory target)))
    (make-directory directory t)
    (find-file target)))

(setq travelogue-location (expand-file-name "~/src/travelogue/posts/"))
(defun travelogue-now ()
  (interactive)
  (find-blog-entry-for-now travelogue-location))

(setq cameroon-location (expand-file-name "~/src/cameroon/posts/"))
(defun cameroon-now ()
  (interactive)
  (find-blog-entry-for-now cameroon-location))

(setq journal-location (expand-file-name "~/writing/journal/"))
(defun journal-today ()
  (interactive)
  (let* ((now (current-time))
         (decoded (decode-time now))
         (hr (caddr decoded))
         (appropriate-time (if (< hr 5) ; not yet 5 AM; it's still yesterday
                               (progn
                                 (setf (cadddr decoded) (- (cadddr decoded) 1))
                                 decoded)
                             decoded))
         (filename (concat journal-location
                           (format-time-string "%Y-%m-%d"
                                               (apply 'encode-time decoded)))))
    (find-file filename)))

;; Useful editing keybindings:
;; count-words (M-=, replaces default count-lines-region)
(defun count-words (&optional begin end)
  "Runs wc on region (if active) or otherwise the whole buffer."
  (interactive
   (list
    (if (and transient-mark-mode mark-active)
        (region-beginning)
      (point-min))
    (if (and transient-mark-mode mark-active)
        (region-end)
      (point-max))))
  (shell-command-on-region begin end "wc"))

;; scroll-up-one, M-down, maybe I should get rid of this, but I got used
;; to it when I was using XEmacs
(defun scroll-up-one (arg)
  (interactive "p")
  (scroll-up 1))

;; scroll-down-one, ditto
(defun scroll-down-one (arg)
  (interactive "p")
  (scroll-down 1))

;; used in text-mode-hook
(defun turn-on-visual-line-mode ()
  (visual-line-mode 1))

(provide 'ethan-defuns)
