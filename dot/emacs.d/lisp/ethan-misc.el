(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(xterm-mouse-mode t)

;; Revert automatically. Only reverts nonmodified files. This might cause
;; a lot of network traffic when used with tramp?
(global-auto-revert-mode 1)

(show-paren-mode 1)
(size-indication-mode t)
;; I don't think I'll actually use this, but it doesn't hurt
(setq recentf-max-saved-items 100
      recentf-max-menu-items 15)
(recentf-mode 1)

;; I don't use imenu, but it's a better default
(set-default 'imenu-auto-rescan t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

;; Ido: don't ignore project.git, but ignore .git itself.
(let ((vcs-extensions '(".svn/" ".hg/" ".git/" ".bzr/")))
  ;; remove .git/ from completion-ignored-extensions, because it matches endings
  (mapc #'(lambda (extension)
           (setq completion-ignored-extensions
                 (remove extension completion-ignored-extensions)))
        vcs-extensions)
  (setq ido-ignore-files
        (append
         ;; But do ignore files that are just .git, .hg, .svn, etc.
         ;; generate regexes that are ^.git, etc.
         (mapcar #'(lambda (arg) (concat "^" arg)) vcs-extensions)
         ido-ignore-files)))
;;;

;;; wspace -- both displaying, and editing
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;; CUA Rectangle stuff
;; (setq cua-enable-cua-keys nil)           ;; only for rectangles
;; (cua-mode t)

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

;; This takes a long time
;(yas/load-directory (concat yasnippet-directory "/snippets"))

(setq yas/wrap-around-region t
      yas/prompt-functions '(yas/x-prompt yas/ido-prompt))

;;; desktop-mode config
; I don't expect to ever use it, but it's nice to
; have.  Save session with desktop-save, then read using desktop-read.
; desktop-save doesn't overwrite it's previous save.. not sure why or
; how to fix. Maybe desktop-save-mode?
(set-default 'desktop-path (list (expand-file-name "~/.emacs.d/")))
;;; end desktop-mode

;; Some modes
(setq auto-mode-alist
      (append
       '(
         ("writing/"             . text-mode)
         ("\\.mdwn$"             . mdwn-mode)
         ) auto-mode-alist))
;(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))

(setq diff-switches "-u")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; text mode
(add-to-list 'text-mode-hook 'visual-line-mode)
(add-to-list 'text-mode-hook 'turn-on-flyspell)

;; Get around the emacswiki spam protection
(add-hook 'oddmuse-mode-hook
          (lambda ()
            (unless (string-match "question" oddmuse-post)
              (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(winner-mode 1)

;; Remove vc-mode from mode-line-format
(setq mode-line-format-without-vc
      (remove-if
       (lambda (elt) (and (listp elt) (equal 'vc-mode (car elt))))
       mode-line-format))

;; Move mode-line-misc-info before mode-line-modes by removing the
;; things at the end and slapping it at the end.
(set-default 'mode-line-format
      (append (remove 'mode-line-modes (remove 'mode-line-end-spaces mode-line-format-without-vc))
              '(mode-line-modes mode-line-end-spaces)))

(which-function-mode t)

(provide 'ethan-misc)
