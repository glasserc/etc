;; Hide some modeline lighters.
;; Some of these can be defined specially -- see for example
;; flyspell-mode-line-string and undo-tree-mode-lighter -- but keep
;; them all here for consistency.
;; I couldn't get this to work with isearch or auto-fill-function, so
;; those I used diminish (see ethan-misc.el).
(eval-after-load 'delight
  '(delight
    '((emacs-lisp-mode "EL" :major)
      (paredit-mode " ()" paredit)
      (eldoc-mode "" eldoc)
      (magit-auto-revert-mode nil magit)
      (js-mode "JS" :major)
      (flyspell-mode "⚜" flyspell) ; looks kinda like a fly
      (yas-minor-mode "" yasnippet)
      (undo-tree-mode " ⎌" undo-tree)
      (rainbow-mode)
      (visual-line-mode nil simple)
      (whole-line-or-region-mode nil whole-line-or-region))))
