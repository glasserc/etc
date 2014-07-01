;; Hide some modeline lighters.
(eval-after-load 'delight
  '(delight
    '((emacs-lisp-mode "EL" :major)
      (rainbow-mode)
      (undo-tree-mode " ⎌" undo-tree)
      (paredit-mode " ()" paredit)
      (eldoc-mode "" eldoc)
      (magit-auto-revert-mode nil magit)
      (js-mode "JS" :major)
      (yas-minor-mode " ¥" yasnippet)
      (whole-line-or-region-mode nil whole-line-or-region))))
