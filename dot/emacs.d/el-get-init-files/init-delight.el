;; Hide some modeline lighters.
(eval-after-load 'delight
  '(delight
    '((emacs-lisp-mode "EL" :major)
      (paredit-mode " ()" paredit)
      (eldoc-mode "" eldoc)
      (magit-auto-revert-mode nil magit)
      (js-mode "JS" :major)
      (flyspell-mode " ⚜" flyspell) ; looks kinda like a fly
      (yas-minor-mode " ¥" yasnippet)
      (undo-tree-mode "⎌" undo-tree)
      (rainbow-mode)
      (visual-line-mode nil simple)
      (whole-line-or-region-mode nil whole-line-or-region))))
