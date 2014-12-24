;; Hide some modeline lighters.
;; Some of these can be defined specially -- see for example
;; flyspell-mode-line-string and undo-tree-mode-lighter -- but keep
;; them all here for consistency.
(eval-after-load 'delight
  '(delight
    '((emacs-lisp-mode "EL" :major)
      (js-mode "JS" :major))))
