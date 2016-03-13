;;; rst face customizations
; FIXME: ugly colors
(use-package rst :defer
  :config
  (set-face-background 'rst-level-1 "#565656")
  (set-face-background 'rst-level-2 "#4d4d4d")
  (set-face-background 'rst-level-3 "#464646")
  (set-face-background 'rst-level-4 "#3d3d3d")
  (set-face-background 'rst-level-5 "#363636"))

(provide 'ethan-rst)
