;;; rst face customizations
; FIXME: ugly colors
(require 'rst)

(set-face-background 'rst-level-1-face "#565656")
(set-face-background 'rst-level-2-face "#4d4d4d")
(set-face-background 'rst-level-3-face "#464646")
(set-face-background 'rst-level-4-face "#3d3d3d")
(set-face-background 'rst-level-5-face "#363636")

(setq auto-mode-alist
      (cons '("\\.rst$" . rst-mode)
            auto-mode-alist))

(provide 'ethan-rst)
