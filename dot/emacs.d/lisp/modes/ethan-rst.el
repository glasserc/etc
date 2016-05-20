;;; rst face customizations
; FIXME: ugly colors
(use-package rst :defer
  :config
  ;; FIXME: these are based on whatever theme I happened to be running
  ;; last and they should really be based on some kind of automatic
  ;; thing, maybe using defface as in
  ;; http://emacs.stackexchange.com/questions/9600/how-can-i-override-a-pre-defined-face-for-light-and-dark-backgrounds
  ;; or maybe something even more subtle. But at present there's no
  ;; convenient way to do this. ethan-wspace does this by using a
  ;; window-configuration-change-hook which is probably also wrong. It
  ;; would be cool if there was an emacs library that let you register
  ;; faces that wer based on the current theme background.
  (set-face-background 'rst-level-1 "#c6c6c6")
  (set-face-background 'rst-level-2 "#dddddd")
  (set-face-background 'rst-level-3 "#d6d6d6")
  (set-face-background 'rst-level-4 "#ededed")
  (set-face-background 'rst-level-5 "#e6e6e6"))

(provide 'ethan-rst)
