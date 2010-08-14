;; Emacs-starter-kit recommends espresso, which I know nothing about.
;; There may be other JS mode options here. I can't be bothered right
;; now (until I need to use JS again).
(setq auto-mode-alist
      (cons '("\\.js$" . js2-mode)
            auto-mode-alist))

(provide 'ethan-js)
