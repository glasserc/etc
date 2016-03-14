(defun toggle-fullscreen ()
  (interactive)
  ;; TODO: this only works for X. patches welcome for other OSes.
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(use-package sml-modeline
  :config
  (sml-modeline-mode))

;; By default, use-package "loads" or "requires" themes, which causes
;; them to take effect immediately. We don't want that; we just want
;; them to be available for use with load-theme.
(use-package calmer-forest-theme :defer t)
(use-package afternoon-theme :defer t)
(use-package underwater-theme :defer t)
(use-package lush-theme :defer t)
(use-package warm-night-theme :defer t)
(use-package dark-krystal-theme :defer t)

(load-theme 'lush t)

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

(provide 'ethan-appearance)
