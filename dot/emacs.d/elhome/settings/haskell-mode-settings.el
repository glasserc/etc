(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook
   (quote
    (imenu-add-menubar-index turn-on-eldoc-mode turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indentation subword-mode))))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)))

(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; ghc-mod isn't quite ready for prime time, it seems. Plus all its
;; bindings shadow existing keybindings..
;; (let
;;     ((ghc-insert-key "\e\C-t")
;;      (ghc-toggle-key "\C-c\ec")
;;      (ghc-sort-key "\C-c\es")
;;      (ghc-type-key "\C-c\et")
;;      (ghc-info-key "\C-c\ei"))
;;   (ghc-init))
