(use-package haskell-mode
  :config
  (csetq haskell-compile-cabal-build-command "cd %s && cabal build --ghc-option=-ferror-spans && hlint src/")
  (csetq haskell-indent-spaces 4)
  (csetq haskell-indentation-cycle-warn nil)
  (csetq haskell-indentation-ifte-offset 4)
  (csetq haskell-indentation-layout-offset 4)
  (csetq haskell-indentation-left-offset 4)
  (csetq haskell-indentation-starter-offset 4)
  (csetq haskell-indentation-where-post-offset 2)
  (csetq haskell-indentation-where-pre-offset 2)
  (csetq haskell-mode-hook
         '(imenu-add-menubar-index
           turn-on-eldoc-mode
           turn-on-haskell-decl-scan
           turn-on-haskell-doc
           turn-on-haskell-indentation
           subword-mode))
  (eval-after-load "haskell-cabal"
    '(bind-key "C-c C-c" 'haskell-compile haskell-cabal-mode-map))
;; ghc-mod isn't quite ready for prime time, it seems. Plus all its
;; bindings shadow existing keybindings..
;; (let
;;     ((ghc-insert-key "\e\C-t")
;;      (ghc-toggle-key "\C-c\ec")
;;      (ghc-sort-key "\C-c\es")
;;      (ghc-type-key "\C-c\et")
;;      (ghc-info-key "\C-c\ei"))
;;   (ghc-init))
  :bind
  (:map haskell-mode-map
   ("C-," . haskell-move-nested-left)
   ("C-." . haskell-move-nested-right)
   ("C-c C-c" . haskell-compile)
   ("C-c C-z" . haskell-interactive-switch)
   ("C-c C-l" . haskell-process-load-file)
   ("C-c C-b" . haskell-interactive-switch)
   ("C-c C-t" . haskell-process-do-type)
   ("C-c C-i" . haskell-process-do-info)
   ("M-." . haskell-mode-jump-to-def)))

(provide 'ethan-haskell)
