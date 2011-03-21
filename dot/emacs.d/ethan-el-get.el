;; keep a copy of el-get-install.el somewhere and exec it if this directory doesn't exist
(setq el-get-new nil)
(unless (file-exists-p "~/.emacs.d/el-get/el-get")
  (setq el-get-new t)
  (load "~/.emacs.d/el-get-install.el"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;; It sucks that we can't put these in organize-paths, but I want them
;; to work even in the first call to el-get
(setq bcc-cache-directory (emacs-d "cache/byte-cache"))
(setq url-cookie-file (emacs-d "cache/url/cookies"))

(setq el-get-sources
      '(
         ;; Do this stuff first to init settings and stuff that might be needed
         ;; in installing/initing other packages (specifically, config
         ;; of paths)
         (:name elhome
                :after elhome-init)

         package                   ; this is ELPA
         (:name idle-highlight :type elpa)

         ;; OK, all the other crap
         paredit
         iedit java-mode-indent-annotations
         (:name haml-mode :features nil)  ; autoload's fine, thanks
         (:name whole-line-or-region
                :after whole-line-or-region-mode)
         (:name cheat
                :type git
                :url "https://github.com/emacsmirror/cheat.git")
         (:name oddmuse
                :type git
                :url "https://github.com/emacsmirror/oddmuse.git")
         ;;nxhtml
         (:name rst-mode
                :after (lambda ()
                         (setq auto-mode-alist
                               (cons '("\\.rst$" . rst-mode)
                                     auto-mode-alist))))
         (:name undo-tree
                :load "undo-tree.el"
                :after global-undo-tree-mode)
         (:name yasnippet
                :compile "yasnippet.el"
                :after
                (lambda () (yas/load-directory (emacs-d "my-snippets"))))

;;; This is entirely stolen from Emacs Starter Kit as a good base of
;;; packages to have installed.
              (:name inf-ruby :type elpa)
              ruby-mode yaml-mode gist
              (:name find-file-in-project :type elpa)
              (:name css-mode :type elpa)
              ))

(if (or el-get-new (not (file-exists-p "~/.emacs.d/el-get/elhome")))
    (el-get 'sync)
  (el-get))

;; Workaround for bug in the ELPA package for yaml-mode
(autoload 'yaml-mode "yaml-mode" "" t)

(provide 'ethan-el-get)
