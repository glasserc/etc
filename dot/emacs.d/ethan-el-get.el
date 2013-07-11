;; keep a copy of el-get-install.el somewhere and exec it if this directory doesn't exist
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-new nil)
(unless (file-exists-p "~/.emacs.d/el-get/el-get")
  (setq el-get-new t)
  (load "~/.emacs.d/el-get-install.el"))

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
        rainbow-mode ;; required by coding-hook which is set up by elhome
         (:name elhome
                :depends (initsplit rainbow-mode)
                :after (elhome-init))

         package                   ; this is ELPA
         (:name idle-highlight :type elpa)

         ;; OK, all the other crap
         paredit
         iedit java-mode-indent-annotations
         (:name sml-modeline
                :after (sml-modeline-mode))
         browse-kill-ring
         (:name haml-mode :features nil)  ; autoload's fine, thanks
         (:name whole-line-or-region
                :features whole-line-or-region
                :after (whole-line-or-region-mode))
         (:name cheat
                :type git
                :url "http://github.com/emacsmirror/cheat.git")
         oddmuse
         (:name javadoc-help
                :type git
                :url "http://github.com/emacsmirror/javadoc-help.git") ruby-electric
         ;; espresso got merged upstream; what about this??
         (:name moz
                :type http
                :url "http://download.savannah.gnu.org/releases-noredirect/espresso/moz.el")

         ;;nxhtml
         jinja2-mode

         (:name rst-mode
                :after (setq auto-mode-alist
                             (cons '("\\.rst$" . rst-mode)
                                   auto-mode-alist)))
         (:name undo-tree
                :type git
                :url "http://www.dr-qubit.org/git/undo-tree.git"
                :load "undo-tree.el"
                :after (global-undo-tree-mode))
         (:name yasnippet
                :compile "yasnippet.el"
                :after
                (yas/load-directory (emacs-d "my-snippets")))
         multiple-cursors
;;; This is entirely stolen from Emacs Starter Kit as a good base of
;;; packages to have installed.
              (:name inf-ruby :type elpa)
              ruby-mode yaml-mode gist
              (:name find-file-in-project :type elpa)
              (:name scratch)
              twittering-mode
              (:name less-css-mode :type elpa)
              ))

(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files")
(setq my-packages (mapcar 'el-get-source-name el-get-sources))

(if (or el-get-new (not (file-exists-p "~/.emacs.d/el-get/elhome")))
    (el-get 'sync my-packages)
  (el-get my-packages))

(provide 'ethan-el-get)
