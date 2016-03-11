(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package rainbow-mode
  :ensure t
  :config
  (defun turn-on-rainbow-mode ()
    (rainbow-mode 1))
  (add-hook 'prog-mode-hook 'turn-on-rainbow-mode)
  ;; FIXME: why does css-mode not inherit from prog-mode???
  (add-hook 'css-mode-hook 'turn-on-rainbow-mode))

(use-package idle-highlight-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

;; keep a copy of el-get-install.el somewhere and exec it if this directory doesn't exist
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-new nil)
(unless (file-exists-p "~/.emacs.d/el-get/el-get")
  (setq el-get-new t)
  (load "~/.emacs.d/bootstrap/el-get-install.el"))

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
                :depends (initsplit)
                :after (elhome-init))

         ;package                   ; this is ELPA

         ;; OK, all the other crap
         paredit
         java-mode-indent-annotations
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
         wgrep

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
                :load "yasnippet.el"
                :after (yas-global-mode))
         (:name java-snippets :type elpa)
         multiple-cursors
         magit
         haskell-mode
         (:name inf-ruby :type elpa)
         ruby-mode yaml-mode gist
         (:name find-file-in-project :type elpa)
         (:name scratch)
         twittering-mode
         (:name delight :type emacswiki :features delight)
         (:name less-css-mode :type elpa)
;              (:name elpy :after (elpy-enable))
         ;; (:name cedet
         ;;        :post-init
         ;;        (load (expand-file-name "cedet-devel-load.el" pdir)))
         ;; (:name malabar-mode :type elpa
         ;;        :repo ("marmalade" . "http://marmalade-repo.org/packages/")
         ;;        :load "malabar-mode.el"
         ;;        :after (add-to-list 'auto-mode-alist '("\\.java" . malabar-mode)))
         ))

;; CEDET/semantic/malabar stuff
;(require 'semantic)
;(require 'cedet)
;(load "semantic/loaddefs.el")
;(semantic-mode 1)


(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files")
(setq my-packages (mapcar 'el-get-source-name el-get-sources))

(if (or el-get-new (not (file-exists-p "~/.emacs.d/el-get/elhome")))
    (el-get 'sync my-packages)
  (el-get my-packages))

(unless (package-installed-p 'elpy)
  (package-install 'elpy))
(elpy-enable)

(setq auto-install-elpa-packages
      '(rich-minority
        calmer-forest-theme
        afternoon-theme
        underwater-theme
        lush-theme
        warm-night-theme
        dark-krystal-theme
        swiper
        counsel
        avy
        ace-window
        git-timemachine))
(dolist (package auto-install-elpa-packages)
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'lush t)
(rich-minority-mode 1)

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
;(semantic-mode 1)
;(require 'malabar-mode)
;(add-to-list 'auto-mode-alist '("\\.java" . malabar-mode))

;; Doesn't cause problems
;; (setq elpy-default-minor-modes (remove 'eldoc-etheteh-mode
;;                                        (remove 'auto-complete-mode
;;                                                (remove 'flymake-mode
;;                                                        (remove 'highlight-indentation-mode
;;                                                                elpy-default-minor-modes)))))


;; Causes problems
;; (setq elpy-default-minor-modes (remove 'eldoc-etheteh-mode
;;                                        (remove 'auto-complete-mode-thethethes
;;                                                (remove 'flymake-mode-etsheszhtes
;;                                                        (remove 'highlight-indentation-mode
;;                                                                elpy-default-minor-modes)))))

;; Doesn't cause problems
;; (setq elpy-default-minor-modes (remove 'eldoc-etheteh-mode
;;                                        (remove 'auto-complete-mode-thethethes
;;                                                (remove 'flymake-mode
;;                                                        (remove 'highlight-indentation-mode
;;                                                                elpy-default-minor-modes)))))
;; (setq elpy-default-minor-modes (remove 'flymake-mode elpy-default-minor-modes))

(provide 'ethan-el-get)
