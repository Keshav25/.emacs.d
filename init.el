;; Load Module Path
(add-to-list 'load-path "~/.emacs.d/modules")

;; leaf
(require 'k-leaf)

;; Dashboard
(require 'k-dashboard)

;; Misc
(require 'k-misc)

;; Themes
(require 'k-themes)

;; Window-Management
(require 'k-wm)

;; Completion Framework
(require 'k-emocs)

;; Evil
(require 'k-evil)

;; General
(require 'k-general)

;; Dired
(require 'k-dired)

;; Org-Mode
(require 'k-org)

;; Elfeed
(require 'k-elfeed)

;; Terminals
(require 'k-term)

;; Hexo
(require 'k-hexo)

;; Eshell
(require 'k-eshell)

;; EXWM
(require 'k-exwm)

;; Functions
(require 'k-functions)

;; Guix
(require 'k-guix)

;; Hexo
(require 'k-hexo)

;; Programming
(require 'k-programming)

;; Python
(require 'k-python)

;; Term
(require 'k-term)

;; TEL
;; (leaf TEL
;;    :when istermux
;;    :doc "setup for my TEL configuration"
;;    :setq
;;    (inhibit-startup-screen . t)
;;    :config
;;    (vterm)
;;    (evil-emacs-state))

;; ;; EAF
;; (leaf eaf
;;   :when (or islinux iswindows)
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
;;   :config
;;   (require 'eaf)
;;   (require 'eaf-browser)
;;   (require 'eaf-evil)
;;   (require 'eaf-all-the-icons)
;;   (require 'eaf-2048)
;;   (require 'eaf-airshare)
;;   (require 'eaf-camera)
;;   (require 'eaf-demo)
;;   (require 'eaf-file-browser)
;;   (require 'eaf-file-manager)
;;   (require 'eaf-file-sender)
;;   (require 'eaf-git)
;;   (require 'eaf-image-viewer)
;;   (require 'eaf-jupyter)
;;   (require 'eaf-markdown-previewer)
;;   (require 'eaf-mindmap)
;;   (require 'eaf-music-player)
;;   (require 'eaf-netease-cloud-music)
;;   (require 'eaf-org-previewer)
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-rss-reader)
;;   (require 'eaf-system-monitor)
;;   (require 'eaf-terminal)
;;   (require 'eaf-video-player)
;;   (require 'eaf-vue-demo)
;;   (require 'eaf-vue-mindmap)
;;   :setq
;;   (eaf-browser-search-engines . '(("duckduckgo" . "https://duckduckgo.com/?q=%s")))
;;   (eaf-browser-default-search-engine . "duckduckgo"))
