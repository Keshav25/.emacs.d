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

;; Git
(require 'k-vc)

;; OS
(require 'k-os)

(alert "emacs has loaded")
