;; Load Module Path
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/modules/lang")

(defun k/main-configuration ()
  ;; Leaf
  (require 'k-leaf)
  
  ;; Keybindings
  (require 'k-bindings)

  ;; Dashboard
  (require 'k-dashboard)

  ;; Misc
  (require 'k-misc)

  ;; Themes
  (require 'k-themes)

  ;; Window-Management
  (require 'k-wm)

  ;; Hydra
  (require 'k-hydra)

  ;; Completion Framework
  (require 'k-emocs)

  ;; Dired
  (require 'k-dired)

  ;; Org-Mode
  (require 'k-org)

  ;; Denote
  (require 'k-denote)
  
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

  ;; Programming
  (require 'k-programming)

  ;; Python
  (require 'k-python)

  ;; PHP
  (org-babel-load-file "~/.emacs.d/modules/lang/k-php.org")
  
  ;; Rust
  (require 'k-rust)

  ;; Go
  (require 'k-go)
  
  ;; Term
  (require 'k-term)

  ;; Git
  (require 'k-vc)

  ;; OS
  (require 'k-os)

  ;; Modeline
  (require 'k-modeline))

(defun k/phone-configuration ()
  ;; Evil
  (require 'k-evil)

  ;; General
  (require 'k-general))

(k/main-configuration)

(if istermux
	(k/phone-configuration))
