;; -*- lexical-binding: t -*-

;; Load Module Path
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/modules/lang")

(defun k/main-configuration ()
  ;; Leaf
  (load "k-leaf")
  
  ;; Keybindings
  (load  "k-bindings")

  ;; Dashboard
  (load "k-dashboard")

  ;; Misc
  (load "k-misc")

  ;; Themes
  (load "k-themes")

  ;; Window-Management
  (load "k-wm")

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
  ;;(require 'k-hexo)

  ;; Eshell
  (require 'k-eshell)

  ;; EXWM
  (require 'k-exwm)

  ;; Functions
  ;; (require 'k-functions)

  ;; Guix
  ;; (require 'k-guix)

  ;; Programming
  (require 'k-programming)

  ;; Python
  (require 'k-python)

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
  (require 'k-modeline)
  )

(defun k/phone-configuration ()
  ;; Evil
  (require 'k-evil)

  ;; General
  (require 'k-general))

(k/main-configuration)

(if istermux
	(k/phone-configuration))
