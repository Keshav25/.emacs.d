;; Load Module Path
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/modules/lang")

;; (defun k/mod (module)
;;   (require (read (concat "k-" module)))
;;   (setq (read (concat "k/is" module)) 1))

;; (k/mod "leaf")

(defun k/main-configuration ()
  ;; Leaf
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

  ;; Meow
  (require 'k-bindings)

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

(alert "emacs has loaded")
