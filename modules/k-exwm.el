;; -*- lexical-binding: t -*-

(leaf transparent-frame
  :config
  (defun k/turn-background-transparency-on ()
	"turn background transparency on and text transparency off"
	(interactive)
	(set-frame-parameter nil 'alpha 100)
	(set-frame-parameter nil 'alpha-background 85)
	(setq vertico-posframe-parameters
		  '((alpha . 100)
			(alpha-background . 90)))
	(setq which-key-posframe-parameters
		  '((alpha . 100)
			(alpha-background . 90))))

  (defun k/turn-background-transparency-off ()
	"turn background transparency off and text transparency on"
	(interactive)
	(set-frame-parameter nil 'alpha 100)
	(set-frame-parameter nil 'alpha-background 100)
	(setq vertico-posframe-parameters
		  '((alpha . 100)
			(alpha-background . 90)))
	(setq which-key-posframe-parameters
		  '((parent-frame . nil)
			(alpha . 100)
			(alpha-background . 90))))
  (k/turn-background-transparency-on))

(leaf exwm
  :when isexwm
  :require (xelb compat)
  :ensure t
  :init
  (setq exwm-input-line-mode-passthrough  nil
		mouse-autoselect-window t
		ediff-window-setup-function 'ediff-setup-windows-plain
		focus-follows-mouse t
		exwm-input-prefix-keys '(?\C-x
								 ?\C-u
								 ?\C-h
								 ?\M-x
								 ?\M-t
								 ?\M-`
								 ?\M-&
								 ?\M-:))
  (defun fhd/toggle-exwm-input-line-mode-passthrough ()
	(interactive)
	(if exwm-input-line-mode-passthrough
		(progn
          (setq exwm-input-line-mode-passthrough nil)
          (message "App receives all the keys now (with some simulation)"))
      (progn
		(setq exwm-input-line-mode-passthrough t)
		(message "emacs receives all the keys now"))))

  (setq my/exwm-last-workspaces '(1))

  (defun my/exwm-store-last-workspace ()
	"Save the last workspace to `my/exwm-last-workspaces'."
	(setq my/exwm-last-workspaces
          (seq-uniq (cons exwm-workspace-current-index
                          my/exwm-last-workspaces))))

  (add-hook 'exwm-workspace-switch-hook
			#'my/exwm-store-last-workspace)
  (defun my/exwm-last-workspaces-clear ()
	"Clean `my/exwm-last-workspaces' from deleted workspaces."
	(setq my/exwm-last-workspaces
          (seq-filter
		   (lambda (i) (nth i exwm-workspace--list))
		   my/exwm-last-workspaces)))

  (defun my/fix-exwm-floating-windows ()
	(setq-local exwm-workspace-warp-cursor nil)
	(setq-local mouse-autoselect-window nil)
	(setq-local focus-follows-mouse nil))

  (add-hook 'exwm-floating-setup-hook #'my/fix-exwm-floating-windows)

  (defun efs/exwm-init-hook ()
	(display-battery-mode 1)
	(setq display-time-day-and-date t)
	(display-time-mode 1))

  (defun efs/run-in-background (command)
	(let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun efs/exwm-update-class ()
	(exwm-workspace-rename-buffer exwm-class-name))

  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  (defun efs/exwm-update-title ()
	(pcase exwm-class-name
	  ("firefox" (exwm-workspace-rename-buffer (format "Firefox %s" exwm-title)))
	  ("librewolf" (exwm-workspace-rename-buffer (format "Librewolf %s" exwm-title)))))

  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 17)
  (exwm-systemtray-mode 1)
  (defun k-exwm/C-a ()
	"Pass C-a to the EXWM window."
	(interactive)
	(execute-kbd-macro (kbd "C-q C-a")))
  (defun k-exwm/C-o ()
	"Pass the equivalent of C-o to the EXWM window."
	(interactive)
	(execute-kbd-macro (kbd "<S-return> C-b")))
  (defun k-exwm/M-quote ()
	"double quotes"
	(interactive)
	(execute-kbd-macro (kbd "\" <C-right> \"")))
  (setq exwm-input-global-keys
		`(
          ([?\s-b] . windmove-left)
          ([?\s-f] . windmove-right)
          ([?\s-p] . windmove-up)
          ([?\s-n] . windmove-down)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
          ([?\s-w] . exwm-workspace-switch)
		  ([?\s-m] . (lambda () (interactive) (exwm-layout-toggle-model-line) (exwm-workspace-toggle-minibuffer)))
		  ([?\s-i] . exwm-input-toggle-keyboard)
		  (,(kbd "s-<tab>") . windower-switch-to-last-buffer) ;; Switch to last open buffer in current window
          (,(kbd "s-o") . dmenu) ;; Toggle between multiple windows, and a single window
          (,(kbd "s-O") . windower-toggle-split)  ;; Toggle between vertical and horizontal split. Only works with exactly two windows.
          (,(kbd "s-B") . windower-swap-left)  ;; Swap current window with the window to the left
          (,(kbd "s-N") . windower-swap-below) ;; Swap current window with the window below
          (,(kbd "s-P") . windower-swap-above) ;; Swap current window with the window above
          (,(kbd "s-F") . windower-swap-right) ;; Swap current window with the window to the right
          (,(kbd "s-\\") . exwm-floating-toggle-floating) ;; Toggle the current window between floating and non-floating states
          (,(kbd "s-Q") . exwm-layout-toggle-fullscreen) ;; Toggle fullscreen mode, when in an EXWM window.
          (,(kbd "s-D") . kill-this-buffer)
		  (,(kbd "s-s") . split-and-follow-vertically)
		  (,(kbd "s-'") . fhd/toggle-exwm-input-line-mode-passthrough)
          (,(kbd "s-c") . kill-buffer-and-window)
		  (,(kbd "C-`") . popper-toggle)
		  (,(kbd "C-S-o") . ace-window)
		  (,(kbd "C-S-d") . dirvish-side)
		  (,(kbd "s-," ) . persp-prev)
		  (,(kbd "s-.") . persp-next)
		  (,(kbd "s-[") . perspective-exwm-cycle-exwm-buffers-backward)
		  (,(kbd "s-]") . perspective-exwm-cycle-exwm-buffers-forward)
		  (,(kbd "s-<return>") . eshell-new)
		  (,(kbd "C-c '") . exwm-edit--compose)
		  ,@(mapcar (lambda (i)
					  `(,(kbd (format "s-%d" i)) .
						(lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
					(number-sequence 0 9))))
  (setq exwm-input-simulation-keys
		`(
		  ([?\C-b] . [left])
		  ([?\C-f] . [right])
		  ([?\C-p] . [up])
		  ([?\C-n] . [down])
		  (,(kbd "C-S-b") . [S-left])
		  (,(kbd "C-S-f") . [S-right])
		  (,(kbd "C-S-p") . [S-up])
		  (,(kbd "C-S-n") . [S-down])
		  (,(kbd "C-S-a") . [S-home])
		  (,(kbd "C-S-e") . [S-end])
		  (,(kbd "C-S-<backspace>") . [home S-end delete])
		  ([?\C-a] . [home])
		  ([?\C-e] . [end])
		  ([?\M-v] . [prior])
		  ([?\C-v] . [next])
		  ([?\C-d] . [delete])
		  ([?\C-k] . [S-end delete])
		  ([?\C-m] . [return])
		  ([?\C-i] . [tab])
		  ([?\M-w] . [?\C-c])
		  ([?\C-w] . [?\C-x])
		  ([?\C-y] . [?\C-v])
		  ([?\C-s] . [?\C-f])
		  ([?\C-/] . [?\C-z])
		  ([?\M-f] . [C-right])
		  ([?\M-b] . [C-left])
		  (,(kbd "M-S-b") . [C-S-left])
		  (,(kbd "M-S-f") . [C-S-right])
		  ([?\M-d] . [C-S-right delete])
		  ([?\C-g] . [escape])
		  ([?\M-<] . [home])
		  ([?\M->] . [end])
		  (,(kbd "C-x C-s") . [C-s])))
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP1"))
  (add-hook 'exwm-randr-screen-change-hook
			(lambda ()
              (start-process-shell-command
			   "xrandr" nil "xrandr --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal")))
  (exwm-randr-mode 1)
  (defun efs/configure-window-by-class ()
	(interactive)
	(pcase exwm-class-name
      ("kitty" (exwm-floating-toggle-floating)
	   (exwm-layout-toggle-mode-line))
	  ("Alacritty" (exwm-floating-toggle-floating)
	   (exwm-layout-toggle-mode-line))))
  (add-hook 'exwm-manage-finish-hook
			#'efs/configure-window-by-class)
  ;; Remove ALL bindings
  (define-key exwm-mode-map "\C-c\C-f" nil)
  (define-key exwm-mode-map "\C-c\C-h" nil)
  (define-key exwm-mode-map "\C-c\C-k" nil)
  (define-key exwm-mode-map "\C-c\C-m" nil)
  (define-key exwm-mode-map "\C-c\C-q" nil)
  (define-key exwm-mode-map "\C-c\C-t\C-f" nil)
  (define-key exwm-mode-map "\C-c\C-t\C-m" nil)
  :bind (:exwm-mode-map
		 ("C-q" . #'exwm-input-send-next-key)
		 ("s-i" . #'exwm-input-toggle-keyboard)
		 ("s-e" . #'switch-to-buffer)
		 ("s-r" . #'dmenu)
		 ("s-w" . #'exwm-workspace-switch)
		 ("s-D" . #'kill-this-buffer)
		 ("s-TAB" . #'exwm/jump-to-last-exwm)
		 ("M-t" . #'execute-extended-command)
		 ("M-!" . #'shell-command)
		 ("M-o" . #'ace-window)
		 ("C-<tab>" . #'other-window)
		 ("C-x h" . #'k-exwm/C-a)
		 ("C-o" . #'k-exwm/C-o)
		 ("C-u" . #'universal-argument)
		 ("M-\"" . #'k-exwm/M-quote)
		 ("<XF86AudioLowerVolume>" . #'desktop-environment-volume-decrement)
		 ("<XF86AudioRaiseVolume>" . #'desktop-environment-volume-increment))
  :hook ((exwm-input--input-mode-change-hook . force-modeline-update)))

(leaf exwm-edit
  :elpaca t
  :require t
  :config
  (defun k/on-exwm-edit-compose ()
	(funcall 'org-mode))
  ;; exwm-edit-compose-hook
  (add-hook 'exwm-edit-compose-hook 'k/on-exwm-edit-compose))

;; Desktop-Environment
(leaf desktop-environment
  :elpaca t
  :require t
  :custom
  (desktop-environment-brightness-small-increment . "2%+")
  (desktop-environment-brightness-small-decrement . "2%-")
  (desktop-environment-brightness-normal-increment . "5%+")
  (desktop-environment-brightness-normal-decrement . "5%-")
  (exwm-layout-show-all-buffers . t)
  :init
  (desktop-environment-mode 1)
  :bind (:desktop-environment-mode-map
		 ("s-l" . nil)))

;; Adds to more evil states for EXWM's char and line modes,
;; stole from https://github.com/domenzain/evil-exwm-state/tree/master
;; (leaf evil-exwm-state
;;   :disabled t

;;   :config

;;   (evil-define-state exwm
;; 					 "`exwm state' interfacing exwm mode."
;; 					 :tag " <X> "
;; 					 :enable (motion)
;; 					 :message "-- EXWM --"
;; 					 :intput-method f
;; 					 :entry-hook (evil-exwm-state/enter-exwm))

;;   (evil-define-state exwm-insert
;; 					 "Replace insert state in `exwm state'."
;; 					 :tag " <Xi> "
;; 					 :enable (motion)
;; 					 :message "-- EXWM-INSERT --"
;; 					 :input-method t
;; 					 :entry-hook (evil-exwm-state/enter-exwm-insert))

;;   (defun evil-exwm-state/escape-exwm ()
;; 	"Quit `evil-exwm-insert-state'."
;; 	(interactive)
;; 	(evil-exwm-state))

;;   (defun evil-exwm-state/enter-exwm-insert ()
;; 	"Quit `evil-exwm-insert-state'."
;; 	(call-interactively 'exwm-input-release-keyboard))

;;   (defun evil-exwm-state/enter-exwm ()
;; 	"Quit `evil-exwm-insert-state'."
;; 	(call-interactively 'exwm-input-grab-keyboard))

;;   (define-key evil-exwm-state-map "i" 'evil-exwm-insert-state)

;;   ;; Ensure initial state is char mode / exwm-insert
;;   (setq exwm-manage-configurations '((t char-mode t)))
;;   (evil-set-initial-state 'exwm-mode 'exwm-insert))

(defun k/toggle-fancy-windows ()
  "make vertical monitors more useful"
  (interactive)
  (golden-ratio-mode)
  (window-divider-mode)
  (centered-window-mode))

(leaf exwm-background
  :elpaca (exwm-background :host github :repo "keshav25/exwm-background"))

(leaf exwm-mff
  :require t
  :elpaca t
  :config
  (exwm-mff-mode 1))

(leaf ednc
  :require t
  :elpaca t
  :config
  (defun list-notifications ()
	(mapconcat #'ednc-format-notification (ednc-notifications) ""))
  (defun stack-notifications (&optional hide)
	(mapconcat (lambda (notification)
				 (let ((app-name (ednc-notification-app-name notification)))
                   (unless (member app-name hide)
					 (push app-name hide)
					 (ednc-format-notification notification))))
               (ednc-notifications) ""))
  (nconc global-mode-string '((:eval (list-notifications))))  ; or stack
  (add-hook 'ednc-notification-presentation-functions
			(lambda (&rest _) (force-mode-line-update t))))

(leaf ednc-popup
  :after ednc
  :elpaca (ednc-popup :host git :url "https://codeberg.org/akib/emacs-ednc-popup.git")
  :hook (ednc-notification-presentation-functions . ednc-popup-presentation-function))

(leaf exwm-float
  :require t
  :elpaca t
  :init
  (setq exwm-float-modify-amount '(:move-slow 20 :move-fast 100 :resize 50)
        exwm-float-border '(:stationary ("navy" . 1) :moving ("maroon" . 2)))
  (exwm-float-setup))

(leaf consult-exwm
  :after (consult)
  :config
  (defvar +consult-exwm-filter "\\`\\*exwm")
  (add-to-list 'consult-buffer-filter +consult-exwm-filter)
  (defvar +consult-source-exwm
	`(:name      "EXWM"
				 :narrow    ?x
				 ;; :hidden t
				 :category  buffer
				 :face      consult-buffer
				 :history   buffer-name-history
				 ;; Specify either :action or :state
				 :action    ,#'consult--buffer-action ;; No preview
				 ;; :state  ,#'consult--buffer-state  ;; Preview
				 :items
				 ,(lambda () (consult--buffer-query
							  :sort 'visibility
							  :as #'buffer-name
							  :exclude (remq +consult-exwm-filter consult-buffer-filter)
							  :mode 'exwm-mode)))
	"EXWM buffer source."))

(leaf perspective-exwm
  :after (perspective)
  :elpaca t
  :custom
  (perspective-exwm-override-initial-name . '((0 . "main")))
  :config
  (perspective-exwm-mode 1))

;; (leaf exwm-initialize
;; :when isexwm
;; :after (exwm)
;; :hook (after-init-hook . (lambda () (exwm-wm-mode))))
(elpaca-wait)
(require 'exwm)
(exwm-enable)
(exwm-init)
(provide 'k-exwm)

;; (leaf exwm
;;   :when isexwm
;;   :require exwm-rander
;;   :setq
;;   (exwm-randr-workspace-monitor-plist . '(0 "eDP1"))
;;   (exwm-input-global-keys
;;    .
;;    (setq exwm-input-global-keys
;;        `(
;;          ([?\s-r] . exwm-reset)
;;          ([?\s-h] . windmove-left)
;;          ([?\s-l] . windmove-right)
;;          ([?\s-k] . windmove-up)
;;          ([?\s-j] . windmove-down)
;;         ([?\s-o] . counsel-linux-app)
;;         ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
;;         ([?\s-w] . exwm-workspace-switch)
;;         ,@(mapcar (lambda (i)
;;                       `(,(kbd (format "s-%d" i)) .
;;                         (lambda ()
;;                           (interactive)
;;                           (exwm-workspace-switch-create ,i))))
;;                   (number-sequence 0 9))))
;;    :defun efs/set-wallpaper efs/exwm-init-hook)
;;   :hook
;;   ('exwm-randr-screen-change-hook
;;    .
;;    (lambda ()
;; 	 (start-process-shell-command
;; 	  "xrandr" nil "xrandr --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal")))
;;   :configure
;;   (exwm-randr-enable))

