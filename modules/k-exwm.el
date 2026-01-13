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
  :after (perspective)
  :require (xelb compat)
  :ensure t
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
								 ?\M-:
								 XF86AudioLowerVolume
								 XF86AudioRaiseVolume
								 XF86PowerOff
								 XF86AudioMute
								 XF86AudioPlay
								 XF86AudioStop
								 XF86AudioPrev
								 XF86AudioNext
								 XF86ScreenSaver
								 XF86Back
								 XF86Forward
								 Scroll_Lock
								 print))
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
	  ("librewolf" (exwm-workspace-rename-buffer (format "Librewolf %s" exwm-title)))
	  (_ (when (and (boundp 'exwm-class-name)
					(boundp 'exwm-title))) (exwm-workspace-rename-buffer (truncate-string-to-width
					(concat exwm-class-name "|" exwm-title) 32)))))

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
  (setopt exwm-input-global-keys
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
			(,(kbd "s-,") . tab-switcher)
			(,(kbd "C-c '") . exwm-edit--compose)
			,@(mapcar (lambda (i)
						`(,(kbd (format "s-%d" i)) .
						  (lambda ()
							(interactive)
							(exwm-workspace-switch-create ,i))))
					  (number-sequence 0 9))))
  (setopt exwm-input-simulation-keys
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
  (setopt exwm-manage-force-tiling t)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP1"))
  (add-hook 'exwm-randr-screen-change-hook
			(lambda ()
			  (start-process-shell-command
			   "xrandr" nil "xrandr --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal")))
  (exwm-randr-mode 1)

  ;; (defun efs/configure-window-by-class ()
  ;; 	(interactive)
  ;; 	(pcase exwm-class-name
  ;; 	  ("kitty" (exwm-floating-toggle-floating)
  ;; 	   (exwm-layout-toggle-mode-line))
  ;; 	  ("Alacritty" (exwm-floating-toggle-floating)
  ;; 	   (exwm-layout-toggle-mode-line))))

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
  (defun my/exwm-direction-exists-p (dir)
	"Check if there is space in the direction DIR.

Does not take the minibuffer into account."
	(cl-some (lambda (dir)
               (let ((win (windmove-find-other-window dir)))
				 (and win (not (window-minibuffer-p win)))))
			 (pcase dir
               ('width '(left right))
               ('height '(up down)))))

  (defun my/exwm-move-window (dir)
	"Move the current window in the direction DIR."
	(let ((other-window (windmove-find-other-window dir))
          (other-direction (my/exwm-direction-exists-p
							(pcase dir
                              ('up 'width)
                              ('down 'width)
                              ('left 'height)
                              ('right 'height)))))
      (cond
       ((and other-window (not (window-minibuffer-p other-window)))
		(window-swap-states (selected-window) other-window))
       (other-direction
		(evil-move-window dir)))))

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

;;; Nested Window Manager (via Xephyr)
;; Run another WM inside EXWM using a nested X server

(defgroup k-exwm-nested nil
  "Nested window manager support via Xephyr."
  :group 'exwm)

(defcustom k-exwm-nested-size "1280x720"
  "Default size for Xephyr window (WIDTHxHEIGHT)."
  :type 'string
  :group 'k-exwm-nested)

(defcustom k-exwm-nested-default-wm "dwm"
  "Default window manager to run in Xephyr."
  :type 'string
  :group 'k-exwm-nested)

(defvar k-exwm-nested--sessions nil
  "Alist of active nested sessions: ((display . (xephyr-proc . wm-proc)) ...).")

(defvar k-exwm-nested--next-display 10
  "Next display number to use for Xephyr.")

(defun k-exwm-nested--find-free-display ()
  "Find a free display number for Xephyr."
  (let ((display k-exwm-nested--next-display))
    (while (file-exists-p (format "/tmp/.X%d-lock" display))
      (setq display (1+ display)))
    (setq k-exwm-nested--next-display (1+ display))
    display))

(defun k-exwm-nested-start (wm &optional size)
  "Start a nested X session with WM inside Xephyr.
SIZE is optional WIDTHxHEIGHT string."
  (interactive
   (list (read-string "Window manager: " k-exwm-nested-default-wm)
         (when current-prefix-arg
           (read-string "Size (WxH): " k-exwm-nested-size))))
  (let* ((display (k-exwm-nested--find-free-display))
         (display-str (format ":%d" display))
         (size (or size k-exwm-nested-size))
         (xephyr-proc nil)
         (wm-proc nil))
    ;; Start Xephyr
    (setq xephyr-proc
          (start-process
           (format "xephyr-%d" display)
           nil
           "Xephyr"
           display-str
           "-screen" size
           "-resizeable"
           "-title" (format "Nested: %s" wm)
           "-name" (format "nested-%s" wm)))
    ;; Wait for Xephyr to start
    (sleep-for 0.5)
    ;; Start the WM inside Xephyr
    (let ((process-environment
           (cons (format "DISPLAY=%s" display-str)
                 process-environment)))
      (setq wm-proc
            (start-process
             (format "nested-wm-%d" display)
             nil
             wm)))
    ;; Track session
    (push (cons display (cons xephyr-proc wm-proc)) k-exwm-nested--sessions)
    ;; Clean up when Xephyr exits
    (set-process-sentinel
     xephyr-proc
     (lambda (proc _event)
       (when (not (process-live-p proc))
         (setq k-exwm-nested--sessions
               (assq-delete-all display k-exwm-nested--sessions))
         (message "Nested session :%d closed" display))))
    (message "Started %s on display %s (%s)" wm display-str size)))

(defun k-exwm-nested-run (command)
  "Run COMMAND inside the most recent nested Xephyr session."
  (interactive "sCommand to run in nested session: ")
  (if k-exwm-nested--sessions
      (let* ((session (car k-exwm-nested--sessions))
             (display (car session))
             (display-str (format ":%d" display)))
        (start-process-shell-command
         "nested-cmd" nil
         (format "DISPLAY=%s %s" display-str command))
        (message "Running '%s' on display %s" command display-str))
    (message "No active nested session")))

(defun k-exwm-nested-list ()
  "List active nested sessions."
  (interactive)
  (if k-exwm-nested--sessions
      (message "Active nested sessions: %s"
               (mapconcat (lambda (s) (format ":%d" (car s)))
                          k-exwm-nested--sessions ", "))
    (message "No active nested sessions")))

(defun k-exwm-nested-stop (display)
  "Stop nested session on DISPLAY."
  (interactive
   (list (if k-exwm-nested--sessions
             (string-to-number
              (completing-read "Stop display: "
                               (mapcar (lambda (s) (format "%d" (car s)))
                                       k-exwm-nested--sessions)
                               nil t))
           (user-error "No active nested sessions"))))
  (when-let ((session (assq display k-exwm-nested--sessions)))
    (let ((xephyr-proc (cadr session))
          (wm-proc (cddr session)))
      (when (process-live-p wm-proc) (kill-process wm-proc))
      (when (process-live-p xephyr-proc) (kill-process xephyr-proc)))
    (setq k-exwm-nested--sessions
          (assq-delete-all display k-exwm-nested--sessions))
    (message "Stopped nested session :%d" display)))

(defun k-exwm-nested-stop-all ()
  "Stop all nested sessions."
  (interactive)
  (dolist (session k-exwm-nested--sessions)
    (k-exwm-nested-stop (car session)))
  (message "All nested sessions stopped"))

;;; Smart Gaps for EXWM Windows
;; Adds configurable gaps between tiled X windows with focus indication.

(defgroup k-exwm-gaps nil
  "Smart gaps between EXWM windows."
  :group 'exwm)

(defcustom k-exwm-gaps-inner-gap 20
  "Gap size in pixels between adjacent windows."
  :type 'integer
  :group 'k-exwm-gaps)

(defcustom k-exwm-gaps-outer-gap 10
  "Gap size in pixels at frame edges."
  :type 'integer
  :group 'k-exwm-gaps)

(defcustom k-exwm-gaps-top-gap t
  "When non-nil, add outer gap at top edge of topmost windows."
  :type 'boolean
  :group 'k-exwm-gaps)

(defcustom k-exwm-gaps-bottom-gap t
  "When non-nil, add outer gap at bottom edge of bottommost windows."
  :type 'boolean
  :group 'k-exwm-gaps)

(defcustom k-exwm-gaps-hide-mode-line t
  "When non-nil, hide mode-line on EXWM windows.
Works well with `ace-window' overlays (set `aw-display-mode-line' to nil)."
  :type 'boolean
  :group 'k-exwm-gaps)

(defcustom k-exwm-gaps-focus-border-color "#61afef"
  "Color for the border around the focused EXWM window.
The border appears in the gap area. Set to nil to disable."
  :type '(choice (string :tag "Color")
                 (const :tag "Disabled" nil))
  :group 'k-exwm-gaps)

(defvar-local k-exwm-gaps--bg-cookie nil
  "Cookie for buffer-local background face remapping.")

(defvar k-exwm-gaps--saved-aw-display-mode-line nil
  "Saved value of `aw-display-mode-line' before enabling gaps.")

(defvar k-exwm-gaps--mode-lines-hidden nil
  "Non-nil if mode-lines were hidden when gaps mode was enabled.
Used to track state for proper cleanup on disable.")

(defun k-exwm-gaps--neighbor-p (window dir)
  "Return non-nil if WINDOW has a non-minibuffer neighbor in DIR."
  (when-let ((w (window-in-direction dir window)))
    (not (window-minibuffer-p w))))

(defun k-exwm-gaps--calculate-geometry (x y width height window)
  "Calculate adjusted geometry with smart gaps for WINDOW.
Returns (X Y WIDTH HEIGHT) with gaps applied based on neighbors."
  (let* ((half-inner (/ k-exwm-gaps-inner-gap 2))
         (outer k-exwm-gaps-outer-gap)
         (has-left (k-exwm-gaps--neighbor-p window 'left))
         (has-right (k-exwm-gaps--neighbor-p window 'right))
         (has-above (k-exwm-gaps--neighbor-p window 'above))
         (has-below (k-exwm-gaps--neighbor-p window 'below))
         (left-gap (if has-left half-inner outer))
         (right-gap (if has-right half-inner outer))
         (top-gap (cond (has-above half-inner)
                        (k-exwm-gaps-top-gap outer)
                        (t 0)))
         (bottom-gap (cond (has-below half-inner)
                           (k-exwm-gaps-bottom-gap outer)
                           (t 0))))
    (list (+ x left-gap)
          (+ y top-gap)
          (- width left-gap right-gap)
          (- height top-gap bottom-gap))))

(defun k-exwm-gaps--show-advice (orig-fun id &optional window)
  "Advice for `exwm-layout--show' to apply gaps to tiled windows."
  (if (or (not k-exwm-gaps-mode)
          (not window)
          (not (buffer-live-p (exwm--id->buffer id))))
      (funcall orig-fun id window)
    (with-current-buffer (exwm--id->buffer id)
      (if (or exwm--floating-frame
              (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state))
          (funcall orig-fun id window)
        (let* ((edges (window-inside-absolute-pixel-edges window))
               (x (nth 0 edges))
               (y (nth 1 edges))
               (w (- (nth 2 edges) x))
               (h (- (nth 3 edges) y))
               (geom (k-exwm-gaps--calculate-geometry x y w h window)))
          (exwm--set-geometry id (nth 0 geom) (nth 1 geom) (nth 2 geom) (nth 3 geom))
          (xcb:+request exwm--connection
              (make-instance 'xcb:MapWindow :window id))
          (exwm-layout--set-state id xcb:icccm:WM_STATE:NormalState)
          (setq exwm--ewmh-state
                (delq xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state))
          (exwm-layout--set-ewmh-state id)
          (xcb:flush exwm--connection))))))

(defun k-exwm-gaps--set-border (buffer color)
  "Set BUFFER's background to COLOR for focus indication."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'exwm-mode)
        (when k-exwm-gaps--bg-cookie
          (ignore-errors (face-remap-remove-relative k-exwm-gaps--bg-cookie))
          (setq k-exwm-gaps--bg-cookie nil))
        (when color
          (setq k-exwm-gaps--bg-cookie
                (face-remap-add-relative 'default :background color)))))))

(defun k-exwm-gaps--update-focus (&optional _frame)
  "Update focus border colors for all EXWM windows."
  (when (and k-exwm-gaps-mode k-exwm-gaps-focus-border-color)
    (let ((focused-buf (window-buffer (selected-window))))
      (dolist (buf (buffer-list))
        (when (and (buffer-live-p buf)
                   (with-current-buffer buf (derived-mode-p 'exwm-mode)))
          (k-exwm-gaps--set-border
           buf (when (eq buf focused-buf) k-exwm-gaps-focus-border-color)))))))

(defun k-exwm-gaps--hide-modeline ()
  "Hide mode-line on new EXWM buffer."
  (exwm-layout-hide-mode-line))

(defun k-exwm-gaps-set (size)
  "Set inner gap SIZE interactively."
  (interactive "nGap size (pixels): ")
  (setq k-exwm-gaps-inner-gap (max 0 size))
  (when k-exwm-gaps-mode
    (k-exwm-gaps--refresh-layout))
  (message "EXWM gaps: %dpx" size))

(defun k-exwm-gaps-increase ()
  "Increase inner gap by 4 pixels."
  (interactive)
  (k-exwm-gaps-set (max 0  (+ k-exwm-gaps-inner-gap 4))))

(defun k-exwm-gaps-decrease ()
  "Decrease inner gap by 4 pixels."
  (interactive)
  (k-exwm-gaps-set (max 0 (- k-exwm-gaps-inner-gap 4))))

(defun k-exwm-gaps--restore-modelines ()
  "Restore mode-lines on all EXWM buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'exwm-mode)
          (exwm-layout-show-mode-line))))))

(defun k-exwm-gaps--refresh-layout ()
  "Refresh all EXWM window layouts to apply/remove gaps."
  (when (fboundp 'exwm-layout--refresh)
    (dolist (frame (frame-list))
      (when (frame-live-p frame)
        (with-selected-frame frame
          (ignore-errors (exwm-layout--refresh)))))))

;;;###autoload
(define-minor-mode k-exwm-gaps-mode
  "Toggle smart gaps between EXWM windows.
When enabled, tiled X windows are shrunk to create transparent gaps.
The focused window gets a colored border visible in the gap area.

Requires a compositor (picom) with transparency for best results.
Pairs well with picom's corner-radius for rounded window corners."
  :global t
  :group 'k-exwm-gaps
  (if k-exwm-gaps-mode
      (progn
        ;; Save state before modifying (only if not already saved)
        (when (and (boundp 'aw-display-mode-line)
                   (not k-exwm-gaps--mode-lines-hidden))
          (setq k-exwm-gaps--saved-aw-display-mode-line aw-display-mode-line))
        ;; Enable gaps
        (advice-add 'exwm-layout--show :around #'k-exwm-gaps--show-advice)
        (when k-exwm-gaps-hide-mode-line
          (setq k-exwm-gaps--mode-lines-hidden t)  ; Track that we hid mode-lines
          (add-hook 'exwm-manage-finish-hook #'k-exwm-gaps--hide-modeline)
          (when (boundp 'aw-display-mode-line)
            (setq aw-display-mode-line nil))
          ;; Hide mode-lines on existing EXWM buffers
          (dolist (buf (buffer-list))
            (when (and (buffer-live-p buf)
                       (with-current-buffer buf (derived-mode-p 'exwm-mode)))
              (with-current-buffer buf
                (exwm-layout-hide-mode-line)))))
        (when k-exwm-gaps-focus-border-color
          (add-hook 'window-selection-change-functions #'k-exwm-gaps--update-focus)
          (k-exwm-gaps--update-focus))
        ;; Refresh to apply gaps
        (k-exwm-gaps--refresh-layout)
        (message "EXWM gaps enabled (inner=%dpx outer=%dpx)"
                 k-exwm-gaps-inner-gap k-exwm-gaps-outer-gap))
    ;; Disable gaps
    (advice-remove 'exwm-layout--show #'k-exwm-gaps--show-advice)
    (remove-hook 'exwm-manage-finish-hook #'k-exwm-gaps--hide-modeline)
    (remove-hook 'window-selection-change-functions #'k-exwm-gaps--update-focus)
    ;; Clear all borders
    (dolist (buf (buffer-list))
      (k-exwm-gaps--set-border buf nil))
    ;; Restore mode-lines (based on whether we hid them, not current config)
    (when k-exwm-gaps--mode-lines-hidden
      (k-exwm-gaps--restore-modelines)
      (setq k-exwm-gaps--mode-lines-hidden nil))
    ;; Restore aw-display-mode-line
    (when (boundp 'aw-display-mode-line)
      (setq aw-display-mode-line k-exwm-gaps--saved-aw-display-mode-line))
    ;; Refresh to remove gaps
    (k-exwm-gaps--refresh-layout)
    (message "EXWM gaps disabled")))

;;; Initialize EXWM
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

