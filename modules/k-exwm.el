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
  :custom
  (exwm-layout-fullscreen-grab-keyboard . nil)
  (exwm-layout-show-all-buffers . t)
  :bind (:exwm-mode-map
		 ("C-q" . #'exwm-input-send-next-key)
		 ("s-i" . #'exwm-input-toggle-keyboard)
		 ("s-e" . #'switch-to-buffer)
		 ("s-r" . #'dmenu)
		 ("s-w" . #'exwm-workspace-switch)
		 ("s-D" . #'kill-this-buffer)
		 ("s-TAB" . #'k/exwm-jump-to-last-exwm)
		 ("M-t" . #'execute-extended-command)
		 ("M-!" . #'shell-command)
		 ("M-o" . #'ace-window)
		 ("C-<tab>" . #'other-window)
		 ("C-x h" . #'k-exwm/C-a)
		 ("C-o" . #'k-exwm/C-o)
		 ("C-u" . #'universal-argument)
		 ("C-y" . #'k/exwm-paste-from-kill-ring)
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

  (add-hook 'exwm-workspace-list-change-hook #'my/exwm-last-workspaces-clear)

  (defun my/fix-exwm-floating-windows ()
	(setq-local exwm-workspace-warp-cursor nil)
	(setq-local mouse-autoselect-window nil)
	(setq-local focus-follows-mouse nil))

  (add-hook 'exwm-floating-setup-hook #'my/fix-exwm-floating-windows)

  (defvar k/exwm--last-exwm-buffer nil
	"Last visited EXWM buffer, for quick switching.")

  (defun k/exwm--track-last-exwm-buffer ()
	"Track the last EXWM buffer for quick-switch."
	(when (derived-mode-p 'exwm-mode)
	  (setq k/exwm--last-exwm-buffer (current-buffer))))

  (add-hook 'buffer-list-update-hook #'k/exwm--track-last-exwm-buffer)

  (defun k/exwm-jump-to-last-exwm ()
	"Jump to the last visited EXWM buffer."
	(interactive)
	(if (and k/exwm--last-exwm-buffer
			 (buffer-live-p k/exwm--last-exwm-buffer)
			 (not (eq k/exwm--last-exwm-buffer (current-buffer))))
		(switch-to-buffer k/exwm--last-exwm-buffer)
	  ;; Fallback: find any other EXWM buffer
	  (let ((exwm-bufs (cl-remove-if-not
						 (lambda (buf)
						   (and (not (eq buf (current-buffer)))
								(with-current-buffer buf
								  (derived-mode-p 'exwm-mode))))
						 (buffer-list))))
		(if exwm-bufs
			(switch-to-buffer (car exwm-bufs))
		  (message "No other EXWM buffer found")))))

  (defun efs/exwm-init-hook ()
	(display-battery-mode 1)
	(setq display-time-day-and-date t)
	(display-time-mode 1))

  (defun efs/run-in-background (command)
	(let ((command-parts (split-string command "[ ]+")))
	  (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  ;; --- Smart buffer naming ---
  (defvar k/exwm-class-name-map
	'(("firefox" . "Firefox")
	  ("librewolf" . "Librewolf")
	  ("Brave-browser" . "Brave")
	  ("Google-chrome" . "Chrome")
	  ("Chromium-browser" . "Chromium")
	  ("kitty" . "Kitty")
	  ("Alacritty" . "Alacritty")
	  ("mpv" . "MPV")
	  ("Gimp-2.10" . "GIMP")
	  ("Gimp" . "GIMP")
	  ("Inkscape" . "Inkscape")
	  ("libreoffice" . "LibreOffice")
	  ("Thunar" . "Thunar")
	  ("Nautilus" . "Files")
	  ("discord" . "Discord")
	  ("Slack" . "Slack")
	  ("Spotify" . "Spotify")
	  ("steam" . "Steam")
	  ("obs" . "OBS")
	  ("VirtualBox Manager" . "VBox")
	  ("Virt-manager" . "VirtMgr"))
	"Map of exwm-class-name to human-readable short names.")

  (defun k/exwm-buffer-name (class title)
	"Build a clean buffer name from CLASS and TITLE."
	(let* ((short-class (or (cdr (assoc class k/exwm-class-name-map)) class))
		   (clean-title (if (and title (not (string-empty-p title)))
							 (string-trim title)
						   ""))
		   ;; Avoid duplicating the class name in the title
		   (display-title (if (string-prefix-p short-class clean-title)
							   (string-trim (substring clean-title (length short-class)))
							 clean-title)))
	  (if (string-empty-p display-title)
		  short-class
		(truncate-string-to-width
		 (format "%s: %s" short-class display-title) 72))))

  (defun efs/exwm-update-class ()
	(exwm-workspace-rename-buffer (k/exwm-buffer-name exwm-class-name "")))

  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  (defun efs/exwm-update-title ()
	(exwm-workspace-rename-buffer (k/exwm-buffer-name exwm-class-name exwm-title)))

  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 17)
  (exwm-systemtray-mode 1)
  (defun k-exwm/C-a ()
	"Send C-a (select all) to the EXWM window."
	(interactive)
	(exwm-input--fake-key ?\C-a))
  (defun k-exwm/C-o ()
	"Open line below: send Shift-Return then move back in the EXWM window."
	(interactive)
	(exwm-input--fake-key 'S-return)
	(run-at-time 0.05 nil (lambda () (exwm-input--fake-key 'left))))
  (defun k-exwm/M-quote ()
	"Insert double quotes around word at point in the EXWM window."
	(interactive)
	(exwm-input--fake-key ?\")
	(exwm-input--fake-key 'C-right)
	(exwm-input--fake-key ?\"))
  (setopt exwm-input-global-keys
		  `(
			([?\s-b] . windmove-left)
			([?\s-f] . windmove-right)
			([?\s-p] . windmove-up)
			([?\s-n] . windmove-down)
			([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
			([?\s-w] . exwm-workspace-switch)
			([?\s-m] . (lambda () (interactive) (exwm-layout-toggle-mode-line) (exwm-workspace-toggle-minibuffer)))
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
			(,(kbd "s-s") . k/split-and-follow-below)
			(,(kbd "s-v") . k/split-and-follow-right)
			(,(kbd "s-'") . fhd/toggle-exwm-input-line-mode-passthrough)
			(,(kbd "s-c") . kill-buffer-and-window)
			(,(kbd "C-`") . popper-toggle)
			(,(kbd "C-S-o") . ace-window)
			(,(kbd "C-S-d") . dirvish-side)
			(,(kbd "s-,") . persp-prev)
			(,(kbd "s-.") . persp-next)
			(,(kbd "s-[") . perspective-exwm-cycle-exwm-buffers-backward)
			(,(kbd "s-]") . perspective-exwm-cycle-exwm-buffers-forward)
			(,(kbd "s-<return>") . eshell-new)
			(,(kbd "s-/") . tab-switcher)
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

  ;; --- Multi-monitor auto-detection ---
  (defvar k/exwm-monitors nil
	"List of currently connected monitor output names.")

  (defvar k/exwm-primary-monitor nil
	"The primary monitor output name, or the first connected one.")

  (defun k/exwm--parse-monitors ()
	"Parse xrandr output and return an alist of (OUTPUT . PLIST).
Each plist has :connected, :primary, :resolution."
	(let ((output (shell-command-to-string "xrandr --query"))
		  (monitors '()))
	  (dolist (line (split-string output "\n" t))
		(when (string-match
			   "^\\([^ ]+\\) connected \\(primary \\)?\\([0-9]+x[0-9]+\\)?\\+?\\([0-9]+\\)?\\+?\\([0-9]+\\)?"
			   line)
		  (let ((name (match-string 1 line))
				(primary (match-string 2 line))
				(res (match-string 3 line)))
			(push (list name
						:connected t
						:primary (not (null primary))
						:resolution res)
				  monitors))))
	  (nreverse monitors)))

  (defun k/exwm-detect-monitors ()
	"Auto-detect connected monitors and configure EXWM workspaces.
Assigns workspace 0 to the primary monitor.  Additional monitors get
higher-numbered workspaces.  Updates `exwm-randr-workspace-monitor-plist'."
	(interactive)
	(let* ((parsed (k/exwm--parse-monitors))
		   (names (mapcar #'car parsed))
		   (primary (or (car (cl-find-if
							  (lambda (m) (plist-get (cdr m) :primary))
							  parsed))
					    (car names))))
	  (setq k/exwm-monitors names
			k/exwm-primary-monitor primary)
	  (when names
		(let ((plist '())
			  (num-monitors (length names)))
		  ;; Round-robin assign workspaces, primary first
		  (let ((sorted (cons primary (remove primary names))))
			(dotimes (i 10)
			  (setq plist (append plist
								  (list i (nth (mod i num-monitors) sorted))))))
		  (setq exwm-randr-workspace-monitor-plist plist))
		(message "EXWM monitors detected: %s (primary: %s)"
				 (string-join names ", ") primary))))

  (defun k/exwm-show-monitors ()
	"Display current monitor configuration."
	(interactive)
	(let ((parsed (k/exwm--parse-monitors)))
	  (message "Monitors: %s"
			   (mapconcat
				(lambda (m)
				  (format "%s%s %s"
						  (car m)
						  (if (plist-get (cdr m) :primary) " [PRIMARY]" "")
						  (or (plist-get (cdr m) :resolution) "unknown")))
				parsed ", "))))

  (k/exwm-detect-monitors)
  (add-hook 'exwm-randr-screen-change-hook #'k/exwm-detect-monitors)
  (exwm-randr-mode 1)

  ;; --- Smart window placement rules ---
  (defvar k/exwm-floating-classes
	'("kitty" "Alacritty" "Pavucontrol" "Nm-connection-editor"
	  "Arandr" "Blueman-manager" "Lxappearance" "Xfce4-settings-manager"
	  "feh" "Sxiv" "Nsxiv" "mpv")
	"Window classes that should always float.")

  (defvar k/exwm-workspace-rules
	'(("firefox" . 2)
	  ("librewolf" . 2)
	  ("Brave-browser" . 2)
	  ("Google-chrome" . 2)
	  ("discord" . 3)
	  ("Slack" . 3)
	  ("Spotify" . 4)
	  ("steam" . 5))
	"Alist mapping window class to preferred workspace number.")

  (defun k/exwm-configure-window ()
	"Configure new EXWM windows: apply floating, workspace, and mode-line rules."
	(interactive)
	(let ((class (or exwm-class-name "")))
	  ;; Float certain classes
	  (when (member class k/exwm-floating-classes)
		(exwm-floating-toggle-floating)
		(exwm-layout-hide-mode-line))
	  ;; Send to preferred workspace
	  (when-let ((ws (cdr (assoc class k/exwm-workspace-rules))))
		(exwm-workspace-move-window ws))))

  (add-hook 'exwm-manage-finish-hook #'k/exwm-configure-window)

  (defun k/exwm-add-floating-class (class)
	"Interactively add CLASS to the floating window list."
	(interactive
	 (list (completing-read "Float class: "
							(cl-remove-duplicates
							 (mapcar (lambda (buf)
									   (with-current-buffer buf
										 (when (derived-mode-p 'exwm-mode) exwm-class-name)))
									 (buffer-list))
							 :test #'equal)
							nil nil)))
	(add-to-list 'k/exwm-floating-classes class)
	(message "Added '%s' to floating classes" class))
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
		(windmove-swap-states-in-direction dir)))))

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
  (desktop-environment-volume-set-command . "pactl set-sink-volume 0 %s")
  (desktop-environment-volume-get-command . "pactl get-sink-volume 0 | awk '{print $5}'")
  (desktop-environment-volume-normal-increment . "+5%")
  (desktop-environment-volume-normal-decrement . "-5%")
  (desktop-environment-volume-small-increment . "+1%")
  (desktop-environment-volume-small-decrement . "-1%")
  :init
  (desktop-environment-mode 1)
  :bind (:desktop-environment-mode-map
		 ("s-l" . nil)))

(defun k/toggle-fancy-windows ()
  "make vertical monitors more useful"
  (interactive)
  (golden-ratio-mode)
  (window-divider-mode)
  (centered-window-mode))

;;; Screenshot integration
;; Uses maim (region/window/full) and xclip (clipboard).
;; Screenshots saved to ~/Pictures/Screenshots/ with timestamps.

(defgroup k-exwm-screenshot nil
  "Screenshot support for EXWM."
  :group 'exwm)

(defcustom k-exwm-screenshot-directory "~/Pictures/Screenshots/"
  "Directory for saving screenshots."
  :type 'directory
  :group 'k-exwm-screenshot)

(defcustom k-exwm-screenshot-format "png"
  "Image format for screenshots (png, jpg, webp)."
  :type 'string
  :group 'k-exwm-screenshot)

(defun k/exwm-screenshot--ensure-dir ()
  "Ensure screenshot directory exists."
  (let ((dir (expand-file-name k-exwm-screenshot-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun k/exwm-screenshot--filename (&optional suffix)
  "Generate a timestamped screenshot filename."
  (expand-file-name
   (format "screenshot_%s%s.%s"
           (format-time-string "%Y-%m-%d_%H-%M-%S")
           (or suffix "")
           k-exwm-screenshot-format)
   (k/exwm-screenshot--ensure-dir)))

(defun k/exwm-screenshot--notify (file)
  "Show message and optionally open the screenshot FILE."
  (message "Screenshot saved: %s" file)
  (when (y-or-n-p "Open screenshot? ")
    (find-file file)))

(defun k/exwm-screenshot-full ()
  "Take a full-screen screenshot."
  (interactive)
  (let ((file (k/exwm-screenshot--filename "_full")))
    (call-process "maim" nil nil nil
                  "--format" k-exwm-screenshot-format
                  file)
    (k/exwm-screenshot--notify file)))

(defun k/exwm-screenshot-region ()
  "Take a screenshot of a selected region."
  (interactive)
  (let ((file (k/exwm-screenshot--filename "_region")))
    (message "Select a region...")
    (start-process "maim-region" nil "maim"
                   "--select"
                   "--format" k-exwm-screenshot-format
                   file)
    (set-process-sentinel
     (get-process "maim-region")
     (lambda (proc _event)
       (when (= 0 (process-exit-status proc))
         (k/exwm-screenshot--notify file))))))

(defun k/exwm-screenshot-window ()
  "Take a screenshot of the current window."
  (interactive)
  (let ((file (k/exwm-screenshot--filename "_window")))
    (if (and (derived-mode-p 'exwm-mode)
             (boundp 'exwm--id) exwm--id)
        (call-process "maim" nil nil nil
                      "--window" (format "%d" exwm--id)
                      "--format" k-exwm-screenshot-format
                      file)
      ;; Fallback: use focused window via xdotool
      (call-process-shell-command
       (format "maim --window $(xdotool getactivewindow) --format %s %s"
               k-exwm-screenshot-format
               (shell-quote-argument file))))
    (k/exwm-screenshot--notify file)))

(defun k/exwm-screenshot-to-clipboard ()
  "Take a region screenshot and copy it to the clipboard."
  (interactive)
  (message "Select a region to copy to clipboard...")
  (start-process-shell-command
   "maim-clip" nil
   "maim --select | xclip -selection clipboard -t image/png")
  (set-process-sentinel
   (get-process "maim-clip")
   (lambda (proc _event)
     (when (= 0 (process-exit-status proc))
       (message "Screenshot copied to clipboard")))))

(defun k/exwm-screenshot-to-org ()
  "Take a region screenshot and insert an org link at point."
  (interactive)
  (let ((file (k/exwm-screenshot--filename "_org")))
    (message "Select a region for org capture...")
    (start-process "maim-org" nil "maim"
                   "--select"
                   "--format" k-exwm-screenshot-format
                   file)
    (set-process-sentinel
     (get-process "maim-org")
     (lambda (proc _event)
       (when (= 0 (process-exit-status proc))
         (insert (format "[[file:%s]]" file))
         (message "Org link inserted for %s" file))))))

;; Global keybindings for screenshots (available everywhere)
(global-set-key (kbd "<print>") #'k/exwm-screenshot-full)
(global-set-key (kbd "S-<print>") #'k/exwm-screenshot-region)
(global-set-key (kbd "C-<print>") #'k/exwm-screenshot-window)
(global-set-key (kbd "M-<print>") #'k/exwm-screenshot-to-clipboard)

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
	"EXWM buffer source.")

  (defun k/exwm-paste-from-kill-ring ()
    "Browse the kill ring with consult and paste into the EXWM application.
Uses the consult completion UI to select a kill ring entry, copies
it to the system clipboard, and simulates C-v (paste) in the
focused X application.  Falls back to `consult-yank-pop' in
non-EXWM buffers."
    (interactive)
    (if (derived-mode-p 'exwm-mode)
        (let ((text (consult--read
                     (consult--remove-dups
                      (mapcar #'substring-no-properties kill-ring))
                     :prompt "Paste from kill ring: "
                     :history t
                     :category 'kill-ring
                     :require-match t
                     :sort nil)))
          (when (and text (not (string-empty-p text)))
            ;; Update kill ring so the selection is current
            (kill-new text)
            ;; Copy to both CLIPBOARD and PRIMARY selections
            (gui-set-selection 'CLIPBOARD text)
            (gui-set-selection 'PRIMARY text)
            ;; Give the clipboard a moment to propagate, then paste
            (run-at-time 0.05 nil
                         (lambda ()
                           (exwm-input--fake-key ?\C-v)))))
      (call-interactively #'consult-yank-pop)))

  ;; Also make M-x consult-yank-from-kill-ring work in EXWM buffers
  (advice-add 'consult-yank-from-kill-ring :around
              (lambda (orig-fn &rest args)
                "Make `consult-yank-from-kill-ring' paste into EXWM applications."
                (if (derived-mode-p 'exwm-mode)
                    (k/exwm-paste-from-kill-ring)
                  (apply orig-fn args)))))

(leaf perspective-exwm
  :after (perspective)
  :elpaca t
  :custom
  (perspective-exwm-override-initial-name . '((0 . "main")))
  :config
  (perspective-exwm-mode 1))

(require 'k-exwm-gaps)
(require 'k-nested-exwm)

;;; BSPWM-style Binary Space Partitioning
;; Automatically splits along the longest dimension

(defgroup k-exwm-bsp nil
  "BSPWM-style window management for EXWM."
  :group 'exwm)

(defcustom k-exwm-bsp-split-ratio 0.5
  "Ratio for BSP splits. 0.5 = equal, 0.6 = 60/40, etc."
  :type 'float
  :group 'k-exwm-bsp)

(defcustom k-exwm-bsp-split-mode 'longest
  "How to determine split direction.
'longest - split along longest dimension (default BSPWM behavior)
'spiral  - alternate directions for spiral pattern
'horizontal - always split horizontally
'vertical - always split vertically"
  :type '(choice (const :tag "Longest dimension" longest)
                 (const :tag "Spiral/alternate" spiral)
                 (const :tag "Always horizontal" horizontal)
                 (const :tag "Always vertical" vertical))
  :group 'k-exwm-bsp)

(defvar k-exwm-bsp--last-direction 'horizontal
  "Last split direction, used for spiral mode.")

(defun k-exwm-bsp-one-window ()
  (perfect-margin-mode 0)
  'horizontal)

(defun k-exwm-bsp-split-direction ()
  "Return the optimal split direction based on current mode.
Returns 'horizontal for side-by-side, 'vertical for top-bottom."
  (pcase k-exwm-bsp-split-mode
    ('longest
     (let* ((edges (window-inside-pixel-edges))
            (width (- (nth 2 edges) (nth 0 edges)))
            (height (- (nth 3 edges) (nth 1 edges))))
	   (if (one-window-p)
		   (k-exwm-bsp-one-window)
		 (if (>= width height) 'horizontal 'vertical))))
    ('spiral
     (setq k-exwm-bsp--last-direction
           (if (eq k-exwm-bsp--last-direction 'horizontal)
               'vertical
             'horizontal)))
    ('horizontal 'horizontal)
    ('vertical 'vertical)
    (_ 'horizontal)))

(defun k-exwm-bsp-split ()
  "Split the current window using BSPWM-style binary space partitioning."
  (interactive)
  (let* ((dir (k-exwm-bsp-split-direction))
         (size (if (eq dir 'horizontal)
                   (floor (* (window-width) k-exwm-bsp-split-ratio))
                 (floor (* (window-height) k-exwm-bsp-split-ratio)))))
    (if (eq dir 'horizontal)
        (split-window-right size)
      (split-window-below size))
    (other-window 1)))

(defun k-exwm-bsp-open (command)
  "Split using BSP logic, then run COMMAND in the new window."
  (interactive "sCommand: ")
  (k-exwm-bsp-split)
  (start-process-shell-command command nil command))

(defun k-exwm-bsp-open-terminal ()
  "Split using BSP logic and open a terminal."
  (interactive)
  (k-exwm-bsp-split)
  (eshell-new))

(defun k-exwm-bsp-open-browser ()
  "Split using BSP logic and open a browser."
  (interactive)
  (k-exwm-bsp-open "firefox"))

(defun k-exwm-rotate-windows ()
  "Rotate window positions clockwise."
  (interactive)
  (let* ((windows (window-list nil 'no-minibuffer))
         (buffers (mapcar #'window-buffer windows))
         (len (length windows)))
    (when (> len 1)
      (dotimes (i len)
        (set-window-buffer (nth i windows)
                           (nth (mod (1- i) len) buffers)))
      (message "Windows rotated"))))


(defun k-exwm-swap-master ()
  "Swap current window with the largest (master) window."
  (interactive)
  (let* ((windows (window-list nil 'no-minibuf))
         (master (car (sort (copy-sequence windows)
                            (lambda (a b)
                              (> (* (window-width a) (window-height a))
                                 (* (window-width b) (window-height b))))))))
    (unless (eq (selected-window) master)
      (window-swap-states (selected-window) master)
      (message "Swapped with master"))))

(defun k-exwm-bsp-set-ratio (ratio)
  "Set the BSP split ratio interactively."
  (interactive "nSplit ratio (0.1-0.9): ")
  (setq k-exwm-bsp-split-ratio (max 0.1 (min 0.9 ratio)))
  (message "BSP ratio: %.0f/%.0f"
           (* 100 k-exwm-bsp-split-ratio)
           (* 100 (- 1 k-exwm-bsp-split-ratio))))

(defun k-exwm-bsp-cycle-mode ()
  "Cycle through BSP split modes."
  (interactive)
  (setq k-exwm-bsp-split-mode
        (pcase k-exwm-bsp-split-mode
          ('longest 'spiral)
          ('spiral 'horizontal)
          ('horizontal 'vertical)
          ('vertical 'longest)))
  (message "BSP mode: %s" k-exwm-bsp-split-mode))

;;; Load EXWM scripting modules
(require 'k-ocr)
(require 'k-exwm-script)

;;; Initialize EXWM
(elpaca-wait)
(require 'exwm)
(exwm-enable)
(exwm-init)

(leaf dmenu
  :elpaca t
  :bind ("s-o" . dmenu))

(provide 'k-exwm)

