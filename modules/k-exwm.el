(leaf exwm
  :ensure t
  :custom
  (use-dialog-box . nil)
  (exwm-input-line-mode-passthrough . t)
  (mouse-autoselect-window . t)
  (exwm-input-line-mode-passthrough . nil)
  (ediff-window-setup-function . 'ediff-setup-windows-plain)
  (focus-follows-mouse . t)
  :hook ((exwm-update-title-hook . (lambda () (exwm-workspace-rename-buffer exwm-title)))
		 (exwm-input--input-mode-change-hook . force-modeline-update))
  :config
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
		 ("s-TAB" . #'exwm/jump-to-last-exwm)))

(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "eDP1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal")))
(exwm-randr-enable)
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
        (,(kbd "s-o") . windower-toggle-single) ;; Toggle between multiple windows, and a single window
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
        (,(kbd "s-<backspace>") . kill-this-buffer)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(setq exwm-input-simulation-keys
	  '(
		([?\C-b] . [left])
		([?\C-f] . [right])
		([?\C-p] . [up])
		([?\C-n] . [down])
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
		([?\C-\_] . [?\C-z])
		([?\M-f] . [C-right])
		([?\M-b] . [C-left])
		([?\M-d] . [C-S-right delete])
		))

(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)

(window-divider-mode 1)

(defun fhd/toggle-exwm-input-line-mode-passthrough ()
  (interactive)
  (if exwm-input-line-mode-passthrough
      (progn
        (setq exwm-input-line-mode-passthrough nil)
        (message "App receives all the keys now (with some simulation)"))
    (progn
      (setq exwm-input-line-mode-passthrough t)
      (message "emacs receives all the keys now"))))

(defun efs/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil  "feh --bg-scale ~/Pictures/wallpaper.jpg"))

(defun efs/exwm-init-hook ()
  (exwm-workspace-switch-create 1))

(display-battery-mode 1)
(setq display-time-day-and-date t)
(display-time-mode 1)

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(add-hook 'exwm-update-class-hook #'efs/exwm-update-class)
(add-hook 'exwm-init-hook #'efs/exwm-init-hook)
(efs/set-wallpaper)
(require 'exwm-systemtray)
(setq exwm-systemtray-height 17)
(exwm-systemtray-enable)

;; Desktop-Environment
(leaf desktop-environment
  :ensure t
  :after exwm
  :custom
  (desktop-environment-brightness-small-increment . "2%+")
  (desktop-environment-brightness-small-decrement . "2%-")
  (desktop-environment-brightness-normal-increment . "5%+")
  (desktop-environment-brightness-normal-decrement . "5%-")
  (exwm-layout-show-all-buffers . t)
  :config
  (desktop-environment-mode 1)
  :bind (:desktop-environment-mode-map
		 ("s-l" . nil)))

;; Adds to more evil states for EXWM's char and line modes,
;; stole from https://github.com/domenzain/evil-exwm-state/tree/master
;; (leaf evil-exwm-state
;;   :disabled t
;;   :after (evil exwm)
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


(leaf exwm-edit
  :ensure t
  ;; exwm-edit-compose-hook
  )

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

