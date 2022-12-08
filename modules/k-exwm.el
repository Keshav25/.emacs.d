(leaf exwm)
(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "eDP1"))
 (add-hook 'exwm-randr-screen-change-hook
           (lambda ()
             (start-process-shell-command
              "xrandr" nil "xrandr --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal")))
  (exwm-randr-enable)
 (setq exwm-input-global-keys
       `(
         ([?\s-r] . exwm-reset)
         ([?\s-h] . windmove-left)
         ([?\s-l] . windmove-right)
         ([?\s-k] . windmove-up)
         ([?\s-j] . windmove-down)
        ([?\s-o] . counsel-linux-app)
        ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
        ([?\s-w] . exwm-workspace-switch)
        ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

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
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment . "2%+")
  (desktop-environment-brightness-small-decrement . "2%-")
  (desktop-environment-brightness-normal-increment . "5%+")
  (desktop-environment-brightness-normal-decrement . "5%-"))

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

