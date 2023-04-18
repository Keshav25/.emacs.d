(leaf nano-modeline
  :ensure t
  :config
  (nano-modeline-mode 1))

(winner-mode 1)

;; Window Divider Mode
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)

;; Split Thresholds
(setq split-width-threshold 160
      split-height-threshold nil)

;; Switch Window
(leaf switch-window
  :ensure t
  :setq
  (switch-window-input-style . 'minibuffer)
  (switch-window-increase . 4)
  (switch-window-threshold . 2)
  (switch-window-shortcut-style . 'qwerty)
  (switch-window-qwerty-shortcuts .
				  '("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

;; Split and Follow Functions
(defun split-and-follow-horizontally ()
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))
 (global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

 (defun split-and-follow-vertically ()
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))
 (global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; Toggle fullscreen
(defun k-toggle-fullscreen ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
		   (equal (selected-window) (next-window)))
	  (winner-undo)
	    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'k-toggle-fullscreen)

(leaf zoom :ensure t)
(leaf popper
  :ensure t
  :config
  (popper-mode 1))
(leaf tabspaces :ensure t)
(leaf windmove :ensure t)

(leaf centered-window
  :ensure t
  :config
  (centered-window-mode 1))

(leaf balanced-windows
  :ensure t
  :config
  (balanced-windows-mode 1))

(provide 'k-wm)
