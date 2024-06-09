(winner-mode 1)

;; Window Divider Mode
(setq window-divider-default-places t
	  window-divider-default-bottom-width 1
	  window-divider-default-right-width 1)

;; Split Thresholds
(setq split-width-threshold 160
	  split-height-threshold nil)

										;: From perspective.el README
(customize-set-variable 'display-buffer-base-action
						'((display-buffer-reuse-window display-buffer-same-window)
						  (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

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
  :custom
  (popper-reference-buffers .  '("\\*Messages\\*"
								 "Output\\*$"
								 "\\*Async Shell Command\\*"
								 help-mode
								 prodigy-mode
								 "COMMIT_EDITMSG"
								 "\\*deadgrep.\*"
								 "\\*eldoc.\*"
								 "\\*xref\\*"
								 "\\*direnv\\*"
								 "\\*Warnings\\*"
								 "\\*Bookmark List\\*"
								 haskell-compilation-mode
								 compilation-mode
								 bqn-inferior-mode))
  (popper-display-control . t)
  (popper-display-function . #'display-buffer-at-bottom)
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(leaf shackle
  :ensure t
  :custom
  (shackle-default-rule . '(:same t))
  (shackle-rules . `(,popper-reference-buffers :align t :size 0.4)))


(leaf tabspaces :ensure t)
(leaf windmove :ensure t)

(leaf centered-window
  :ensure t)

(leaf balanced-windows
  :ensure t
  :config
  (balanced-windows-mode 1))

;;;###autoload
(defun fwb-toggle-window-split ()
  "Toggle between vertical and horizontal split."
  ;; Source: https://www.emacswiki.org/emacs/ToggleWindowSplit.
  ;; Author: Jeff Dwork
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  #'split-window-horizontally
				#'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))

;; (leaf perspective
;;   :ensure t  ; use `:straight t` if using straight.el!
;;   :bind (("C-x k" . persp-kill-buffer*)
;; 		 (:persp-mode-map
;; 		  ("s" . persp-switch)))
;;   :custom ((persp-mode-prefix-key . "C-x x"))
;;   :init
;;   (persp-mode))

(leaf other-wm
  ;; need to replace i3-command with xdotools or something that works on wayland
  :disabled t
  :require windmove
  :bind (("s-j" . k-owm-left)
         ("s-l" . k-owm-right)
         ("s-i" . k-owm-up)
         ("s-k" . k-owm-down)
         ("s-J" . k-owm-swap-states-left)
         ("s-L" . k-owm-swap-states-right))
  :config
  (defun k-owm-left (&optional arg)
    "Like windmove-left but call i3 command `focus left'
if there is no window on the left."
    (interactive "P")
    (if (windmove-find-other-window 'left arg)
        (windmove-do-window-select 'left arg)
      ;; No window to the left
      (i3-command 0 "focus left")))

  (defun k-owm-right (&optional arg)
    "Like windmove-right but call i3 command `focus right'
if there is no window on the right."
    (interactive "P")
    (if (windmove-find-other-window 'right arg)
        (windmove-do-window-select 'right arg)
      ;; No window to the right
      (i3-command 0 "focus right")
      ;; (i3-command 0 "mode 'default'")
      ))

  (defun k-owm-up (&optional arg)
    "Like windmove-up but call i3 command `focus up'
if there is no window on the up."
    (interactive "P")
    (if (windmove-find-other-window 'up arg)
        (windmove-do-window-select 'up arg)
      ;; No window to the up
      (i3-command 0 "focus up")))

  (defun k-owm-down (&optional arg)
    "Like windmove-down but call i3 command `focus down'
if there is no window on the down."
    (interactive "P")
    (let ((other-window (windmove-find-other-window 'down arg)))
      (if (or (and other-window
                   (not (window-minibuffer-p other-window)))
              (and (window-minibuffer-p other-window)
                   (minibuffer-window-active-p other-window)))
          (windmove-do-window-select 'down arg)
        ;; No window to the down
        (i3-command 0 "focus down"))))

  (defun k-owm-swap-states-left (&optional arg)
    "Like windmove-swap-states-left but call i3 command `move left'
if there is no window on the left."
    (interactive "P")
    (if (windmove-find-other-window 'left arg)
        (windmove-swap-states-in-direction 'left)
      ;; No window to the left
      (i3-command 0 "move left")))

  (defun k-owm-swap-states-right (&optional arg)
    "Like windmove-swap-states-right but call i3 command `move right'
if there is no window on the right."
    (interactive "P")
    (if (windmove-find-other-window 'right arg)
        (windmove-swap-states-in-direction 'right)
      ;; No window to the right
      (i3-command 0 "move right"))))

(leaf golden-ratio
  :ensure t
  :config)

(leaf hydra
  :ensure t
  :config
  (leaf major-mode-hydra
	:ensure t
	:bind
	("M-SPC" . major-mode-hydra)
	:config
	(major-mode-hydra-define emacs-lisp-mode
	  (:quit-key "q")
	  ("Eval"
	   (("b" eval-buffer "buffer")
		("e" eval-defun "defun")
		("r" eval-region "region"))
	   "REPL"
	   (("I" ielm "ielm"))
	   "Test"
	   (("t" ert "prompt")
		("T" (ert t) "all")
		("F" (ert :failed) "failed"))
	   "Doc"
	   (("d" describe-foo-at-point "thing-at-pt")
		("f" describe-function "function")
		("v" describe-variable "variable")
		("i" info-lookup-symbol "info lookup")))))
  (pretty-hydra-define k-window-movement
	(:color amaranth :quit-key "q" :title "Window Management")
	("Navigation"
	 (("h" windmove-left "Move to Left Window")
	  ("l" windmove-right "Move to Right Window")
	  ("j" windmove-down "Move Down a Window")
	  ("k" windmove-up "Move Up a Window"))
	 "Manipulation"
	 (("s" split-and-follow-vertically "Split Window Horizontally")
	  ("v" split-and-follow-horizontally "Split Window Vertically")
	  ("d" delete-window "Delete Window")
	  ("o" switch-window "Switch Window")
	  ("f" k-toggle-fullscreen "Un/Maximize a Window")
	  ("R" evil-window-rotate-upwards "Rotate Windows")
	  ("r" evil-window-rotate-downwards "Reverse Rotate Windows")
	  ("u" winner-undo "Undo Window Manipulation")
	  ("U" winner-redo "Redo Window Manipulation"))
	 "Size"
	 (("+" evil-window-increase-height "Increase Height")
	  ("-" evil-window-decrease-height "Decrease Heigth")
	  ("<" evil-window-decrease-width "Decrease Width")
	  (">" evil-window-increase-width "Increase Width")
	  ("=" balance-windows "Balance Windows")
	  (";" enlarge-window "Enlarge Window"))
	 "Buffer"
	 (("b" consult-buffer "Change Buffer")
	  ("c" centered-window-mode "Un/Center Window"))
	 "Swap Windows"
	 (("H" windmove-swap-states-left "Move Window Left")
	  ("J" windmove-swap-states-down "Move Window Down")
	  ("K" windmove-swap-states-up "Move Window Up")
	  ("L" windmove-swap-states-right "Move Window Right"))
	 "Text"
	 (("C-=" text-scale-increase "zoom in")
	  ("C--" text-scale-decrease "zoom out")))))

;; https://github.com/alphapapa/yequake

(leaf good-scroll
  :ensure t
  :config
  (good-scroll-mode 1))

(provide 'k-wm)
;;; k-wm.el ends here
