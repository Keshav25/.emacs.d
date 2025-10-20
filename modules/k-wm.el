;; -*- lexical-binding: t -*-

(defmacro with-other-window (&rest body)
  "Execute forms in BODY in the other-window."
  `(unless (one-window-p)
     (with-selected-window (other-window-for-scrolling)
       ,@body)))

(leaf winner
  :init
  (winner-mode 1))

(leaf windower
  :elpaca t)

;; Window Divider Mode
(leaf window-divider
  :init
  (setq window-divider-default-places t
		window-divider-default-bottom-width 1
		window-divider-default-right-width 1))

;; Split Thresholds
(setq split-width-threshold 160
	  split-height-threshold nil)

										;: From perspective.el README
(customize-set-variable 'display-buffer-base-action
						'((display-buffer-same-window)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

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

(leaf zoom :elpaca t)

(leaf popper
  :elpaca t
  :bind (("C-`" . popper-toggle)
		 ("M-`" . popper-cycle)
		 ("C-M-`" . popper-toggle-type)
		 ([remap kill-window] . popper-kill-latest-popup))
  :custom
  (popper-reference-buffers .  '("\\*Messages\\*"
								 "Output\\*$"
								 "\\*Async Shell Command\\*"
								 help-mode
								 prodigy-mode
								 "COMMIT_EDITMSG"
								 embark-collect-mode
								 "\\*deadgrep.\*"
								 "\\*eldoc.\*"
								 "\\*xref\\*"
								 "\\*direnv\\*"
								 "\\*Warnings\\*"
								 "\\*Bookmark List\\*"
								 "\\*exwm-edit"
								 "magit:"
								 elfeed-show-mode
								 "*Guix REPL*"
								 haskell-compilation-mode
								 compilation-mode
								 detached-compilation-mode
								 bqn-inferior-mode))
  (popper-display-control . t)
  (popper-display-function . #'display-buffer-at-bottom)
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(leaf shackle
  :elpaca t
  :custom
  (shackle-default-rule . '(:same t))
  (shackle-rules . `(,popper-reference-buffers :align t :size 0.4)))


(leaf tabspaces :elpaca t)

(leaf ace-window
  :elpaca t
  :config
  (setq aw-keys '(?i ?s ?r ?t ?g ?p ?n ?e ?o)
		aw-background nil)
  (setq aw-dispatch-alist
		'((?x aw-delete-window "Delete Window")
		  (?m aw-swap-window "Swap Windows")
		  (?M aw-move-window "Move Window")
		  (?c aw-copy-window "Copy Window")
		  (?l aw-switch-buffer-in-window "Select Buffer")
		  (?f aw-flip-window)
		  (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
		  (?c aw-split-window-fair "Split Fair Window")
		  (?v aw-split-window-vert "Split Vert Window")
		  (?b aw-split-window-horz "Split Horz Window")
		  (?F delete-other-windows "Delete Other Windows")
		  (?? aw-show-dispatch-help)))
  (setq aw-dispatch-always nil)
  (ace-window-display-mode)

  (defun ace-window-prefix ()
	"Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
	(interactive)
	(display-buffer-override-next-command
	 (lambda (buffer _)
       (let (window type)
		 (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
		 (cons window type)))
	 nil "[ace-window]")
	(message "Use `ace-window' to display next command buffer..."))

  :bind
  ("M-o" . ace-window-prefix)
  ("C-x o" . ace-window))

(leaf transpose-frame
  :elpaca t)

(leaf centered-window
  :disabled t
  :elpaca t)

(leaf balanced-windows
  :elpaca t
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
  :elpaca t
  :config)

;; https://github.com/alphapapa/yequake

(leaf good-scroll
  :elpaca t
  :config
  (good-scroll-mode 1))

(leaf context-menu
  :config
  (context-menu-mode 1))

(leaf other-window
  :config
  (defun other-window-mru ()
	"Select the most recently used window on this frame."
	(interactive)
	(when (one-window-p) (split-window-sensibly))
	(when-let ((mru-window
				(get-mru-window
				 nil nil 'not-this-one-dummy)))
      (select-window mru-window)))

  (defalias 'other-window-alternating
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive)
        (if (equal last-command 'other-window-alternating)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))
  (put 'other-window-alternating 'repeat-map 'other-window-repeat-map)
  (keymap-set other-window-repeat-map "o" 'other-window-alternating)
  :bind ("C-x o" . other-window))

(leaf nova
  :disabled t
  :elpaca t
  :elpaca (nova :host github :repo "thisisran/nova")
  :init
  (nova-vertico-mode 1))

(leaf perspective
  :elpaca t
  :require t
  :custom
  (persp-sort . 'created)
  :config
  ;; overrides overrides tab-mode functions
  (setopt persp-mode-prefix-key (kbd "C-x t"))
  (persp-mode))

(defun kill-other-buffers ()
  "Kill all other buffers but this one.  Leave one frame open."
  (interactive)
  (mapc 'kill-buffer
		(delq (current-buffer) (buffer-list)))
  (delete-other-windows))

(leaf perspective-project-bridge
  :elpaca t
  :hook
  (perspective-project-bridge-mode-hook . (lambda ()
											(if perspective-project-bridge-mode
												(perspective-project-bridge-find-perspectives-for-all-buffers)
											  (perspective-project-bridge-kill-perspectives))))
  (persp-mode-hook . perspective-project-bridge-mode))

(leaf treemacs-perspective
  :require t
  :elpaca t
  :after (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives)
  (treemacs-start-on-boot))

;; TODO: buffler integration with perspective and project
;; TODO: avy with perspective and consult buffer
;; TODO: hydra for perspective
(provide 'k-wm)
;;; k-wm.el ends here
