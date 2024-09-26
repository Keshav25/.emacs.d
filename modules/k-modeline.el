
(setq mode-line-compact nil)
(setq mode-line-right-align-edge 'right-margin) ; Emacs 30
										; Note that separate to this is my `prot-modeline-subtle-mode'.
(setq-default mode-line-format
			  '("%e"
                my-modeline-kbd-macro
                my-modeline-narrow
                my-modeline-input-method
                my-modeline-buffer-status
                " "
                my-modeline-buffer-identification
                "  "
                my-modeline-major-mode
                my-modeline-process
                "  "
                my-modeline-vc-branch
                "  "
                my-modeline-flymake
                "  "
                my-modeline-align-right
                my-modeline-misc-info))
(setq mode-line-format nil)


(with-eval-after-load 'spacious-padding
  (defun my/modeline-spacious-indicators ()
    "Set box attribute to `'my-modeline-indicator-button' if spacious-padding is enabled."
    (if (bound-and-true-p spacious-padding-mode)
        (set-face-attribute 'my-modeline-indicator-button nil :box t)
      (set-face-attribute 'my-modeline-indicator-button nil :box 'unspecified)))

  (my/modeline-spacious-indicators)

  (kill-local-variable 'mode-line-format)
  
  (force-mode-line-update)

  (setq-default mode-line-format
				'("%e"
                  my-modeline-buffer-name
                  "  "
                  my-modeline-major-mode))

  (defface my-modeline-background
	'((t :background "#3355bb" :foreground "white" :inherit bold))
	"Face with a red background for use on the mode line.")

  (defun my-modeline--buffer-name ()
	"Return `buffer-name' with spaces around it."
	(format " %s " (buffer-name)))

  (defvar-local my-modeline-buffer-name
      '(:eval
		(when (mode-line-window-selected-p)
          (propertize (my-modeline--buffer-name) 'face 'my-modeline-background)))
	"Mode line construct to display the buffer name.")

  (put 'my-modeline-buffer-name 'risky-local-variable t)

  (defun my-modeline--major-mode-name ()
	"Return capitalized `major-mode' as a string."
	(capitalize (symbol-name major-mode)))

  (defvar-local my-modeline-major-mode
      '(:eval
		(list
		 (propertize "λ" 'face 'shadow)
		 " "
		 (propertize (my-modeline--major-mode-name) 'face 'bold)))
	"Mode line construct to display the major mode.")

  (put 'my-modeline-major-mode 'risky-local-variable t)





  ;; Emacs 29, check the definition right below
  (mode-line-window-selected-p)

  (defun mode-line-window-selected-p ()
	"Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
	(let ((window (selected-window)))
      (or (eq window (old-selected-window))
		  (and (minibuffer-window-active-p (minibuffer-window))
			   (with-selected-window (minibuffer-window)
				 (eq window (minibuffer-selected-window)))))))

  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'my-modeline-vc-branch)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
	(add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
	(add-to-list 'keycast-substitute-alist `(,event nil))))

(provide 'k-modeline)
