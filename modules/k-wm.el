;; -*- lexical-binding: t -*-

(leaf winner
  :init
  (winner-mode 1))

(leaf window-divider
  :custom
  (window-divider-default-places . t)
  (window-divider-default-bottom-width . 1)
  (window-divider-default-right-width . 1)
  :init
  (window-divider-mode 1))

(leaf window
  :custom
  (split-width-threshold . 160)
  (split-height-threshold . nil)
  (display-buffer-base-action . '((display-buffer-same-window)))
  (even-window-sizes . nil)
  :config
  (defun k/split-and-follow-below ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))

  (defun k/split-and-follow-right ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))

  (defun k/toggle-fullscreen ()
    (interactive)
    (if (and winner-mode
             (equal (selected-window) (next-window)))
        (winner-undo)
      (delete-other-windows)))

  (defun k/toggle-window-split ()
    (interactive)
    (unless (= (count-windows) 2)
      (user-error "Can only toggle with exactly 2 windows"))
    (let* ((this-buf (window-buffer))
           (next-buf (window-buffer (next-window)))
           (this-edges (window-edges (selected-window)))
           (next-edges (window-edges (next-window)))
           (this-2nd (not (and (<= (car this-edges) (car next-edges))
                               (<= (cadr this-edges) (cadr next-edges)))))
           (splitter (if (= (car this-edges) (car (window-edges (next-window))))
                         #'split-window-horizontally
                       #'split-window-vertically)))
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (when this-2nd (other-window 1))
        (set-window-buffer (selected-window) this-buf)
        (set-window-buffer (next-window) next-buf)
        (select-window first-win)
        (when this-2nd (other-window 1)))))

  (defun k/kill-other-buffers ()
    (interactive)
    (when (y-or-n-p "Kill all other buffers? ")
      (if (and (fboundp 'persp-current-buffers) (bound-and-true-p persp-mode))
          (let ((current (current-buffer)))
            (dolist (buf (persp-current-buffers))
              (unless (eq buf current)
                (kill-buffer buf))))
        (mapc #'kill-buffer (delq (current-buffer) (buffer-list))))
      (delete-other-windows)))

  (defun k/window-config-save (register)
    (interactive "cSave layout to register: ")
    (window-configuration-to-register register)
    (message "Saved to '%c'" register))

  (defun k/window-config-restore (register)
    (interactive "cRestore layout from register: ")
    (jump-to-register register)
    (message "Restored '%c'" register))

  (defmacro with-other-window (&rest body)
    `(unless (one-window-p)
       (with-selected-window (other-window-for-scrolling)
         ,@body)))

  ;; Layouts
  (defun k/layout-code ()
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (if (fboundp 'eshell) (eshell) (term "/bin/bash"))
    (other-window 1)
    (let ((target-width (round (* 0.7 (frame-width)))))
      (enlarge-window-horizontally (- target-width (window-width)))))

  (defun k/layout-reading ()
    (interactive)
    (delete-other-windows)
    (when (fboundp 'perfect-margin-mode)
      (perfect-margin-mode 1)))

  (defun k/layout-three-columns ()
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (split-window-right)
    (balance-windows))

  (defun k/layout-main-side ()
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (split-window-below)
    (other-window -1)
    (let ((target-width (round (* 0.65 (frame-width)))))
      (enlarge-window-horizontally (- target-width (window-width)))))

  (defun k/layout-elfeed ()
    (interactive)
    (delete-other-windows)
    (elfeed)
    (split-window-right))

  ;; display-buffer rules
  (setq display-buffer-alist
        '(("\\*e?shell\\*\\|\\*vterm\\*\\|\\*terminal\\*"
           (display-buffer-reuse-window display-buffer-at-bottom)
           (window-height . 0.3))
          ("\\*Help\\*\\|\\*helpful\\|\\*info\\*\\|\\*Man "
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right) (window-width . 0.4))
          ("\\*compilation\\*\\|\\*Compile-Log\\*"
           (display-buffer-reuse-window display-buffer-at-bottom)
           (window-height . 0.3))
          ("magit-diff\\|magit-log"
           (display-buffer-reuse-window display-buffer-at-bottom)
           (window-height . 0.4))
          ("\\*Org Src"
           (display-buffer-same-window))
          ("\\*Denote Backlinks\\*"
           (display-buffer-in-side-window)
           (side . right) (window-width . 0.35))))

  :bind
  (("C-x 2" . k/split-and-follow-below)
   ("C-x 3" . k/split-and-follow-right)
   ("C-x 1" . k/toggle-fullscreen)
   ("C-x |" . k/toggle-window-split)
   ("C-x w s" . k/window-config-save)
   ("C-x w r" . k/window-config-restore)
   ("C-x w K" . k/kill-other-buffers)
   ("C-x w c" . k/layout-code)
   ("C-x w R" . k/layout-reading)
   ("C-x w 3" . k/layout-three-columns)
   ("C-x w m" . k/layout-main-side)
   ("C-x w e" . k/layout-elfeed)))

(leaf popper
  :elpaca t
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers . '("\\*Messages\\*"
                                "Output\\*$"
                                "\\*Async Shell Command\\*"
                                help-mode
                                helpful-mode
                                prodigy-mode
                                "COMMIT_EDITMSG"
                                "\\*eldoc.*\\*"
                                "\\*xref\\*"
                                "\\*direnv\\*"
                                "\\*Warnings\\*"
                                "\\*Bookmark List\\*"
                                "\\*exwm-edit"
                                "^magit:.*"
                                elfeed-show-mode
                                haskell-compilation-mode
                                compilation-mode
                                detached-compilation-mode
                                occur-mode
                                grep-mode
                                embark-collect-mode
                                deadgrep-mode
                                "^\\*deadgrep"
                                backtrace-mode
                                "^\\*eshell"
                                "^\\*Denote"
                                "^\\*elfeed-tube"))
  (popper-display-control . t)
  (popper-display-function . #'display-buffer-at-bottom)
  (popper-group-function . #'popper-group-by-directory)
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(leaf ace-window
  :elpaca t
  :custom
  (aw-keys . '(?i ?s ?r ?t ?g ?p ?n ?e ?o))
  (aw-background . nil)
  (aw-dispatch-always . nil)
  (aw-dispatch-alist . '((?x aw-delete-window "Delete Window")
                          (?w aw-swap-window "Swap Windows")
                          (?M aw-move-window "Move Window")
                          (?c aw-copy-window "Copy Window")
                          (?l aw-switch-buffer-in-window "Select Buffer")
                          (?f aw-flip-window)
                          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
                          (?v aw-split-window-vert "Split Vert Window")
                          (?b aw-split-window-horz "Split Horz Window")
                          (?F delete-other-windows "Delete Other Windows")
                          (?? aw-show-dispatch-help)))
  :config
  (defun aw--switch-buffer ()
    (call-interactively 'consult-buffer))
  (ace-window-display-mode)

  (defun ace-window-prefix ()
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
               type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))

  :bind
  (("M-o" . ace-window-prefix)
   ("C-x o" . ace-window)))

(leaf windmove
  :config
  (windmove-default-keybindings 'super)
  (windmove-swap-states-default-keybindings '(super shift)))

(leaf transpose-frame
  :elpaca t
  :bind (("C-x 4 t" . transpose-frame)
         ("C-x 4 r" . rotate-frame-clockwise)))

(leaf windower
  :elpaca t
  :bind (("C-x 4 s" . windower-swap-right)
         ("C-x 4 S" . windower-swap-below)))

(leaf good-scroll
  :elpaca t
  :config
  (good-scroll-mode 1))

(leaf context-menu
  :config
  (context-menu-mode 1))

(leaf perspective
  :elpaca t
  :require t
  :custom
  (persp-sort . 'created)
  :config
  (setopt persp-mode-prefix-key (kbd "C-x t"))
  (persp-mode)
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)

  (defun k/persp-switch-or-create (name)
    (persp-switch name))

  (defun k/persp-elfeed ()
    (interactive)
    (k/persp-switch-or-create "elfeed")
    (unless (get-buffer "*elfeed-search*")
      (elfeed)))

  (defun k/persp-notes ()
    (interactive)
    (k/persp-switch-or-create "notes")
    (let ((denote-dir (or (bound-and-true-p denote-directory) "~/Documents/notes/")))
      (unless (cl-some (lambda (b) (string-prefix-p denote-dir (or (buffer-file-name b) "")))
                       (buffer-list))
        (dired denote-dir))))

  (defun k/persp-magit ()
    (interactive)
    (let* ((project (ignore-errors (project-name (project-current t))))
           (name (format "git:%s" (or project "magit"))))
      (k/persp-switch-or-create name)
      (magit-status)))

  (defun k/persp-term ()
    (interactive)
    (k/persp-switch-or-create "term")
    (unless (cl-some (lambda (b)
                       (with-current-buffer b
                         (derived-mode-p 'eshell-mode 'vterm-mode 'term-mode)))
                     (buffer-list))
      (eshell)))

  (defun k/persp-status ()
    (interactive)
    (let ((current (persp-current-name)))
      (message "Perspectives: %s"
               (string-join
                (mapcar (lambda (name)
                          (let ((buf-count (length (persp-buffers (persp-get-by-name name)))))
                            (if (string= name current)
                                (propertize (format "[%s:%d]" name buf-count) 'face 'bold)
                              (format "%s:%d" name buf-count))))
                        (persp-names))
                " | "))))

  :bind
  (("C-x t e" . k/persp-elfeed)
   ("C-x t n" . k/persp-notes)
   ("C-x t g" . k/persp-magit)
   ("C-x t T" . k/persp-term)
   ("C-x t I" . k/persp-status)))

(leaf perspective-project-bridge
  :elpaca t
  :hook
  (perspective-project-bridge-mode-hook .
   (lambda ()
     (if perspective-project-bridge-mode
         (perspective-project-bridge-find-perspectives-for-all-buffers)
       (perspective-project-bridge-kill-perspectives))))
  (persp-mode-hook . perspective-project-bridge-mode))

(leaf treemacs-perspective
  :elpaca t
  :require t
  :config
  (treemacs-set-scope-type 'Perspectives))

(leaf golden-ratio
  :elpaca t
  :custom
  (golden-ratio-exclude-modes . '(treemacs-mode ediff-mode elfeed-search-mode))
  (golden-ratio-exclude-buffer-regexp . '("\\*Calc" "\\*which-key\\*"))
  :config
  (defun k/toggle-golden-ratio ()
    (interactive)
    (golden-ratio-mode (if golden-ratio-mode -1 1))
    (message "Golden ratio: %s" (if golden-ratio-mode "ON" "OFF")))
  :bind
  (("C-x w g" . k/toggle-golden-ratio)))

(provide 'k-wm)
;;; k-wm.el ends here
