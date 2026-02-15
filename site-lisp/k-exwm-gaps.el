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

(defcustom k-exwm-gaps-focus-border-color "purple"
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
          (max 0 (- width left-gap right-gap))
          (max 0 (- height top-gap bottom-gap)))))

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
  (message "EXWM gaps: %dpx" k-exwm-gaps-inner-gap))

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

(provide 'k-exwm-gaps)
