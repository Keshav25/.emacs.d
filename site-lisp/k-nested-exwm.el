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

(provide 'k-nested-exwm)
