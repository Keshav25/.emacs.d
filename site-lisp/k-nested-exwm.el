;; -*- lexical-binding: t -*-
;; requires: Xephyr

(require 'cl-lib)

(defgroup k-exwm-nested nil
  "Nested window manager support via Xephyr."
  :group 'exwm
  :prefix "k-exwm-nested-")

(defcustom k-exwm-nested-size "1920x1080"
  "Default size for Xephyr window (WIDTHxHEIGHT).
When `k-exwm-nested-auto-size' is non-nil, this is ignored in
favor of the current window pixel dimensions."
  :type 'string
  :group 'k-exwm-nested)

(defcustom k-exwm-nested-default-wm "dwm"
  "Default window manager to run in Xephyr."
  :type 'string
  :group 'k-exwm-nested)

(defcustom k-exwm-nested-auto-size t
  "When non-nil, automatically size Xephyr to the current Emacs window."
  :type 'boolean
  :group 'k-exwm-nested)

(defcustom k-exwm-nested-clipboard-sync t
  "When non-nil, synchronize clipboard between host and nested X server.
Requires `xclip' to be installed."
  :type 'boolean
  :group 'k-exwm-nested)

(defcustom k-exwm-nested-xephyr-extra-args '("-resizeable")
  "Extra arguments to pass to Xephyr.
Common options:
  \"-host-cursor\"   — use the host cursor in Xephyr (seamless mouse)
  \"-resizeable\"    — allow Xephyr window to be resized
  \"-fullscreen\"    — start Xephyr in fullscreen
  \"-no-host-grab\"  — don't grab keyboard/mouse on click"
  :type '(repeat string)
  :group 'k-exwm-nested)

(defcustom k-exwm-nested-presets
  '(("dwm"       :command "dwm"            :type wm)
    ("i3"        :command "i3"             :type wm)
    ("bspwm"     :command "bspwm"          :type wm)
    ("herbstluftwm" :command "herbstluftwm --locked" :type wm)
    ("openbox"   :command "openbox-session" :type wm)
    ("fluxbox"   :command "startfluxbox"   :type wm)
    ("awesome"   :command "awesome"        :type wm)
    ("xfce"      :command "startxfce4"     :type de)
    ("lxde"      :command "startlxde"      :type de)
    ("lxqt"      :command "startlxqt"      :type de)
    ("mate"      :command "mate-session"   :type de)
    ("cinnamon"  :command "cinnamon-session" :type de))
  "Alist of WM/DE presets.
Each entry is (NAME :command CMD :type TYPE) where TYPE is `wm' or `de'."
  :type '(alist :key-type string
                :value-type (plist :options ((:command string)
                                             (:type (choice (const wm)
                                                            (const de))))))
  :group 'k-exwm-nested)

(defcustom k-exwm-nested-startup-delay 0.8
  "Seconds to wait for Xephyr to initialize before starting the WM.
Increase this if the WM fails to start on slower machines."
  :type 'number
  :group 'k-exwm-nested)


(cl-defstruct (k-exwm-nested-session (:constructor k-exwm-nested-session--create))
  "A nested X session running inside Xephyr."
  (display nil :documentation "Display number (integer).")
  (display-str nil :documentation "Display string like \":10\".")
  (wm-name nil :documentation "Name of the WM/DE running.")
  (wm-command nil :documentation "Full command used to launch the WM/DE.")
  (xephyr-proc nil :documentation "Xephyr process object.")
  (wm-proc nil :documentation "WM/DE process object.")
  (clipboard-proc nil :documentation "Clipboard sync process, if any.")
  (log-buffer nil :documentation "Log buffer for process output.")
  (size nil :documentation "Size string WxH.")
  (start-time nil :documentation "Time when session was started."))

(defvar k-exwm-nested--sessions nil
  "List of active `k-exwm-nested-session' structs.")

(defvar k-exwm-nested--next-display 10
  "Next display number to try for Xephyr.")


(defun k-exwm-nested--find-free-display ()
  "Find a free X display number starting from `k-exwm-nested--next-display'."
  (let ((display k-exwm-nested--next-display))
    (while (file-exists-p (format "/tmp/.X%d-lock" display))
      (setq display (1+ display)))
    (setq k-exwm-nested--next-display (1+ display))
    display))

(defun k-exwm-nested--window-pixel-size ()
  "Return the current window's pixel size as a \"WIDTHxHEIGHT\" string."
  (format "%dx%d"
          (window-pixel-width)
          (window-pixel-height)))

(defun k-exwm-nested--check-xephyr ()
  "Signal an error if Xephyr is not available."
  (unless (executable-find "Xephyr")
    (user-error "Xephyr is not installed. Install it with your package manager (e.g. `apt install xserver-xephyr')")))

(defun k-exwm-nested--find-session (display)
  "Find the session struct for DISPLAY number."
  (cl-find display k-exwm-nested--sessions
           :key #'k-exwm-nested-session-display))

(defun k-exwm-nested--remove-session (display)
  "Remove the session for DISPLAY from the session list."
  (setq k-exwm-nested--sessions
        (cl-remove display k-exwm-nested--sessions
                   :key #'k-exwm-nested-session-display)))

(defun k-exwm-nested--completing-read-session (prompt)
  "Interactively select an active session with PROMPT.
Returns the display number."
  (unless k-exwm-nested--sessions
    (user-error "No active nested sessions"))
  (let* ((candidates
          (mapcar (lambda (s)
                    (cons (format ":%d (%s) — %s"
                                  (k-exwm-nested-session-display s)
                                  (k-exwm-nested-session-wm-name s)
                                  (k-exwm-nested-session-size s))
                          (k-exwm-nested-session-display s)))
                  k-exwm-nested--sessions))
         (choice (completing-read prompt candidates nil t)))
    (cdr (assoc choice candidates))))

(defun k-exwm-nested--make-log-buffer (display wm-name)
  "Create or get a log buffer for session on DISPLAY running WM-NAME."
  (get-buffer-create (format "*nested:%d (%s)*" display wm-name)))


;;;###autoload
(defun k-exwm-nested-start (wm-command &optional size)
  "Start a nested X session running WM-COMMAND inside Xephyr.
SIZE is an optional \"WIDTHxHEIGHT\" string; when nil and
`k-exwm-nested-auto-size' is non-nil, the current window's pixel
dimensions are used.  With a prefix argument, prompt for SIZE."
  (interactive
   (list (read-string "WM/DE command: " k-exwm-nested-default-wm)
         (when current-prefix-arg
           (read-string "Size (WxH): " k-exwm-nested-size))))
  (k-exwm-nested--check-xephyr)
  (let* ((display (k-exwm-nested--find-free-display))
         (display-str (format ":%d" display))
         (size (or size
                   (and k-exwm-nested-auto-size
                        (k-exwm-nested--window-pixel-size))
                   k-exwm-nested-size))
         (wm-name (car (split-string wm-command)))
         (log-buf (k-exwm-nested--make-log-buffer display wm-name))
         (xephyr-args (append (list display-str
                                    "-screen" size
                                    "-title" (format "Nested: %s" wm-name)
                                    "-name" (format "nested-%s" wm-name))
                              k-exwm-nested-xephyr-extra-args))
         (xephyr-proc (apply #'start-process
                             (format "xephyr-%d" display)
                             log-buf
                             "Xephyr"
                             xephyr-args))
         (session (k-exwm-nested-session--create
                   :display display
                   :display-str display-str
                   :wm-name wm-name
                   :wm-command wm-command
                   :xephyr-proc xephyr-proc
                   :log-buffer log-buf
                   :size size
                   :start-time (current-time))))
    ;; Sentinel for Xephyr — cleans up the whole session
    (set-process-sentinel
     xephyr-proc
     (lambda (proc event)
       (unless (process-live-p proc)
         (let ((s (k-exwm-nested--find-session display)))
           (when s
             ;; Kill WM process if still alive
             (when (and (k-exwm-nested-session-wm-proc s)
                        (process-live-p (k-exwm-nested-session-wm-proc s)))
               (kill-process (k-exwm-nested-session-wm-proc s)))
             ;; Kill clipboard sync if running
             (when (and (k-exwm-nested-session-clipboard-proc s)
                        (process-live-p (k-exwm-nested-session-clipboard-proc s)))
               (kill-process (k-exwm-nested-session-clipboard-proc s)))
             (k-exwm-nested--remove-session display)
             (message "Nested session %s (%s) closed: %s"
                      display-str wm-name (string-trim event)))))))
    ;; Launch WM after Xephyr initializes
    (run-at-time
     k-exwm-nested-startup-delay nil
     (lambda ()
       (if (process-live-p xephyr-proc)
           (let* ((process-environment
                   (cons (format "DISPLAY=%s" display-str)
                         process-environment))
                  (wm-proc (start-process-shell-command
                            (format "nested-wm-%d" display)
                            log-buf
                            wm-command)))
             (setf (k-exwm-nested-session-wm-proc session) wm-proc)
             ;; Sentinel for WM — restart or notify
             (set-process-sentinel
              wm-proc
              (lambda (proc event)
                (unless (process-live-p proc)
                  (message "Nested WM %s on %s exited: %s"
                           wm-name display-str (string-trim event)))))
             ;; Start clipboard sync if enabled
             (when (and k-exwm-nested-clipboard-sync
                        (executable-find "xclip"))
               (k-exwm-nested--start-clipboard-sync session))
             (message "Started %s on display %s (%s)" wm-name display-str size))
         (message "Xephyr failed to start for display %s" display-str))))
    ;; Track session
    (push session k-exwm-nested--sessions)
    session))

;;;###autoload
(defun k-exwm-nested-preset ()
  "Start a nested session from a preset WM/DE configuration."
  (interactive)
  (k-exwm-nested--check-xephyr)
  (let* ((names (mapcar #'car k-exwm-nested-presets))
         (choice (completing-read "Preset WM/DE: " names nil t))
         (preset (cdr (assoc choice k-exwm-nested-presets)))
         (command (plist-get preset :command)))
    (k-exwm-nested-start command)))


;;;###autoload
(defun k-exwm-nested-run (command &optional display)
  "Run COMMAND inside a nested Xephyr session.
If DISPLAY is nil, use the most recent session.  With a prefix
argument, prompt to select which session."
  (interactive
   (let ((disp (if (or current-prefix-arg
                       (> (length k-exwm-nested--sessions) 1))
                   (k-exwm-nested--completing-read-session "Run in session: ")
                 (and k-exwm-nested--sessions
                      (k-exwm-nested-session-display (car k-exwm-nested--sessions))))))
     (list (read-string "Command to run in nested session: ") disp)))
  (unless k-exwm-nested--sessions
    (user-error "No active nested sessions"))
  (let* ((display (or display
                      (k-exwm-nested-session-display (car k-exwm-nested--sessions))))
         (session (k-exwm-nested--find-session display))
         (display-str (k-exwm-nested-session-display-str session)))
    (start-process-shell-command
     "nested-cmd"
     (k-exwm-nested-session-log-buffer session)
     (format "DISPLAY=%s %s" display-str command))
    (message "Running '%s' on display %s" command display-str)))


;;;###autoload
(defun k-exwm-nested-list ()
  "Display active nested sessions in a help buffer."
  (interactive)
  (if (null k-exwm-nested--sessions)
      (message "No active nested sessions")
    (with-help-window "*Nested Sessions*"
      (with-current-buffer "*Nested Sessions*"
        (insert (propertize "Active Nested Sessions\n" 'face 'bold)
                (make-string 40 ?─) "\n\n")
        (dolist (s k-exwm-nested--sessions)
          (let* ((uptime (float-time
                          (time-subtract (current-time)
                                         (k-exwm-nested-session-start-time s))))
                 (mins (floor (/ uptime 60)))
                 (hrs (floor (/ mins 60))))
            (insert (format "  Display:  %s\n" (k-exwm-nested-session-display-str s))
                    (format "  WM/DE:    %s\n" (k-exwm-nested-session-wm-name s))
                    (format "  Command:  %s\n" (k-exwm-nested-session-wm-command s))
                    (format "  Size:     %s\n" (k-exwm-nested-session-size s))
                    (format "  Uptime:   %dh %dm\n" hrs (mod mins 60))
                    (format "  Xephyr:   %s\n"
                            (if (process-live-p (k-exwm-nested-session-xephyr-proc s))
                                "running" "dead"))
                    (format "  WM:       %s\n"
                            (if (and (k-exwm-nested-session-wm-proc s)
                                     (process-live-p (k-exwm-nested-session-wm-proc s)))
                                "running" "dead"))
                    (format "  Log:      %s\n"
                            (buffer-name (k-exwm-nested-session-log-buffer s)))
                    "\n")))))))

;;;###autoload
(defun k-exwm-nested-stop (&optional display)
  "Stop nested session on DISPLAY.
Interactively, prompt to select a session."
  (interactive
   (list (k-exwm-nested--completing-read-session "Stop session: ")))
  (let ((session (k-exwm-nested--find-session display)))
    (unless session
      (user-error "No session on display :%d" display))
    ;; Kill WM first, then Xephyr
    (when (and (k-exwm-nested-session-wm-proc session)
               (process-live-p (k-exwm-nested-session-wm-proc session)))
      (kill-process (k-exwm-nested-session-wm-proc session)))
    (when (and (k-exwm-nested-session-clipboard-proc session)
               (process-live-p (k-exwm-nested-session-clipboard-proc session)))
      (kill-process (k-exwm-nested-session-clipboard-proc session)))
    (when (process-live-p (k-exwm-nested-session-xephyr-proc session))
      (kill-process (k-exwm-nested-session-xephyr-proc session)))
    (k-exwm-nested--remove-session display)
    (message "Stopped nested session :%d (%s)"
             display (k-exwm-nested-session-wm-name session))))

;;;###autoload
(defun k-exwm-nested-stop-all ()
  "Stop all active nested sessions."
  (interactive)
  (let ((count (length k-exwm-nested--sessions)))
    (dolist (session (copy-sequence k-exwm-nested--sessions))
      (k-exwm-nested-stop (k-exwm-nested-session-display session)))
    (message "Stopped %d nested session(s)" count)))

;;;###autoload
(defun k-exwm-nested-restart-wm (&optional display)
  "Restart the WM/DE in the nested session on DISPLAY.
This kills the current WM process and starts a new one using the
same command, without restarting Xephyr."
  (interactive
   (list (k-exwm-nested--completing-read-session "Restart WM in session: ")))
  (let ((session (k-exwm-nested--find-session display)))
    (unless session
      (user-error "No session on display :%d" display))
    ;; Kill old WM
    (when (and (k-exwm-nested-session-wm-proc session)
               (process-live-p (k-exwm-nested-session-wm-proc session)))
      (kill-process (k-exwm-nested-session-wm-proc session)))
    ;; Start new WM
    (let* ((display-str (k-exwm-nested-session-display-str session))
           (wm-command (k-exwm-nested-session-wm-command session))
           (wm-name (k-exwm-nested-session-wm-name session))
           (process-environment
            (cons (format "DISPLAY=%s" display-str)
                  process-environment))
           (wm-proc (start-process-shell-command
                     (format "nested-wm-%d" display)
                     (k-exwm-nested-session-log-buffer session)
                     wm-command)))
      (setf (k-exwm-nested-session-wm-proc session) wm-proc)
      (set-process-sentinel
       wm-proc
       (lambda (proc event)
         (unless (process-live-p proc)
           (message "Nested WM %s on %s exited: %s"
                    wm-name display-str (string-trim event)))))
      (message "Restarted %s on display %s" wm-name display-str))))

;;;###autoload
(defun k-exwm-nested-view-log (&optional display)
  "View the log buffer for the nested session on DISPLAY."
  (interactive
   (list (k-exwm-nested--completing-read-session "View log for session: ")))
  (let ((session (k-exwm-nested--find-session display)))
    (unless session
      (user-error "No session on display :%d" display))
    (pop-to-buffer (k-exwm-nested-session-log-buffer session))))


(defun k-exwm-nested--start-clipboard-sync (session)
  "Start clipboard synchronization for SESSION.
Uses `xclip' to periodically sync the X clipboard between the host
and the nested X server."
  (let* ((display-str (k-exwm-nested-session-display-str session))
         (script (format
                  "while true; do \
                     xclip -selection clipboard -o 2>/dev/null | \
                     DISPLAY=%s xclip -selection clipboard -i 2>/dev/null; \
                     DISPLAY=%s xclip -selection clipboard -o 2>/dev/null | \
                     xclip -selection clipboard -i 2>/dev/null; \
                     sleep 1; \
                   done"
                  display-str display-str))
         (proc (start-process-shell-command
                (format "clipboard-sync-%d" (k-exwm-nested-session-display session))
                nil script)))
    (setf (k-exwm-nested-session-clipboard-proc session) proc)
    (set-process-query-on-exit-flag proc nil)))

;;;###autoload
(defun k-exwm-nested-send-to-session (command &optional display)
  "Launch COMMAND so it opens in the nested session on DISPLAY.
This sets the DISPLAY environment variable so the application's
windows appear inside the nested Xephyr."
  (interactive
   (let ((disp (k-exwm-nested--completing-read-session "Send to session: ")))
     (list (read-string "Application command: ") disp)))
  (let* ((session (k-exwm-nested--find-session display))
         (display-str (k-exwm-nested-session-display-str session)))
    (start-process-shell-command
     "nested-app"
     (k-exwm-nested-session-log-buffer session)
     (format "DISPLAY=%s %s" display-str command))
    (message "Launched '%s' in nested session %s" command display-str)))


(defun k-exwm-nested--cleanup-on-exit ()
  "Kill all nested sessions when Emacs exits."
  (when k-exwm-nested--sessions
    (k-exwm-nested-stop-all)))

(add-hook 'kill-emacs-hook #'k-exwm-nested--cleanup-on-exit)

;; Don't prompt about live processes on exit for nested sessions
(dolist (name '("xephyr" "nested-wm" "nested-cmd" "clipboard-sync" "nested-app"))
  (add-hook 'kill-emacs-hook
            (lambda ()
              (dolist (proc (process-list))
                (when (string-prefix-p name (process-name proc))
                  (set-process-query-on-exit-flag proc nil))))))

(provide 'k-nested-exwm)
