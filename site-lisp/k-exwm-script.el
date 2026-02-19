;;; k-exwm-script.el --- Make every app scriptable from Emacs -*- lexical-binding: t -*-

;; Author: Keshav
;; URL: https://github.com/keshav25/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A unified Elisp API for programmatically controlling any X11
;; application running inside EXWM.  The goal: make every app as
;; scriptable as Emacs itself.
;;
;; This module combines multiple approaches:
;;
;;  1. xdotool   — send keystrokes, mouse events, window management
;;  2. xprop     — query window properties (class, title, PID, etc.)
;;  3. AT-SPI    — D-Bus accessibility API for reading UI widget trees,
;;                 finding buttons/text fields, activating controls
;;  4. OCR       — fall-back visual text extraction (via k-ocr.el)
;;  5. Clipboard — programmatic read/write via xclip
;;
;; Together these let you write Elisp that drives any application:
;;
;;   ;; Type into the focused app
;;   (k-script-type "Hello, World!")
;;
;;   ;; Send a keyboard shortcut
;;   (k-script-key "ctrl+s")
;;
;;   ;; Click on a button by its label (AT-SPI or OCR fallback)
;;   (k-script-click-on "Save")
;;
;;   ;; Read text from a text field via accessibility
;;   (k-script-a11y-get-text "search-entry")
;;
;;   ;; Compose a full automation
;;   (k-script-with-app "Firefox"
;;     (k-script-key "ctrl+l")      ; focus address bar
;;     (k-script-type "emacs.org")
;;     (k-script-key "Return"))
;;
;; Requirements:
;;   - xdotool  (apt install xdotool)
;;   - xprop    (apt install x11-utils)
;;   - xclip    (apt install xclip)
;;   - python3-atspi (apt install python3-atspi) for AT-SPI support
;;   - maim + tesseract for OCR fallback (see k-ocr.el)

;;; Code:

(require 'cl-lib)
(require 'dbus)

;; Forward declarations for optional k-ocr integration
(defvar k-ocr--last-text)
(declare-function k-ocr-click-text "k-ocr")
(declare-function k-ocr-window "k-ocr")
(declare-function k-ocr--extract-text "k-ocr")

;;;; ════════════════════════════════════════════════════════════════════
;;;; Customization
;;;; ════════════════════════════════════════════════════════════════════

(defgroup k-script nil
  "Scriptable application control for EXWM."
  :group 'exwm
  :prefix "k-script-")

(defcustom k-script-type-delay 12
  "Delay in milliseconds between keystrokes when typing.
Lower is faster but some apps may drop characters."
  :type 'integer
  :group 'k-script)

(defcustom k-script-default-pause 0.15
  "Default pause in seconds between scripted actions.
Gives applications time to respond."
  :type 'number
  :group 'k-script)

(defcustom k-script-use-a11y t
  "When non-nil, prefer AT-SPI accessibility over OCR when available."
  :type 'boolean
  :group 'k-script)

(defcustom k-script-a11y-helper
  (expand-file-name "site-lisp/k-exwm-a11y-helper.py" user-emacs-directory)
  "Path to the AT-SPI Python helper script."
  :type 'string
  :group 'k-script)

;;;; ════════════════════════════════════════════════════════════════════
;;;; 1. xdotool — Keyboard, Mouse, Window Control
;;;; ════════════════════════════════════════════════════════════════════

(defun k-script--check-xdotool ()
  "Error if xdotool is not available."
  (unless (executable-find "xdotool")
    (user-error "xdotool is not installed. Install with: sudo apt install xdotool")))

;;; Keyboard

;;;###autoload
(defun k-script-key (key-combo &optional window-id)
  "Send KEY-COMBO to the focused window (or WINDOW-ID).
KEY-COMBO uses xdotool syntax: \"ctrl+c\", \"alt+F4\", \"Return\",
\"ctrl+shift+t\", etc."
  (interactive "sKey combo (xdotool syntax): ")
  (k-script--check-xdotool)
  (if window-id
      (call-process "xdotool" nil nil nil
                    "key" "--window" (format "%d" window-id) key-combo)
    (call-process "xdotool" nil nil nil "key" key-combo)))

;;;###autoload
(defun k-script-key-sequence (keys &optional pause)
  "Send a sequence of KEYS with PAUSE seconds between each.
KEYS is a list of key-combo strings."
  (let ((pause (or pause k-script-default-pause)))
    (dolist (key keys)
      (k-script-key key)
      (sit-for pause))))

;;;###autoload
(defun k-script-type (text &optional window-id)
  "Type TEXT into the focused window (or WINDOW-ID) as keystrokes.
Handles special characters properly via xdotool."
  (interactive "sType text: ")
  (k-script--check-xdotool)
  (if window-id
      (call-process "xdotool" nil nil nil
                    "type" "--clearmodifiers"
                    "--delay" (number-to-string k-script-type-delay)
                    "--window" (format "%d" window-id)
                    text)
    (call-process "xdotool" nil nil nil
                  "type" "--clearmodifiers"
                  "--delay" (number-to-string k-script-type-delay)
                  text)))

;;;###autoload
(defun k-script-type-from-kill-ring ()
  "Select text from the kill ring and type it into the focused app.
Unlike clipboard paste, this simulates actual keystrokes."
  (interactive)
  (let ((text (completing-read "Type from kill ring: "
                               (cl-remove-duplicates
                                (mapcar #'substring-no-properties kill-ring)
                                :test #'equal)
                               nil t)))
    (when (and text (not (string-empty-p text)))
      (k-script-type text))))

;;; Mouse

;;;###autoload
(defun k-script-mouse-move (x y)
  "Move mouse to absolute screen position (X, Y)."
  (k-script--check-xdotool)
  (call-process "xdotool" nil nil nil
                "mousemove" (number-to-string x) (number-to-string y)))

;;;###autoload
(defun k-script-mouse-move-relative (dx dy)
  "Move mouse by DX, DY pixels relative to current position."
  (k-script--check-xdotool)
  (call-process "xdotool" nil nil nil
                "mousemove_relative" (number-to-string dx) (number-to-string dy)))

;;;###autoload
(defun k-script-click (&optional button x y)
  "Click mouse BUTTON (1=left,2=middle,3=right) at (X, Y) or current pos."
  (interactive)
  (k-script--check-xdotool)
  (when (and x y)
    (k-script-mouse-move x y)
    (sit-for 0.05))
  (call-process "xdotool" nil nil nil
                "click" (number-to-string (or button 1))))

;;;###autoload
(defun k-script-double-click (&optional x y)
  "Double-click left mouse button at (X, Y) or current position."
  (interactive)
  (when (and x y) (k-script-mouse-move x y) (sit-for 0.05))
  (call-process "xdotool" nil nil nil
                "click" "--repeat" "2" "--delay" "50" "1"))

;;;###autoload
(defun k-script-drag (x1 y1 x2 y2 &optional button)
  "Drag from (X1,Y1) to (X2,Y2) with BUTTON (default left)."
  (k-script--check-xdotool)
  (k-script-mouse-move x1 y1)
  (sit-for 0.05)
  (call-process "xdotool" nil nil nil
                "mousedown" (number-to-string (or button 1)))
  (sit-for 0.1)
  (k-script-mouse-move x2 y2)
  (sit-for 0.1)
  (call-process "xdotool" nil nil nil
                "mouseup" (number-to-string (or button 1))))

;;;###autoload
(defun k-script-scroll (direction &optional amount)
  "Scroll DIRECTION (up/down/left/right) by AMOUNT clicks (default 3)."
  (k-script--check-xdotool)
  (let ((button (pcase direction
                  ('up "4") ('down "5") ('left "6") ('right "7")
                  (_ "5")))
        (amount (or amount 3)))
    (call-process "xdotool" nil nil nil
                  "click" "--repeat" (number-to-string amount)
                  "--delay" "50" button)))

;;; Window Management via xdotool

;;;###autoload
(defun k-script-get-active-window-id ()
  "Return the X window ID of the currently focused window."
  (k-script--check-xdotool)
  (string-to-number
   (string-trim
    (with-output-to-string
      (with-current-buffer standard-output
        (call-process "xdotool" nil t nil "getactivewindow"))))))

;;;###autoload
(defun k-script-focus-window (window-id)
  "Focus the window with WINDOW-ID."
  (k-script--check-xdotool)
  (call-process "xdotool" nil nil nil
                "windowactivate" (format "%d" window-id)))

;;;###autoload
(defun k-script-find-window-by-name (name)
  "Find window IDs matching NAME (regex).  Returns list of IDs."
  (k-script--check-xdotool)
  (let ((output (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process "xdotool" nil t nil
                                   "search" "--name" name))))))
    (unless (string-empty-p output)
      (mapcar #'string-to-number (split-string output "\n")))))

;;;###autoload
(defun k-script-find-window-by-class (class)
  "Find window IDs matching CLASS name (regex).  Returns list of IDs."
  (k-script--check-xdotool)
  (let ((output (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process "xdotool" nil t nil
                                   "search" "--class" class))))))
    (unless (string-empty-p output)
      (mapcar #'string-to-number (split-string output "\n")))))

;;;###autoload
(defun k-script-minimize-window (&optional window-id)
  "Minimize WINDOW-ID or the active window."
  (k-script--check-xdotool)
  (if window-id
      (call-process "xdotool" nil nil nil
                    "windowminimize" (format "%d" window-id))
    (call-process "xdotool" nil nil nil
                  "windowminimize" (format "%d" (k-script-get-active-window-id)))))

;;;###autoload
(defun k-script-set-window-size (width height &optional window-id)
  "Resize WINDOW-ID (or active window) to WIDTH x HEIGHT pixels."
  (k-script--check-xdotool)
  (let ((wid (or window-id (k-script-get-active-window-id))))
    (call-process "xdotool" nil nil nil
                  "windowsize" (format "%d" wid)
                  (number-to-string width) (number-to-string height))))

;;;###autoload
(defun k-script-move-window (x y &optional window-id)
  "Move WINDOW-ID (or active window) to position (X, Y)."
  (k-script--check-xdotool)
  (let ((wid (or window-id (k-script-get-active-window-id))))
    (call-process "xdotool" nil nil nil
                  "windowmove" (format "%d" wid)
                  (number-to-string x) (number-to-string y))))

;;;; ════════════════════════════════════════════════════════════════════
;;;; 2. xprop — Window Property Queries
;;;; ════════════════════════════════════════════════════════════════════

(defun k-script--check-xprop ()
  "Error if xprop is not available."
  (unless (executable-find "xprop")
    (user-error "xprop not installed. Install with: sudo apt install x11-utils")))

;;;###autoload
(defun k-script-window-properties (&optional window-id)
  "Return alist of X properties for WINDOW-ID or the active window."
  (k-script--check-xprop)
  (let* ((wid (or window-id (k-script-get-active-window-id)))
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process "xprop" nil t nil
                                   "-id" (format "%d" wid)))))
         (props '()))
    (dolist (line (split-string output "\n"))
      (when (string-match "^\\([A-Z_]+\\)(\\([^)]*\\)) = \\(.*\\)" line)
        (push (cons (match-string 1 line) (match-string 3 line)) props)))
    (nreverse props)))

;;;###autoload
(defun k-script-window-class (&optional window-id)
  "Return the WM_CLASS of WINDOW-ID or the active window."
  (k-script--check-xprop)
  (let* ((wid (or window-id (k-script-get-active-window-id)))
         (output (string-trim
                  (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "xprop" nil t nil
                                    "-id" (format "%d" wid) "WM_CLASS"))))))
    (when (string-match "= \"\\([^\"]+\\)\", \"\\([^\"]+\\)\"" output)
      (cons (match-string 1 output) (match-string 2 output)))))

;;;###autoload
(defun k-script-window-title (&optional window-id)
  "Return the window title (WM_NAME) of WINDOW-ID or active window."
  (k-script--check-xprop)
  (let* ((wid (or window-id (k-script-get-active-window-id)))
         (output (string-trim
                  (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "xprop" nil t nil
                                    "-id" (format "%d" wid) "_NET_WM_NAME"))))))
    (when (string-match "= \"\\(.+\\)\"" output)
      (match-string 1 output))))

;;;###autoload
(defun k-script-window-pid (&optional window-id)
  "Return the PID of the process owning WINDOW-ID or active window."
  (k-script--check-xprop)
  (let* ((wid (or window-id (k-script-get-active-window-id)))
         (output (string-trim
                  (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "xprop" nil t nil
                                    "-id" (format "%d" wid) "_NET_WM_PID"))))))
    (when (string-match "= \\([0-9]+\\)" output)
      (string-to-number (match-string 1 output)))))

;;;###autoload
(defun k-script-window-geometry (&optional window-id)
  "Return geometry plist (:x :y :width :height) for WINDOW-ID."
  (k-script--check-xdotool)
  (let* ((wid (or window-id (k-script-get-active-window-id)))
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process "xdotool" nil t nil
                                   "getwindowgeometry" "--shell"
                                   (format "%d" wid)))))
         (x 0) (y 0) (w 0) (h 0))
    (when (string-match "X=\\([0-9]+\\)" output) (setq x (string-to-number (match-string 1 output))))
    (when (string-match "Y=\\([0-9]+\\)" output) (setq y (string-to-number (match-string 1 output))))
    (when (string-match "WIDTH=\\([0-9]+\\)" output) (setq w (string-to-number (match-string 1 output))))
    (when (string-match "HEIGHT=\\([0-9]+\\)" output) (setq h (string-to-number (match-string 1 output))))
    (list :x x :y y :width w :height h)))

;;;; ════════════════════════════════════════════════════════════════════
;;;; 3. AT-SPI — Accessibility API (D-Bus)
;;;; ════════════════════════════════════════════════════════════════════

;; AT-SPI (Assistive Technology Service Provider Interface) provides
;; programmatic access to the UI widget tree of running applications.
;; This is how screen readers work, but we use it for scripting.
;;
;; We interface with AT-SPI through D-Bus, which Emacs supports natively.
;; For complex tree traversal, we fall back to a small Python helper.

(defconst k-script--atspi-bus "org.a11y.Bus"
  "D-Bus service name for AT-SPI.")

(defconst k-script--atspi-registry-path "/org/a11y/atspi/accessible/root"
  "D-Bus path for AT-SPI registry root.")

(defun k-script--atspi-available-p ()
  "Return non-nil if AT-SPI accessibility is available."
  (condition-case nil
      (dbus-call-method :session
                        "org.a11y.Bus"
                        "/org/a11y/bus"
                        "org.a11y.Bus"
                        "GetAddress")
    (error nil)))

(defun k-script--a11y-helper-available-p ()
  "Return non-nil if the Python AT-SPI helper is available."
  (and (executable-find "python3")
       ;; Check if pyatspi2 is installed
       (= 0 (call-process "python3" nil nil nil
                          "-c" "import pyatspi"))))

;;;###autoload
(defun k-script-a11y-list-apps ()
  "List accessible applications via AT-SPI.
Returns a list of (NAME . PID) cons cells."
  (if (k-script--a11y-helper-available-p)
      (let ((output (with-output-to-string
                      (with-current-buffer standard-output
                        (call-process "python3" nil t nil "-c"
                                      "import pyatspi
desktop = pyatspi.Registry.getDesktop(0)
for app in desktop:
    try:
        print(f'{app.name}\\t{app.get_process_id()}')
    except:
        pass")))))
        (cl-remove-if
         #'null
         (mapcar (lambda (line)
                   (let ((parts (split-string line "\t")))
                     (when (= (length parts) 2)
                       (cons (nth 0 parts)
                             (string-to-number (nth 1 parts))))))
                 (split-string (string-trim output) "\n"))))
    (message "AT-SPI not available (python3-atspi not installed)")
    nil))

;;;###autoload
(defun k-script-a11y-dump-tree (&optional app-name)
  "Dump the accessible UI tree of APP-NAME (or focused app) into a buffer.
This shows every widget, button, text field, label, etc. that the
application exposes — making it fully scriptable."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Application: "
                            (mapcar #'car (k-script-a11y-list-apps))
                            nil t))))
  (if (k-script--a11y-helper-available-p)
      (let* ((py-code (format "
import pyatspi
import sys

def find_app(name=None):
    desktop = pyatspi.Registry.getDesktop(0)
    if name:
        for app in desktop:
            try:
                if app.name and name.lower() in app.name.lower():
                    return app
            except:
                pass
        return None
    else:
        # Get focused app
        for app in desktop:
            try:
                for child in app:
                    if child.getState().contains(pyatspi.STATE_ACTIVE):
                        return app
            except:
                pass
    return None

def dump_tree(node, indent=0):
    try:
        role = node.getRoleName()
        name = node.name or ''
        desc = node.description or ''
        text = ''
        try:
            ti = node.queryText()
            if ti:
                text = ti.getText(0, ti.characterCount)[:200]
        except:
            pass
        state = node.getState()
        states = []
        if state.contains(pyatspi.STATE_FOCUSED): states.append('focused')
        if state.contains(pyatspi.STATE_VISIBLE): states.append('visible')
        if state.contains(pyatspi.STATE_ENABLED): states.append('enabled')
        if state.contains(pyatspi.STATE_EDITABLE): states.append('editable')
        state_str = ','.join(states)
        line = '  ' * indent + f'[{role}]'
        if name: line += f' name=\"{name}\"'
        if text and text != name: line += f' text=\"{text}\"'
        if state_str: line += f' ({state_str})'
        print(line)
        for i in range(node.childCount):
            try:
                dump_tree(node.getChildAtIndex(i), indent + 1)
            except:
                pass
    except Exception as e:
        print('  ' * indent + f'[error: {e}]')

app = find_app(%s)
if app:
    print(f'Application: {app.name} (PID: {app.get_process_id()})')
    print('=' * 60)
    dump_tree(app)
else:
    print('No matching application found')
" (if app-name (format "'%s'" app-name) "None")))
             (output (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "python3" nil t nil "-c" py-code)))))
        (with-current-buffer (get-buffer-create
                              (format "*A11y Tree: %s*" (or app-name "focused")))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output))
          (goto-char (point-min))
          (special-mode)
          (pop-to-buffer (current-buffer))))
    (user-error "python3-atspi not available. Install with: sudo apt install python3-atspi")))

;;;###autoload
(defun k-script-a11y-find-widget (role-or-name &optional app-name)
  "Find a widget by ROLE-OR-NAME in APP-NAME (or focused app).
Returns a plist with :name :role :text :x :y :width :height or nil."
  (when (k-script--a11y-helper-available-p)
    (let* ((py-code (format "
import pyatspi
import json

def find_app(name=None):
    desktop = pyatspi.Registry.getDesktop(0)
    if name:
        for app in desktop:
            try:
                if app.name and name.lower() in app.name.lower():
                    return app
            except:
                pass
    else:
        for app in desktop:
            try:
                for child in app:
                    if child.getState().contains(pyatspi.STATE_ACTIVE):
                        return app
            except:
                pass
    return None

def find_widget(node, target):
    try:
        role = node.getRoleName()
        name = node.name or ''
        if target.lower() in name.lower() or target.lower() in role.lower():
            try:
                comp = node.queryComponent()
                ext = comp.getExtents(pyatspi.DESKTOP_COORDS)
                result = {
                    'name': name, 'role': role,
                    'x': ext.x, 'y': ext.y,
                    'width': ext.width, 'height': ext.height
                }
                try:
                    ti = node.queryText()
                    if ti:
                        result['text'] = ti.getText(0, min(ti.characterCount, 500))
                except:
                    pass
                return result
            except:
                pass
        for i in range(node.childCount):
            try:
                r = find_widget(node.getChildAtIndex(i), target)
                if r:
                    return r
            except:
                pass
    except:
        pass
    return None

app = find_app(%s)
if app:
    result = find_widget(app, '%s')
    if result:
        print(json.dumps(result))
    else:
        print('null')
else:
    print('null')
" (if app-name (format "'%s'" app-name) "None")
  role-or-name))
           (output (string-trim
                    (with-output-to-string
                      (with-current-buffer standard-output
                        (call-process "python3" nil t nil "-c" py-code))))))
      (unless (or (string= output "null") (string-empty-p output))
        (let ((json-object-type 'plist)
              (json-key-type 'keyword))
          (json-read-from-string output))))))

;;;###autoload
(defun k-script-a11y-get-text (widget-name &optional app-name)
  "Get the text content of WIDGET-NAME via AT-SPI.
This can read text from text fields, labels, etc. in any application."
  (interactive "sWidget name: ")
  (let ((widget (k-script-a11y-find-widget widget-name app-name)))
    (when widget
      (or (plist-get widget :text)
          (plist-get widget :name)))))

;;;###autoload
(defun k-script-a11y-click-widget (widget-name &optional app-name)
  "Click on a widget found via AT-SPI by WIDGET-NAME."
  (interactive "sClick widget: ")
  (let ((widget (k-script-a11y-find-widget widget-name app-name)))
    (if widget
        (let ((x (+ (plist-get widget :x)
                    (/ (plist-get widget :width) 2)))
              (y (+ (plist-get widget :y)
                    (/ (plist-get widget :height) 2))))
          (k-script-click 1 x y)
          (message "Clicked '%s' at (%d, %d)" widget-name x y))
      (message "Widget '%s' not found via AT-SPI" widget-name))))

;;;; ════════════════════════════════════════════════════════════════════
;;;; 4. Clipboard
;;;; ════════════════════════════════════════════════════════════════════

;;;###autoload
(defun k-script-clipboard-get ()
  "Get the current system clipboard content."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (call-process "xclip" nil t nil "-selection" "clipboard" "-o")))))

;;;###autoload
(defun k-script-clipboard-set (text)
  "Set the system clipboard to TEXT."
  (let ((proc (start-process "xclip-set" nil
                             "xclip" "-selection" "clipboard")))
    (process-send-string proc text)
    (process-send-eof proc)))

;;;###autoload
(defun k-script-paste (&optional text)
  "Paste TEXT (or clipboard content) into the focused application.
Sets the clipboard to TEXT if provided, then simulates Ctrl+V."
  (when text (k-script-clipboard-set text))
  (sit-for 0.05)
  (k-script-key "ctrl+v"))

;;;###autoload
(defun k-script-copy ()
  "Simulate Ctrl+C in the focused application and return the clipboard."
  (k-script-key "ctrl+c")
  (sit-for 0.2)
  (k-script-clipboard-get))

;;;; ════════════════════════════════════════════════════════════════════
;;;; 5. High-Level Unified API
;;;; ════════════════════════════════════════════════════════════════════

;;;###autoload
(defun k-script-click-on (label &optional app-name)
  "Click on a UI element identified by LABEL.
Tries AT-SPI first (if available), falls back to OCR.
This is the primary way to interact with application UI elements."
  (interactive "sClick on: ")
  (cond
   ;; Try AT-SPI first
   ((and k-script-use-a11y (k-script--a11y-helper-available-p))
    (let ((widget (k-script-a11y-find-widget label app-name)))
      (if widget
          (let ((x (+ (plist-get widget :x) (/ (plist-get widget :width) 2)))
                (y (+ (plist-get widget :y) (/ (plist-get widget :height) 2))))
            (k-script-click 1 x y)
            (message "Clicked '%s' via AT-SPI at (%d, %d)" label x y))
        ;; AT-SPI didn't find it, fall back to OCR
        (if (fboundp 'k-ocr-click-text)
            (k-ocr-click-text label)
          (message "Widget '%s' not found" label)))))
   ;; OCR fallback
   ((fboundp 'k-ocr-click-text)
    (k-ocr-click-text label))
   (t (message "No method available to find '%s'" label))))

;;;###autoload
(defun k-script-read-text (&optional app-name)
  "Read all visible text from the focused (or APP-NAME) application.
Uses AT-SPI if available, falls back to OCR."
  (interactive)
  (cond
   ((and k-script-use-a11y (k-script--a11y-helper-available-p))
    (let* ((py-code (format "
import pyatspi

def find_app(name=None):
    desktop = pyatspi.Registry.getDesktop(0)
    if name:
        for app in desktop:
            try:
                if app.name and name.lower() in app.name.lower():
                    return app
            except:
                pass
    else:
        for app in desktop:
            try:
                for child in app:
                    if child.getState().contains(pyatspi.STATE_ACTIVE):
                        return app
            except:
                pass
    return None

def collect_text(node, texts=None):
    if texts is None: texts = []
    try:
        ti = node.queryText()
        if ti and ti.characterCount > 0:
            t = ti.getText(0, ti.characterCount)
            if t.strip():
                texts.append(t)
    except:
        if node.name and node.name.strip():
            texts.append(node.name)
    try:
        for i in range(node.childCount):
            collect_text(node.getChildAtIndex(i), texts)
    except:
        pass
    return texts

app = find_app(%s)
if app:
    texts = collect_text(app)
    print('\\n'.join(texts))
" (if app-name (format "'%s'" app-name) "None")))
           (output (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "python3" nil t nil "-c" py-code)))))
      (string-trim output)))
   ;; OCR fallback
   ((fboundp 'k-ocr-window)
    (k-ocr-window)
    k-ocr--last-text)
   (t (user-error "No text reading method available"))))

;;;###autoload
(defun k-script-read-text-to-buffer (&optional app-name)
  "Read text from the focused app and display in a buffer."
  (interactive)
  (let ((text (k-script-read-text app-name))
        (name (or app-name
                  (and (boundp 'exwm-title) exwm-title)
                  "app")))
    (with-current-buffer (get-buffer-create (format "*App Text: %s*" name))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun k-script-app-info ()
  "Show comprehensive info about the focused application.
Combines xprop, xdotool, and optionally AT-SPI data."
  (interactive)
  (let* ((wid (k-script-get-active-window-id))
         (class (ignore-errors (k-script-window-class wid)))
         (title (ignore-errors (k-script-window-title wid)))
         (pid (ignore-errors (k-script-window-pid wid)))
         (geom (ignore-errors (k-script-window-geometry wid)))
         (a11y-apps (when (k-script--a11y-helper-available-p)
                      (ignore-errors (k-script-a11y-list-apps)))))
    (with-current-buffer (get-buffer-create "*App Info*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Application Info\n" 'face 'bold)
                (make-string 40 ?─) "\n\n")
        (insert (format "  Window ID:  %d (0x%x)\n" wid wid))
        (when class
          (insert (format "  Class:      %s, %s\n" (car class) (cdr class))))
        (when title
          (insert (format "  Title:      %s\n" title)))
        (when pid
          (insert (format "  PID:        %d\n" pid))
          (let ((cmdline (ignore-errors
                           (string-trim
                            (with-output-to-string
                              (with-current-buffer standard-output
                                (call-process "cat" nil t nil
                                              (format "/proc/%d/cmdline" pid))))))))
            (when cmdline
              (insert (format "  Command:    %s\n"
                              (replace-regexp-in-string "\0" " " cmdline))))))
        (when geom
          (insert (format "  Geometry:   %dx%d+%d+%d\n"
                          (plist-get geom :width) (plist-get geom :height)
                          (plist-get geom :x) (plist-get geom :y))))
        (insert "\n")
        (if (k-script--a11y-helper-available-p)
            (insert "  AT-SPI:     ✓ available\n")
          (insert "  AT-SPI:     ✗ not available\n"))
        (when (fboundp 'k-ocr--extract-text)
          (insert "  OCR:        ✓ available\n")))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;;; ════════════════════════════════════════════════════════════════════
;;;; 6. Scripting Macros and Composition
;;;; ════════════════════════════════════════════════════════════════════

;;;###autoload
(defmacro k-script-with-app (app-name &rest body)
  "Focus the application matching APP-NAME, then execute BODY.
After BODY completes, does NOT switch focus back.

Example:
  (k-script-with-app \"Firefox\"
    (k-script-key \"ctrl+l\")
    (k-script-type \"https://emacs.org\")
    (k-script-key \"Return\"))"
  (declare (indent 1))
  `(let ((wids (k-script-find-window-by-class ,app-name)))
     (if wids
         (progn
           (k-script-focus-window (car wids))
           (sit-for k-script-default-pause)
           ,@body)
       (user-error "Application '%s' not found" ,app-name))))

;;;###autoload
(defmacro k-script-with-window (window-id &rest body)
  "Focus WINDOW-ID, then execute BODY."
  (declare (indent 1))
  `(progn
     (k-script-focus-window ,window-id)
     (sit-for k-script-default-pause)
     ,@body))

;;;###autoload
(defun k-script-chain (&rest actions)
  "Execute ACTIONS sequentially with pauses between them.
Each action is a function taking no arguments."
  (dolist (action actions)
    (funcall action)
    (sit-for k-script-default-pause)))

;;;###autoload
(defun k-script-repeat (n action &optional pause)
  "Execute ACTION function N times with PAUSE seconds between."
  (let ((pause (or pause k-script-default-pause)))
    (dotimes (_ n)
      (funcall action)
      (sit-for pause))))

;;;; ════════════════════════════════════════════════════════════════════
;;;; 7. Interactive Scripting REPL
;;;; ════════════════════════════════════════════════════════════════════

(defvar k-script--repl-target nil
  "Window ID of the target app for the scripting REPL.")

;;;###autoload
(defun k-script-repl ()
  "Start an interactive scripting REPL for the current EXWM application.
This opens a minibuffer loop where you can type commands to send
to the focused application:

  type: Hello     — types text
  key: ctrl+s     — sends keystroke
  click: Save     — clicks on element
  read            — reads visible text
  tree            — dumps AT-SPI tree
  info            — shows app info
  quit            — exit REPL"
  (interactive)
  (setq k-script--repl-target
        (and (derived-mode-p 'exwm-mode)
             (boundp 'exwm--id)
             exwm--id))
  (let ((running t))
    (while running
      (let ((cmd (read-string "k-script> ")))
        (cond
         ((string-prefix-p "type: " cmd)
          (let ((text (substring cmd 6)))
            (when k-script--repl-target
              (k-script-focus-window k-script--repl-target)
              (sit-for 0.1))
            (k-script-type text)))
         ((string-prefix-p "key: " cmd)
          (let ((key (string-trim (substring cmd 5))))
            (when k-script--repl-target
              (k-script-focus-window k-script--repl-target)
              (sit-for 0.1))
            (k-script-key key)))
         ((string-prefix-p "click: " cmd)
          (k-script-click-on (string-trim (substring cmd 7))))
         ((string= cmd "read")
          (message "%s" (k-script-read-text)))
         ((string= cmd "tree")
          (k-script-a11y-dump-tree))
         ((string= cmd "info")
          (k-script-app-info))
         ((string= cmd "copy")
          (message "Clipboard: %s" (k-script-copy)))
         ((string-prefix-p "paste: " cmd)
          (k-script-paste (substring cmd 7)))
         ((or (string= cmd "quit") (string= cmd "q") (string-empty-p cmd))
          (setq running nil))
         (t (message "Unknown command. Try: type: key: click: read tree info copy paste: quit")))))))

;;;; ════════════════════════════════════════════════════════════════════
;;;; 8. Dependency Check
;;;; ════════════════════════════════════════════════════════════════════

;;;###autoload
(defun k-script-check-dependencies ()
  "Check all dependencies for the EXWM scripting system."
  (interactive)
  (let ((required '(("xdotool" . "xdotool")
                    ("xprop" . "x11-utils")
                    ("xclip" . "xclip")))
        (ocr '(("tesseract" . "tesseract-ocr")
               ("maim" . "maim")))
        (a11y-ok (k-script--a11y-helper-available-p))
        (missing-req '())
        (missing-ocr '()))
    (dolist (dep required)
      (unless (executable-find (car dep))
        (push (cdr dep) missing-req)))
    (dolist (dep ocr)
      (unless (executable-find (car dep))
        (push (cdr dep) missing-ocr)))
    (with-current-buffer (get-buffer-create "*Script Dependencies*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "EXWM Scripting System — Dependencies\n" 'face 'bold)
                (make-string 45 ?─) "\n\n")
        ;; Required
        (if missing-req
            (insert (format "❌ Missing (required): %s\n   sudo apt install %s\n\n"
                            (string-join missing-req ", ")
                            (string-join missing-req " ")))
          (insert "✓ Core (xdotool, xprop, xclip)\n\n"))
        ;; OCR
        (if missing-ocr
            (insert (format "⚠ Missing (OCR): %s\n   sudo apt install %s\n\n"
                            (string-join missing-ocr ", ")
                            (string-join missing-ocr " ")))
          (insert "✓ OCR (tesseract, maim)\n\n"))
        ;; AT-SPI
        (if a11y-ok
            (insert "✓ AT-SPI accessibility (python3-atspi)\n\n")
          (insert "⚠ AT-SPI not available\n   sudo apt install python3-atspi\n\n"))
        ;; Summary
        (insert (make-string 45 ?─) "\n")
        (insert "\nCapabilities:\n")
        (insert "  • Keyboard/mouse control (xdotool)  "
                (if (executable-find "xdotool") "✓\n" "✗\n"))
        (insert "  • Window properties (xprop)         "
                (if (executable-find "xprop") "✓\n" "✗\n"))
        (insert "  • Clipboard (xclip)                 "
                (if (executable-find "xclip") "✓\n" "✗\n"))
        (insert "  • Visual text extraction (OCR)      "
                (if (and (executable-find "tesseract")
                         (executable-find "maim")) "✓\n" "✗\n"))
        (insert "  • UI widget tree (AT-SPI)           "
                (if a11y-ok "✓\n" "✗\n")))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'k-exwm-script)
;;; k-exwm-script.el ends here
