;;; k-exwm-apps.el --- Application-specific EXWM automation -*- lexical-binding: t -*-

;; Author: Keshav
;; URL: https://github.com/keshav25/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Practical automation scripts for specific applications using the
;; k-exwm-script and k-ocr systems.  Demonstrates how to make any
;; application as scriptable as Emacs.
;;
;; Currently includes:
;;   1. Brave Browser — tab/URL/search automation, page scraping
;;   2. KOTOR (Knights of the Old Republic) — dialog reader, journal
;;      extractor, auto-save, stat monitor, quickslot management
;;
;; Each section shows real scripting patterns you can adapt for any app.

;;; Code:

(require 'cl-lib)
(require 'k-exwm-script)
(require 'k-ocr)

;; ══════════════════════════════════════════════════════════════════════
;;
;;  ██████╗ ██████╗  █████╗ ██╗   ██╗███████╗
;;  ██╔══██╗██╔══██╗██╔══██╗██║   ██║██╔════╝
;;  ██████╔╝██████╔╝███████║██║   ██║█████╗
;;  ██╔══██╗██╔══██╗██╔══██║╚██╗ ██╔╝██╔══╝
;;  ██████╔╝██║  ██║██║  ██║ ╚████╔╝ ███████╗
;;  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝  ╚═══╝  ╚══════╝
;;
;;  Brave Browser Automation
;;
;; ══════════════════════════════════════════════════════════════════════

(defgroup k-app-brave nil
  "Brave browser automation."
  :group 'k-script
  :prefix "k-brave-")

(defcustom k-brave-class "Brave-browser"
  "X window class name for Brave browser."
  :type 'string
  :group 'k-app-brave)

(defcustom k-brave-command "brave-browser"
  "Command to launch Brave."
  :type 'string
  :group 'k-app-brave)

;;; Brave — Finding & Launching

(defun k-brave--find-buffer ()
  "Find the EXWM buffer for Brave, or nil."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (and (derived-mode-p 'exwm-mode)
                       (boundp 'exwm-class-name)
                       (string-match-p "Brave" (or exwm-class-name "")))))
              (buffer-list)))

(defun k-brave--focus ()
  "Focus the Brave browser window.  Launch it if not running."
  (let ((buf (k-brave--find-buffer)))
    (if buf
        (progn (pop-to-buffer buf) (sit-for 0.1))
      (start-process "brave" nil k-brave-command)
      (sit-for 2))))

;;;###autoload
(defun k-brave-open-url (url)
  "Open URL in Brave browser."
  (interactive "sURL: ")
  (k-brave--focus)
  (k-script-key "ctrl+l")
  (sit-for 0.2)
  (k-script-key "ctrl+a")
  (sit-for 0.05)
  (k-script-type url)
  (sit-for 0.1)
  (k-script-key "Return"))

;;;###autoload
(defun k-brave-search (query)
  "Search for QUERY in Brave browser."
  (interactive "sSearch: ")
  (k-brave-open-url (format "https://search.brave.com/search?q=%s"
                            (url-hexify-string query))))

;;;###autoload
(defun k-brave-search-region (beg end)
  "Search the selected region text in Brave."
  (interactive "r")
  (k-brave-search (buffer-substring-no-properties beg end)))

;;; Brave — Tab Management

;;;###autoload
(defun k-brave-new-tab (&optional url)
  "Open a new tab in Brave, optionally navigating to URL."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+t")
  (when url
    (sit-for 0.3)
    (k-script-type url)
    (sit-for 0.1)
    (k-script-key "Return")))

;;;###autoload
(defun k-brave-close-tab ()
  "Close the current Brave tab."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+w"))

;;;###autoload
(defun k-brave-next-tab ()
  "Switch to the next Brave tab."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+Tab"))

;;;###autoload
(defun k-brave-prev-tab ()
  "Switch to the previous Brave tab."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+shift+Tab"))

;;;###autoload
(defun k-brave-tab-n (n)
  "Switch to Brave tab number N (1-8, 9=last)."
  (interactive "nTab number (1-9): ")
  (k-brave--focus)
  (k-script-key (format "ctrl+%d" (min 9 (max 1 n)))))

;;;###autoload
(defun k-brave-reopen-tab ()
  "Reopen the last closed Brave tab."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+shift+t"))

;;;###autoload
(defun k-brave-duplicate-tab ()
  "Duplicate the current Brave tab."
  (interactive)
  (k-brave--focus)
  ;; Focus address bar, select all, copy, new tab, paste, enter
  (k-script-key "ctrl+l")
  (sit-for 0.2)
  (k-script-key "ctrl+c")
  (sit-for 0.1)
  (k-script-key "ctrl+t")
  (sit-for 0.3)
  (k-script-key "ctrl+v")
  (sit-for 0.1)
  (k-script-key "Return"))

;;; Brave — Page Interaction

;;;###autoload
(defun k-brave-get-url ()
  "Get the current URL from Brave's address bar and copy to kill-ring."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+l")
  (sit-for 0.2)
  (k-script-key "ctrl+c")
  (sit-for 0.1)
  (k-script-key "Escape")
  (let ((url (k-script-clipboard-get)))
    (kill-new url)
    (message "URL: %s" url)
    url))

;;;###autoload
(defun k-brave-get-page-text ()
  "Extract all visible text from the current Brave page.
Uses Ctrl+A, Ctrl+C to grab page text, then restores clipboard."
  (interactive)
  (k-brave--focus)
  (let ((old-clip (ignore-errors (k-script-clipboard-get))))
    (k-script-key "ctrl+a")
    (sit-for 0.2)
    (k-script-key "ctrl+c")
    (sit-for 0.3)
    (let ((text (k-script-clipboard-get)))
      ;; Restore old clipboard
      (when old-clip (k-script-clipboard-set old-clip))
      (k-script-key "Escape") ; deselect
      (with-current-buffer (get-buffer-create "*Brave Page Text*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text))
        (goto-char (point-min))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun k-brave-save-page-org ()
  "Save the current Brave page title and URL as an org-mode link."
  (interactive)
  (k-brave--focus)
  ;; Get title from window
  (let* ((title (or (and (boundp 'exwm-title) exwm-title) ""))
         ;; Remove " - Brave" suffix
         (title (replace-regexp-in-string " [-–—] Brave$" "" title)))
    ;; Get URL
    (k-script-key "ctrl+l")
    (sit-for 0.2)
    (k-script-key "ctrl+c")
    (sit-for 0.1)
    (k-script-key "Escape")
    (let* ((url (k-script-clipboard-get))
           (org-link (format "[[%s][%s]]" url title)))
      (kill-new org-link)
      (message "Org link copied: %s" org-link))))

;;;###autoload
(defun k-brave-reader-mode ()
  "Toggle Brave's reader/distill mode for the current page."
  (interactive)
  (k-brave--focus)
  ;; Brave doesn't have a default reader shortcut, use the address bar
  (k-script-key "ctrl+l")
  (sit-for 0.2)
  (let ((url (progn (k-script-key "ctrl+c")
                    (sit-for 0.1)
                    (k-script-clipboard-get))))
    (k-script-key "ctrl+a")
    ;; Navigate to the reader version
    (if (string-prefix-p "chrome-distiller://" url)
        ;; Already in reader mode — go back
        (k-script-key "alt+Left")
      (progn
        (k-script-key "Escape")
        (k-script-key "F9")))))

;;;###autoload
(defun k-brave-bookmark ()
  "Bookmark the current Brave page."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+d"))

;;;###autoload
(defun k-brave-find-in-page (text)
  "Find TEXT in the current Brave page."
  (interactive "sFind in page: ")
  (k-brave--focus)
  (k-script-key "ctrl+f")
  (sit-for 0.2)
  (k-script-type text))

;;;###autoload
(defun k-brave-devtools ()
  "Open Brave DevTools."
  (interactive)
  (k-brave--focus)
  (k-script-key "F12"))

;;;###autoload
(defun k-brave-screenshot ()
  "Take a screenshot of the Brave window via OCR screenshot."
  (interactive)
  (k-brave--focus)
  (let ((buf (k-brave--find-buffer)))
    (when buf
      (with-current-buffer buf
        (let ((file (expand-file-name
                     (format "brave-%s.png"
                             (format-time-string "%Y%m%d-%H%M%S"))
                     "~/Pictures")))
          (k-ocr--screenshot-window file)
          (message "Screenshot saved: %s" file))))))

;;;###autoload
(defun k-brave-ocr-page ()
  "OCR the current Brave page and display in a buffer.
Useful when the page content is in images or can't be selected."
  (interactive)
  (k-brave--focus)
  (sit-for 0.2)
  (k-ocr-window-to-buffer))

;;; Brave — Quick Actions

;;;###autoload
(defun k-brave-private-window ()
  "Open a new Brave private window."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+shift+n"))

;;;###autoload
(defun k-brave-history ()
  "Open Brave history."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+h"))

;;;###autoload
(defun k-brave-downloads ()
  "Open Brave downloads."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+j"))

;;;###autoload
(defun k-brave-zoom-in ()
  "Zoom in on the Brave page."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+equal"))

;;;###autoload
(defun k-brave-zoom-out ()
  "Zoom out on the Brave page."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+minus"))

;;;###autoload
(defun k-brave-zoom-reset ()
  "Reset Brave page zoom to 100%."
  (interactive)
  (k-brave--focus)
  (k-script-key "ctrl+0"))

;;;###autoload
(defun k-brave-fullscreen ()
  "Toggle fullscreen in Brave."
  (interactive)
  (k-brave--focus)
  (k-script-key "F11"))


;; ══════════════════════════════════════════════════════════════════════
;;
;;  ██╗  ██╗ ██████╗ ████████╗ ██████╗ ██████╗
;;  ██║ ██╔╝██╔═══██╗╚══██╔══╝██╔═══██╗██╔══██╗
;;  █████╔╝ ██║   ██║   ██║   ██║   ██║██████╔╝
;;  ██╔═██╗ ██║   ██║   ██║   ██║   ██║██╔══██╗
;;  ██║  ██╗╚██████╔╝   ██║   ╚██████╔╝██║  ██║
;;  ╚═╝  ╚═╝ ╚═════╝    ╚═╝    ╚═════╝ ╚═╝  ╚═╝
;;
;;  Knights of the Old Republic — Game Automation
;;
;; ══════════════════════════════════════════════════════════════════════

(defgroup k-app-kotor nil
  "KOTOR game automation via EXWM scripting."
  :group 'k-script
  :prefix "k-kotor-")

(defcustom k-kotor-steam-id "32370"
  "Steam app ID for KOTOR."
  :type 'string
  :group 'k-app-kotor)

(defcustom k-kotor-game-dir
  (expand-file-name
   "~/.local/share/Steam/steamapps/common/swkotor/")
  "KOTOR installation directory."
  :type 'directory
  :group 'k-app-kotor)

(defcustom k-kotor-save-dir
  (expand-file-name
   "~/.local/share/Steam/steamapps/compatdata/32370/pfx/drive_c/users/steamuser/Saved Games/")
  "KOTOR save game directory (Proton prefix)."
  :type 'directory
  :group 'k-app-kotor)

(defcustom k-kotor-screenshot-dir "~/Pictures/kotor/"
  "Directory to store KOTOR screenshots."
  :type 'directory
  :group 'k-app-kotor)

(defcustom k-kotor-auto-save-interval 300
  "Auto-save interval in seconds (default: 5 minutes)."
  :type 'integer
  :group 'k-app-kotor)

(defcustom k-kotor-window-class "swkotor.exe"
  "X window class pattern for the KOTOR window."
  :type 'string
  :group 'k-app-kotor)

;;; KOTOR — Internal State

(defvar k-kotor--auto-save-timer nil
  "Timer for KOTOR auto-save feature.")

(defvar k-kotor--dialog-history nil
  "History of OCR'd dialog text from KOTOR sessions.")

(defvar k-kotor--stat-history nil
  "History of character stat snapshots.")

(defvar k-kotor--session-start nil
  "Time when the current KOTOR session started.")

;;; KOTOR — Window Management

(defun k-kotor--find-buffer ()
  "Find the EXWM buffer running KOTOR, or nil."
  (cl-find-if
   (lambda (buf)
     (with-current-buffer buf
       (and (derived-mode-p 'exwm-mode)
            (boundp 'exwm-class-name)
            (or (and exwm-class-name
                     (string-match-p
                      (regexp-quote k-kotor-window-class)
                      exwm-class-name))
                (and (boundp 'exwm-title) exwm-title
                     (string-match-p "Star Wars" (or exwm-title "")))))))
   (buffer-list)))

(defun k-kotor--focus ()
  "Focus the KOTOR game window.
Returns non-nil if KOTOR was found and focused."
  (let ((buf (k-kotor--find-buffer)))
    (when buf
      (pop-to-buffer buf)
      (sit-for 0.15)
      t)))

(defun k-kotor--running-p ()
  "Return non-nil if KOTOR appears to be running."
  (not (null (k-kotor--find-buffer))))

;;;###autoload
(defun k-kotor-launch ()
  "Launch KOTOR via Steam."
  (interactive)
  (if (k-kotor--running-p)
      (progn (k-kotor--focus)
             (message "KOTOR is already running"))
    (start-process "kotor" nil "steam"
                   (format "steam://rungameid/%s" k-kotor-steam-id))
    (setq k-kotor--session-start (current-time))
    (message "Launching KOTOR...")))

;;; KOTOR — Screenshots & OCR

(defun k-kotor--ensure-screenshot-dir ()
  "Create screenshot directory if needed."
  (unless (file-directory-p k-kotor-screenshot-dir)
    (make-directory k-kotor-screenshot-dir t)))

;;;###autoload
(defun k-kotor-screenshot ()
  "Take a screenshot of KOTOR and save to `k-kotor-screenshot-dir'."
  (interactive)
  (k-kotor--ensure-screenshot-dir)
  (let ((buf (k-kotor--find-buffer)))
    (unless buf (user-error "KOTOR is not running"))
    (with-current-buffer buf
      (let ((file (expand-file-name
                   (format "kotor-%s.png"
                           (format-time-string "%Y%m%d-%H%M%S"))
                   k-kotor-screenshot-dir)))
        (k-ocr--screenshot-window file)
        (message "KOTOR screenshot: %s" file)
        file))))

(defun k-kotor--ocr-game ()
  "OCR the KOTOR game window and return the text."
  (let ((buf (k-kotor--find-buffer)))
    (unless buf (user-error "KOTOR is not running"))
    (with-current-buffer buf
      (let ((file (expand-file-name "kotor-ocr.png" k-ocr-temp-dir)))
        (k-ocr--screenshot-window file)
        (k-ocr--extract-text file)))))

(defun k-kotor--ocr-game-words ()
  "OCR the KOTOR window and return positioned word plists."
  (let ((buf (k-kotor--find-buffer)))
    (unless buf (user-error "KOTOR is not running"))
    (with-current-buffer buf
      (let ((file (expand-file-name "kotor-ocr-words.png" k-ocr-temp-dir)))
        (k-ocr--screenshot-window file)
        (k-ocr--extract-words file)))))

;;; KOTOR — Dialog System

;;;###autoload
(defun k-kotor-read-dialog ()
  "Read the current KOTOR dialog text via OCR and display in a buffer.
Captures dialog text, NPC speech, and player response options.
The text is also added to the dialog history."
  (interactive)
  (let ((text (k-kotor--ocr-game)))
    (push (list :time (current-time)
                :text text)
          k-kotor--dialog-history)
    (with-current-buffer (get-buffer-create "*KOTOR Dialog*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp) (insert "\n" (make-string 50 ?─) "\n\n"))
        (insert (propertize (format-time-string "[%H:%M:%S] ")
                            'face 'font-lock-comment-face)
                text "\n"))
      (goto-char (point-max))
      (special-mode)
      (pop-to-buffer (current-buffer)))
    (message "Dialog captured (%d chars)" (length text))))

;;;###autoload
(defun k-kotor-dialog-history ()
  "View the full dialog history from this KOTOR session."
  (interactive)
  (if (null k-kotor--dialog-history)
      (message "No dialog history yet. Use k-kotor-read-dialog first.")
    (with-current-buffer (get-buffer-create "*KOTOR Dialog History*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "KOTOR Dialog History\n" 'face 'bold)
                (make-string 40 ?─) "\n\n")
        (dolist (entry (reverse k-kotor--dialog-history))
          (insert (propertize
                   (format-time-string "[%H:%M:%S] "
                                       (plist-get entry :time))
                   'face 'font-lock-comment-face)
                  (plist-get entry :text)
                  "\n\n")))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun k-kotor-clear-dialog-history ()
  "Clear the KOTOR dialog history."
  (interactive)
  (setq k-kotor--dialog-history nil)
  (message "Dialog history cleared"))

;;; KOTOR — Dialog Selection

;;;###autoload
(defun k-kotor-select-dialog (n)
  "Select dialog option N (1-9) in KOTOR.
KOTOR dialog options correspond to number keys 1-9."
  (interactive "nDialog option (1-9): ")
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key (number-to-string (min 9 (max 1 n))))
  (message "Selected dialog option %d" n))

;;;###autoload
(defun k-kotor-dialog-1 () "Select dialog option 1." (interactive) (k-kotor-select-dialog 1))
;;;###autoload
(defun k-kotor-dialog-2 () "Select dialog option 2." (interactive) (k-kotor-select-dialog 2))
;;;###autoload
(defun k-kotor-dialog-3 () "Select dialog option 3." (interactive) (k-kotor-select-dialog 3))
;;;###autoload
(defun k-kotor-dialog-4 () "Select dialog option 4." (interactive) (k-kotor-select-dialog 4))

;;; KOTOR — Game Controls

;;;###autoload
(defun k-kotor-pause ()
  "Pause/unpause KOTOR (spacebar)."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "space")
  (message "KOTOR: toggled pause"))

;;;###autoload
(defun k-kotor-quicksave ()
  "Quicksave in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  ;; KOTOR doesn't have a default quicksave key, so we open the
  ;; menu and navigate to save
  (k-script-key "Escape")
  (sit-for 0.5)
  ;; Click "Save Game" in the menu via OCR or positional click
  (k-script-click-on "Save Game")
  (sit-for 0.5)
  (message "KOTOR: save game menu opened — select a slot"))

;;;###autoload
(defun k-kotor-quickload ()
  "Open the load game menu in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "Escape")
  (sit-for 0.5)
  (k-script-click-on "Load Game")
  (sit-for 0.5)
  (message "KOTOR: load game menu opened"))

;;;###autoload
(defun k-kotor-toggle-map ()
  "Toggle the area map in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "m")
  (message "KOTOR: toggled map"))

;;;###autoload
(defun k-kotor-journal ()
  "Open the journal/quest log in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "j")
  (message "KOTOR: opened journal"))

;;;###autoload
(defun k-kotor-inventory ()
  "Open inventory in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "i")
  (message "KOTOR: opened inventory"))

;;;###autoload
(defun k-kotor-character-sheet ()
  "Open character sheet in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "c")
  (message "KOTOR: opened character sheet"))

;;;###autoload
(defun k-kotor-abilities ()
  "Open abilities/powers screen in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "p")
  (message "KOTOR: opened abilities"))

;;;###autoload
(defun k-kotor-messages ()
  "Open the messages/feedback log in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "l")
  (message "KOTOR: opened messages"))

;;; KOTOR — OCR-based Screen Reading

;;;###autoload
(defun k-kotor-read-journal ()
  "Open the journal in KOTOR and OCR its contents into a buffer.
Gives you a searchable, copyable text version of your quest log."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  ;; Open journal
  (k-script-key "j")
  (sit-for 1.0) ; wait for animation
  ;; OCR the journal screen
  (let ((text (k-kotor--ocr-game)))
    (with-current-buffer (get-buffer-create "*KOTOR Journal*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "KOTOR — Quest Journal\n" 'face 'bold)
                (propertize (format-time-string "Captured: %Y-%m-%d %H:%M:%S\n")
                            'face 'font-lock-comment-face)
                (make-string 40 ?─) "\n\n"
                text "\n"))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))
    ;; Close journal
    (k-kotor--focus)
    (k-script-key "j")
    (message "Journal captured (%d chars)" (length text))))

;;;###autoload
(defun k-kotor-read-inventory ()
  "Open inventory and OCR its contents into an Emacs buffer.
Makes inventory searchable and copyable."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "i")
  (sit-for 1.0)
  (let ((text (k-kotor--ocr-game)))
    (with-current-buffer (get-buffer-create "*KOTOR Inventory*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "KOTOR — Inventory\n" 'face 'bold)
                (propertize (format-time-string "Captured: %Y-%m-%d %H:%M:%S\n")
                            'face 'font-lock-comment-face)
                (make-string 40 ?─) "\n\n"
                text "\n"))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))
    (k-kotor--focus)
    (k-script-key "i")
    (message "Inventory captured")))

;;;###autoload
(defun k-kotor-read-character ()
  "Open character sheet and OCR stats into an Emacs buffer."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "c")
  (sit-for 1.0)
  (let ((text (k-kotor--ocr-game)))
    ;; Save to stat history
    (push (list :time (current-time)
                :text text)
          k-kotor--stat-history)
    (with-current-buffer (get-buffer-create "*KOTOR Character*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "KOTOR — Character Sheet\n" 'face 'bold)
                (propertize (format-time-string "Captured: %Y-%m-%d %H:%M:%S\n")
                            'face 'font-lock-comment-face)
                (make-string 40 ?─) "\n\n"
                text "\n"))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))
    (k-kotor--focus)
    (k-script-key "c")
    (message "Character stats captured")))

;;;###autoload
(defun k-kotor-read-screen ()
  "OCR the current KOTOR game screen without opening any menu.
Captures whatever is visible — combat feedback, area names,
NPC names, item labels, etc."
  (interactive)
  (unless (k-kotor--running-p)
    (user-error "KOTOR is not running"))
  (let ((text (k-kotor--ocr-game)))
    (kill-new text)
    (with-current-buffer (get-buffer-create "*KOTOR Screen*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "KOTOR — Screen Capture\n" 'face 'bold)
                (propertize (format-time-string "%H:%M:%S\n")
                            'face 'font-lock-comment-face)
                (make-string 40 ?─) "\n\n"
                text "\n"))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))
    (message "Screen text captured and copied to kill-ring")))

;;; KOTOR — Auto-Save

;;;###autoload
(defun k-kotor-auto-save-start ()
  "Start auto-saving KOTOR at `k-kotor-auto-save-interval' seconds.
Uses the game's Escape → Save Game flow.  Only triggers when KOTOR
is the focused window to avoid disrupting gameplay."
  (interactive)
  (when k-kotor--auto-save-timer
    (cancel-timer k-kotor--auto-save-timer))
  (setq k-kotor--auto-save-timer
        (run-at-time k-kotor-auto-save-interval
                     k-kotor-auto-save-interval
                     #'k-kotor--auto-save-tick))
  (message "KOTOR auto-save started (every %ds)" k-kotor-auto-save-interval))

(defun k-kotor--auto-save-tick ()
  "Auto-save tick — only saves if KOTOR is focused."
  (when (k-kotor--running-p)
    ;; Check if KOTOR buffer is current (user is playing)
    (let ((buf (k-kotor--find-buffer)))
      (when (and buf (eq buf (current-buffer)))
        ;; Pause first to avoid saving during combat animation
        (k-script-key "space")
        (sit-for 0.3)
        ;; Open menu
        (k-script-key "Escape")
        (sit-for 0.8)
        ;; Look for and click Save Game
        (k-script-click-on "Save")
        (sit-for 0.8)
        ;; Click the first save slot (overwrite)
        (k-script-click 1 nil nil) ; click wherever the first slot is
        (sit-for 0.5)
        ;; Confirm overwrite if prompted
        (k-script-click-on "OK")
        (sit-for 0.5)
        ;; Close menu
        (k-script-key "Escape")
        (sit-for 0.3)
        ;; Unpause
        (k-script-key "space")
        (message "KOTOR: auto-saved at %s"
                 (format-time-string "%H:%M:%S"))))))

;;;###autoload
(defun k-kotor-auto-save-stop ()
  "Stop KOTOR auto-save."
  (interactive)
  (when k-kotor--auto-save-timer
    (cancel-timer k-kotor--auto-save-timer)
    (setq k-kotor--auto-save-timer nil)
    (message "KOTOR auto-save stopped")))

;;; KOTOR — Combat Helpers

;;;###autoload
(defun k-kotor-combat-pause-read ()
  "Pause combat and OCR the combat feedback/damage numbers.
Useful for analyzing what just happened in a fight."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  ;; Pause
  (k-script-key "space")
  (sit-for 0.3)
  ;; Read the screen
  (let ((text (k-kotor--ocr-game)))
    (with-current-buffer (get-buffer-create "*KOTOR Combat*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp) (insert "\n" (make-string 30 ?·) "\n"))
        (insert (propertize (format-time-string "[%H:%M:%S] ")
                            'face 'font-lock-comment-face)
                text "\n"))
      (special-mode)
      (pop-to-buffer (current-buffer)))
    (message "Combat state captured (game paused)")))

;;;###autoload
(defun k-kotor-select-all-party ()
  "Select all party members (Backspace in KOTOR)."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key "BackSpace")
  (message "KOTOR: all party members selected"))

;;;###autoload
(defun k-kotor-solo-mode ()
  "Toggle solo mode in KOTOR."
  (interactive)
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  ;; Solo mode is not a default keybind; some mods add it
  ;; Use the party selection to approximate
  (message "KOTOR: use party selection to control members individually"))

;;; KOTOR — Quickslot Management

;;;###autoload
(defun k-kotor-use-quickslot (n)
  "Use quickslot/item N (1-9) in KOTOR.
Quickslots 1-4 are the default item slots."
  (interactive "nQuickslot (1-9): ")
  (unless (k-kotor--focus)
    (user-error "KOTOR is not running"))
  (k-script-key (number-to-string (min 9 (max 1 n))))
  (message "KOTOR: used quickslot %d" n))

;;; KOTOR — Save Game Management from Emacs

;;;###autoload
(defun k-kotor-list-saves ()
  "List KOTOR save files in a buffer.
Shows save file names, timestamps, and sizes."
  (interactive)
  (let ((save-dir k-kotor-save-dir))
    (if (file-directory-p save-dir)
        (with-current-buffer (get-buffer-create "*KOTOR Saves*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "KOTOR Save Games\n" 'face 'bold)
                    (format "Directory: %s\n" save-dir)
                    (make-string 50 ?─) "\n\n")
            (let ((dirs (directory-files save-dir t "^[^.]")))
              (if dirs
                  (dolist (dir dirs)
                    (when (file-directory-p dir)
                      (let* ((name (file-name-nondirectory dir))
                             (attrs (file-attributes dir))
                             (mtime (file-attribute-modification-time attrs))
                             (files (length (directory-files dir nil "^[^.]"))))
                        (insert (format "  %-30s  %s  (%d files)\n"
                                        (propertize name 'face 'font-lock-function-name-face)
                                        (format-time-string "%Y-%m-%d %H:%M" mtime)
                                        files)))))
                (insert "  No save games found.\n"))))
          (goto-char (point-min))
          (special-mode)
          (pop-to-buffer (current-buffer)))
      (message "Save directory not found: %s" save-dir))))

;;;###autoload
(defun k-kotor-backup-saves ()
  "Create a timestamped backup of all KOTOR save files."
  (interactive)
  (let ((save-dir k-kotor-save-dir)
        (backup-dir (expand-file-name
                     (format "kotor-saves-backup-%s"
                             (format-time-string "%Y%m%d-%H%M%S"))
                     "~/backups/")))
    (unless (file-directory-p save-dir)
      (user-error "Save directory not found: %s" save-dir))
    (make-directory (file-name-directory backup-dir) t)
    (copy-directory save-dir backup-dir nil t t)
    (message "KOTOR saves backed up to %s" backup-dir)))

;;; KOTOR — Game Config from Emacs

;;;###autoload
(defun k-kotor-edit-config ()
  "Open KOTOR's swkotor.ini for editing in Emacs."
  (interactive)
  (let ((ini (expand-file-name "swkotor.ini" k-kotor-game-dir)))
    (if (file-exists-p ini)
        (find-file ini)
      (user-error "Config not found: %s" ini))))

;;;###autoload
(defun k-kotor-toggle-subtitles ()
  "Toggle subtitles in KOTOR config (requires restart)."
  (interactive)
  (let ((ini (expand-file-name "swkotor.ini" k-kotor-game-dir)))
    (unless (file-exists-p ini)
      (user-error "Config not found: %s" ini))
    (with-temp-buffer
      (insert-file-contents ini)
      (if (re-search-forward "^Subtitles=\\([01]\\)" nil t)
          (let ((current (match-string 1)))
            (replace-match (format "Subtitles=%s"
                                   (if (string= current "1") "0" "1")))
            (write-region (point-min) (point-max) ini)
            (message "Subtitles: %s (restart KOTOR to apply)"
                     (if (string= current "1") "OFF" "ON")))
        (message "Subtitles setting not found in config")))))

;;;###autoload
(defun k-kotor-set-resolution (width height)
  "Set KOTOR resolution in config (requires restart)."
  (interactive "nWidth: \nnHeight: ")
  (let ((ini (expand-file-name "swkotor.ini" k-kotor-game-dir)))
    (unless (file-exists-p ini)
      (user-error "Config not found: %s" ini))
    (with-temp-buffer
      (insert-file-contents ini)
      (when (re-search-forward "^Width=.*$" nil t)
        (replace-match (format "Width=%d" width)))
      (goto-char (point-min))
      (when (re-search-forward "^Height=.*$" nil t)
        (replace-match (format "Height=%d" height)))
      (write-region (point-min) (point-max) ini)
      (message "Resolution set to %dx%d (restart KOTOR to apply)"
               width height))))

;;; KOTOR — Session Tracking

;;;###autoload
(defun k-kotor-session-info ()
  "Show info about the current KOTOR gaming session."
  (interactive)
  (with-current-buffer (get-buffer-create "*KOTOR Session*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "KOTOR Session Info\n" 'face 'bold)
              (make-string 40 ?─) "\n\n")
      (insert (format "  Running:     %s\n"
                      (if (k-kotor--running-p) "Yes" "No")))
      (when k-kotor--session-start
        (let* ((elapsed (float-time
                         (time-subtract (current-time)
                                        k-kotor--session-start)))
               (hours (floor (/ elapsed 3600)))
               (mins (floor (/ (mod elapsed 3600) 60))))
          (insert (format "  Session:     %dh %dm\n" hours mins)
                  (format "  Started:     %s\n"
                          (format-time-string "%H:%M:%S"
                                              k-kotor--session-start)))))
      (insert (format "  Dialogs:     %d captured\n"
                      (length k-kotor--dialog-history))
              (format "  Stat snaps:  %d\n"
                      (length k-kotor--stat-history))
              (format "  Auto-save:   %s\n"
                      (if k-kotor--auto-save-timer "active" "inactive"))
              (format "  Game dir:    %s\n" k-kotor-game-dir)
              (format "  Save dir:    %s\n" k-kotor-save-dir)))
    (goto-char (point-min))
    (special-mode)
    (pop-to-buffer (current-buffer))))

;;; KOTOR — Walkthrough/Guide Integration

;;;###autoload
(defun k-kotor-search-guide (query)
  "Search for KOTOR walkthrough/guide info in Brave browser.
Pauses the game first so you don't die while reading."
  (interactive "sSearch KOTOR guide for: ")
  ;; Pause the game first
  (when (k-kotor--running-p)
    (k-kotor--focus)
    (k-script-key "space")
    (sit-for 0.2))
  ;; Search in browser
  (k-brave-search (format "KOTOR walkthrough %s" query)))

;;;###autoload
(defun k-kotor-search-item (item)
  "Look up a KOTOR item on the wiki."
  (interactive "sItem name: ")
  (when (k-kotor--running-p)
    (k-kotor--focus)
    (k-script-key "space")
    (sit-for 0.2))
  (k-brave-open-url
   (format "https://strategywiki.org/w/index.php?search=%s&title=Special%%3ASearch&fulltext=Search&ns0=1&ns3010=1"
           (url-hexify-string (concat "KOTOR " item)))))

;;;###autoload
(defun k-kotor-ocr-and-search ()
  "OCR the current KOTOR screen and search for the most prominent text.
Great for looking up whatever quest/item/character you're seeing."
  (interactive)
  (let* ((text (k-kotor--ocr-game))
         (lines (split-string text "\n" t "[ \t]+"))
         ;; Pick the longest non-trivial line as the search query
         (best (car (sort (cl-remove-if
                           (lambda (l) (< (length l) 5))
                           lines)
                          (lambda (a b) (> (length a) (length b)))))))
    (if best
        (progn
          (message "Searching for: %s" best)
          (k-kotor-search-guide best))
      (message "No meaningful text found on screen"))))


;; ══════════════════════════════════════════════════════════════════════
;;  Hydra / Transient Menus (optional, if hydra is loaded)
;; ══════════════════════════════════════════════════════════════════════

(with-eval-after-load 'pretty-hydra
  ;; Brave Browser Hydra
  (pretty-hydra-define k-brave-hydra
    (:quit-key "q" :title "Brave Browser")
    ("Navigate"
     (("u" k-brave-open-url "open URL")
      ("s" k-brave-search "search")
      ("h" k-brave-history "history")
      ("d" k-brave-downloads "downloads")
      ("b" k-brave-bookmark "bookmark"))
     "Tabs"
     (("t" k-brave-new-tab "new tab")
      ("w" k-brave-close-tab "close tab")
      ("n" k-brave-next-tab "next tab")
      ("p" k-brave-prev-tab "prev tab")
      ("T" k-brave-reopen-tab "reopen tab"))
     "Page"
     (("g" k-brave-get-url "get URL")
      ("r" k-brave-reader-mode "reader mode")
      ("f" k-brave-find-in-page "find")
      ("o" k-brave-ocr-page "OCR page")
      ("x" k-brave-get-page-text "extract text")
      ("l" k-brave-save-page-org "org link"))))

  ;; KOTOR Hydra
  (pretty-hydra-define k-kotor-hydra
    (:quit-key "q" :title "KOTOR")
    ("Game"
     (("SPC" k-kotor-pause "pause")
      ("m" k-kotor-toggle-map "map")
      ("j" k-kotor-journal "journal")
      ("i" k-kotor-inventory "inventory")
      ("c" k-kotor-character-sheet "character")
      ("p" k-kotor-abilities "abilities"))
     "OCR/Read"
     (("d" k-kotor-read-dialog "read dialog")
      ("J" k-kotor-read-journal "OCR journal")
      ("I" k-kotor-read-inventory "OCR inventory")
      ("C" k-kotor-read-character "OCR character")
      ("r" k-kotor-read-screen "OCR screen")
      ("h" k-kotor-dialog-history "dialog history"))
     "Save/Session"
     (("s" k-kotor-quicksave "save game")
      ("l" k-kotor-quickload "load game")
      ("L" k-kotor-list-saves "list saves")
      ("B" k-kotor-backup-saves "backup saves")
      ("a" k-kotor-auto-save-start "auto-save ON")
      ("A" k-kotor-auto-save-stop "auto-save OFF"))
     "Guide"
     (("g" k-kotor-search-guide "search guide")
      ("w" k-kotor-search-item "search item")
      ("o" k-kotor-ocr-and-search "OCR → search")
      ("S" k-kotor-screenshot "screenshot")
      ("?" k-kotor-session-info "session info")))))

(provide 'k-exwm-apps)
;;; k-exwm-apps.el ends here
