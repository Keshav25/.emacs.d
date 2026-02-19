;; -*- lexical-binding: t -*-
;;; k-ocr.el --- OCR utilities for EXWM -*- lexical-binding: t -*-

;; Author: Keshav
;; URL: https://github.com/keshav25/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Provides OCR-powered features for EXWM, enabling text extraction
;; and interaction with X11 windows that would otherwise be opaque.
;;
;; This is one component of the EXWM scripting system that aims to
;; make every application as scriptable as Emacs itself.  OCR provides
;; the "eyes" — reading what's on screen when no accessibility API is
;; available.
;;
;; Requirements:
;;   - tesseract-ocr (apt install tesseract-ocr)
;;   - maim (apt install maim)
;;   - xdotool (apt install xdotool)
;;   - Optional: trans (translate-shell) for translation features

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup k-ocr nil
  "OCR utilities for EXWM."
  :group 'exwm
  :prefix "k-ocr-")

(defcustom k-ocr-tesseract-command "tesseract"
  "Path to tesseract executable."
  :type 'string
  :group 'k-ocr)

(defcustom k-ocr-language "eng"
  "Tesseract language code."
  :type 'string
  :group 'k-ocr)

(defcustom k-ocr-screenshot-command "maim"
  "Path to maim executable for screenshots."
  :type 'string
  :group 'k-ocr)

(defcustom k-ocr-temp-dir "/tmp/k-ocr"
  "Directory for temporary OCR files."
  :type 'string
  :group 'k-ocr)

(defcustom k-ocr-confidence-threshold 60
  "Minimum confidence (0-100) for OCR results.
Words below this threshold are discarded."
  :type 'integer
  :group 'k-ocr)

(defcustom k-ocr-watch-interval 2.0
  "Seconds between OCR polls in watch mode."
  :type 'number
  :group 'k-ocr)

;;;; Internal State

(defvar k-ocr--last-text nil
  "Last OCR plain-text result.")

(defvar k-ocr--last-words nil
  "Last OCR result as a list of word plists.
Each plist: (:text STR :left N :top N :width N :height N :conf N).")

(defvar k-ocr--watch-timer nil
  "Timer for watch mode.")

(defvar k-ocr--watch-callback nil
  "Callback for watch mode, called with (OLD-TEXT NEW-TEXT).")

(defvar k-ocr--watch-window-id nil
  "Window ID being watched, or nil for full screen.")

(defvar k-ocr--history nil
  "History of OCR results for `completing-read'.")

;;;; Ensure temp directory

(defun k-ocr--ensure-temp-dir ()
  "Create `k-ocr-temp-dir' if it doesn't exist."
  (unless (file-directory-p k-ocr-temp-dir)
    (make-directory k-ocr-temp-dir t)))

;;;; Core Screenshot Functions

(defun k-ocr--screenshot-region (file)
  "Take screenshot of user-selected region, save to FILE."
  (k-ocr--ensure-temp-dir)
  (call-process k-ocr-screenshot-command nil nil nil "-s" file))

(defun k-ocr--screenshot-window (file &optional window-id)
  "Take screenshot of WINDOW-ID (or current EXWM window), save to FILE.
If WINDOW-ID is nil and not in an EXWM buffer, takes full screenshot."
  (k-ocr--ensure-temp-dir)
  (let ((id (or window-id
               (and (derived-mode-p 'exwm-mode)
                    (boundp 'exwm--id)
                    exwm--id))))
    (if id
        (call-process k-ocr-screenshot-command nil nil nil
                      "-i" (format "%d" id) file)
      (call-process k-ocr-screenshot-command nil nil nil file))))

(defun k-ocr--screenshot-full (file)
  "Take full screenshot, save to FILE."
  (k-ocr--ensure-temp-dir)
  (call-process k-ocr-screenshot-command nil nil nil file))

;;;; Core OCR Functions

(defun k-ocr--run-tesseract (image-file &optional psm)
  "Run tesseract on IMAGE-FILE and return plain text.
PSM is the page segmentation mode (default: automatic)."
  (let ((args (list image-file "-" "-l" k-ocr-language)))
    (when psm
      (setq args (append args (list "--psm" (number-to-string psm)))))
    (with-temp-buffer
      (apply #'call-process k-ocr-tesseract-command nil t nil args)
      (buffer-string))))

(defun k-ocr--run-tesseract-tsv (image-file &optional psm)
  "Run tesseract on IMAGE-FILE and return TSV output with word positions.
PSM is the page segmentation mode (default 11 for sparse text)."
  (let ((args (list image-file "-"
                    "-l" k-ocr-language
                    "--psm" (number-to-string (or psm 11))
                    "-c" "tessedit_create_tsv=1")))
    (with-temp-buffer
      (apply #'call-process k-ocr-tesseract-command nil t nil args)
      (buffer-string))))

(defun k-ocr--parse-tsv (tsv-output)
  "Parse TSV-OUTPUT from tesseract into a list of word plists.
Each plist has :text :left :top :width :height :conf :level.
Filters out entries below `k-ocr-confidence-threshold'."
  (let ((lines (cdr (split-string tsv-output "\n"))) ; skip header
        (results '()))
    (dolist (line lines)
      (unless (string-empty-p (string-trim line))
        (let ((fields (split-string line "\t")))
          (when (>= (length fields) 12)
            (let ((level (string-to-number (nth 0 fields)))
                  (left (string-to-number (nth 6 fields)))
                  (top (string-to-number (nth 7 fields)))
                  (width (string-to-number (nth 8 fields)))
                  (height (string-to-number (nth 9 fields)))
                  (conf (string-to-number (nth 10 fields)))
                  (text (nth 11 fields)))
              ;; Only include word-level entries (level 5) with text
              (when (and (= level 5)
                         text
                         (not (string-empty-p (string-trim text)))
                         (>= conf k-ocr-confidence-threshold))
                (push (list :text (string-trim text)
                            :left left :top top
                            :width width :height height
                            :conf conf :level level)
                      results)))))))
    (nreverse results)))

(defun k-ocr--extract-text (image-file)
  "Extract plain text from IMAGE-FILE using tesseract."
  (string-trim (k-ocr--run-tesseract image-file)))

(defun k-ocr--extract-words (image-file &optional psm)
  "Extract positioned words from IMAGE-FILE.
Returns list of word plists with :text :left :top :width :height :conf."
  (k-ocr--parse-tsv (k-ocr--run-tesseract-tsv image-file psm)))

(defun k-ocr--find-text-in-words (target words &optional fuzzy)
  "Find TARGET string among WORDS (list of plists).
Returns list of matching word plists.  When FUZZY is non-nil, uses
substring matching instead of exact match."
  (let ((target-down (downcase target)))
    (cl-remove-if-not
     (lambda (w)
       (let ((text-down (downcase (plist-get w :text))))
         (if fuzzy
             (string-match-p (regexp-quote target-down) text-down)
           (string= target-down text-down))))
     words)))

(defun k-ocr--find-phrase-in-words (phrase words)
  "Find multi-word PHRASE among WORDS by combining adjacent word positions.
Returns the bounding box plist (:left :top :width :height) of the
phrase if found, or nil."
  (let* ((phrase-words (split-string (downcase phrase)))
         (phrase-len (length phrase-words))
         (result nil))
    (cl-loop for i from 0 to (- (length words) phrase-len)
             until result
             do (let ((match t)
                      (min-left most-positive-fixnum)
                      (min-top most-positive-fixnum)
                      (max-right 0)
                      (max-bottom 0))
                  (cl-loop for j from 0 below phrase-len
                           do (let ((w (nth (+ i j) words)))
                                (unless (string= (downcase (plist-get w :text))
                                                 (nth j phrase-words))
                                  (setq match nil))
                                (when match
                                  (setq min-left (min min-left (plist-get w :left))
                                        min-top (min min-top (plist-get w :top))
                                        max-right (max max-right
                                                       (+ (plist-get w :left)
                                                          (plist-get w :width)))
                                        max-bottom (max max-bottom
                                                        (+ (plist-get w :top)
                                                           (plist-get w :height)))))))
                  (when match
                    (setq result (list :left min-left :top min-top
                                       :width (- max-right min-left)
                                       :height (- max-bottom min-top))))))
    result))

(defun k-ocr--word-center (word-plist)
  "Return (X . Y) center coordinates of WORD-PLIST."
  (cons (+ (plist-get word-plist :left)
           (/ (plist-get word-plist :width) 2))
        (+ (plist-get word-plist :top)
           (/ (plist-get word-plist :height) 2))))

;;;; Interactive Commands — Text Extraction

;;;###autoload
(defun k-ocr-region ()
  "OCR a user-selected screen region.  Result goes to kill-ring."
  (interactive)
  (let ((file (expand-file-name "ocr-region.png" k-ocr-temp-dir)))
    (message "Select region to OCR...")
    (k-ocr--screenshot-region file)
    (let ((text (k-ocr--extract-text file)))
      (setq k-ocr--last-text text)
      (kill-new text)
      (message "Copied: %s" (truncate-string-to-width text 80)))))

;;;###autoload
(defun k-ocr-window ()
  "OCR the current EXWM window.  Result goes to kill-ring."
  (interactive)
  (let ((file (expand-file-name "ocr-window.png" k-ocr-temp-dir)))
    (k-ocr--screenshot-window file)
    (let ((text (k-ocr--extract-text file)))
      (setq k-ocr--last-text text)
      (kill-new text)
      (message "Copied %d characters from window" (length text)))))

;;;###autoload
(defun k-ocr-screen ()
  "OCR the entire screen.  Result goes to kill-ring."
  (interactive)
  (let ((file (expand-file-name "ocr-screen.png" k-ocr-temp-dir)))
    (k-ocr--screenshot-full file)
    (let ((text (k-ocr--extract-text file)))
      (setq k-ocr--last-text text)
      (kill-new text)
      (message "Copied %d characters from screen" (length text)))))

;;;###autoload
(defun k-ocr-to-buffer ()
  "OCR a user-selected region and display in a buffer."
  (interactive)
  (let ((file (expand-file-name "ocr-buffer.png" k-ocr-temp-dir)))
    (message "Select region to OCR...")
    (k-ocr--screenshot-region file)
    (let ((text (k-ocr--extract-text file)))
      (setq k-ocr--last-text text)
      (with-current-buffer (get-buffer-create "*OCR Result*")
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun k-ocr-window-to-buffer ()
  "OCR the current EXWM window and display in a read-only buffer.
This effectively gives you a text representation of the application
that you can search, copy from, etc."
  (interactive)
  (let ((file (expand-file-name "ocr-winbuf.png" k-ocr-temp-dir)))
    (k-ocr--screenshot-window file)
    (let ((text (k-ocr--extract-text file))
          (win-name (or (and (boundp 'exwm-title) exwm-title)
                        (buffer-name))))
      (setq k-ocr--last-text text)
      (with-current-buffer (get-buffer-create
                            (format "*OCR: %s*" win-name))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text))
        (goto-char (point-min))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun k-ocr-last ()
  "Display last OCR result."
  (interactive)
  (if k-ocr--last-text
      (with-current-buffer (get-buffer-create "*OCR Result*")
        (erase-buffer)
        (insert k-ocr--last-text)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer)))
    (message "No previous OCR result")))

;;;; Interactive Commands — Click on Text (improved)

(defun k-ocr--click-at (x y &optional button)
  "Move mouse to (X, Y) and click BUTTON (default 1=left)."
  (call-process "xdotool" nil nil nil
                "mousemove" (number-to-string x)
                (number-to-string y))
  (sit-for 0.05)
  (call-process "xdotool" nil nil nil
                "click" (number-to-string (or button 1))))

(defun k-ocr--move-to (x y)
  "Move mouse to (X, Y) without clicking."
  (call-process "xdotool" nil nil nil
                "mousemove" (number-to-string x)
                (number-to-string y)))

;;;###autoload
(defun k-ocr-click-text (target)
  "Find TARGET text on screen via OCR and click on it.
If multiple matches are found, presents a choice.
Supports multi-word phrases."
  (interactive
   (list (read-string "Click on text: " nil 'k-ocr--history)))
  (let ((file (expand-file-name "ocr-click.png" k-ocr-temp-dir)))
    (message "Taking screenshot...")
    (k-ocr--screenshot-full file)
    (message "Running OCR...")
    (let* ((words (k-ocr--extract-words file))
           ;; Try phrase match first, then single-word
           (phrase-box (k-ocr--find-phrase-in-words target words))
           (matches (unless phrase-box
                      (k-ocr--find-text-in-words target words t))))
      (cond
       (phrase-box
        (let* ((center (k-ocr--word-center phrase-box)))
          (k-ocr--click-at (car center) (cdr center))
          (message "Clicked on '%s' at (%d, %d)" target (car center) (cdr center))))
       ((= (length matches) 1)
        (let ((center (k-ocr--word-center (car matches))))
          (k-ocr--click-at (car center) (cdr center))
          (message "Clicked on '%s' at (%d, %d)"
                   (plist-get (car matches) :text)
                   (car center) (cdr center))))
       ((> (length matches) 1)
        (let* ((candidates
                (mapcar (lambda (m)
                          (cons (format "%s [at %d,%d conf:%d%%]"
                                        (plist-get m :text)
                                        (plist-get m :left)
                                        (plist-get m :top)
                                        (plist-get m :conf))
                                m))
                        matches))
               (choice (completing-read
                        (format "Multiple matches for '%s': " target)
                        candidates nil t))
               (selected (cdr (assoc choice candidates)))
               (center (k-ocr--word-center selected)))
          (k-ocr--click-at (car center) (cdr center))
          (message "Clicked on '%s' at (%d, %d)"
                   (plist-get selected :text) (car center) (cdr center))))
       (t (message "Text '%s' not found on screen" target))))))

;;;###autoload
(defun k-ocr-right-click-text (target)
  "Find TARGET text on screen via OCR and right-click on it."
  (interactive
   (list (read-string "Right-click on text: " nil 'k-ocr--history)))
  (let ((file (expand-file-name "ocr-rclick.png" k-ocr-temp-dir)))
    (message "Taking screenshot...")
    (k-ocr--screenshot-full file)
    (message "Running OCR...")
    (let* ((words (k-ocr--extract-words file))
           (phrase-box (k-ocr--find-phrase-in-words target words))
           (matches (unless phrase-box
                      (k-ocr--find-text-in-words target words t))))
      (cond
       (phrase-box
        (let ((center (k-ocr--word-center phrase-box)))
          (k-ocr--click-at (car center) (cdr center) 3)
          (message "Right-clicked on '%s'" target)))
       ((>= (length matches) 1)
        (let ((center (k-ocr--word-center (car matches))))
          (k-ocr--click-at (car center) (cdr center) 3)
          (message "Right-clicked on '%s'" (plist-get (car matches) :text))))
       (t (message "Text '%s' not found on screen" target))))))

;;;###autoload
(defun k-ocr-double-click-text (target)
  "Find TARGET text on screen via OCR and double-click on it."
  (interactive
   (list (read-string "Double-click on text: " nil 'k-ocr--history)))
  (let ((file (expand-file-name "ocr-dclick.png" k-ocr-temp-dir)))
    (k-ocr--screenshot-full file)
    (let* ((words (k-ocr--extract-words file))
           (phrase-box (k-ocr--find-phrase-in-words target words))
           (matches (unless phrase-box
                      (k-ocr--find-text-in-words target words t))))
      (when-let ((box (or phrase-box (car matches))))
        (let ((center (k-ocr--word-center box)))
          (k-ocr--move-to (car center) (cdr center))
          (sit-for 0.05)
          (call-process "xdotool" nil nil nil
                        "click" "--repeat" "2" "--delay" "50" "1")
          (message "Double-clicked on '%s'" target))))))

;;;###autoload
(defun k-ocr-move-to-text (target)
  "Find TARGET text on screen and move mouse to it (without clicking)."
  (interactive
   (list (read-string "Move to text: " nil 'k-ocr--history)))
  (let ((file (expand-file-name "ocr-move.png" k-ocr-temp-dir)))
    (k-ocr--screenshot-full file)
    (let* ((words (k-ocr--extract-words file))
           (phrase-box (k-ocr--find-phrase-in-words target words))
           (matches (unless phrase-box
                      (k-ocr--find-text-in-words target words t))))
      (when-let ((box (or phrase-box (car matches))))
        (let ((center (k-ocr--word-center box)))
          (k-ocr--move-to (car center) (cdr center))
          (message "Moved to '%s' at (%d, %d)" target (car center) (cdr center)))))))

;;;; Interactive Commands — Watch Mode

;;;###autoload
(defun k-ocr-watch-start (&optional callback)
  "Start watching the current EXWM window for text changes.
Polls every `k-ocr-watch-interval' seconds.  When text changes,
CALLBACK is called with (old-text new-text).  If CALLBACK is nil,
changes are displayed as messages.

This enables reactive scripting — you can watch for specific text
to appear in an application and take action."
  (interactive)
  (when k-ocr--watch-timer
    (k-ocr-watch-stop))
  (let ((win-id (and (derived-mode-p 'exwm-mode)
                     (boundp 'exwm--id)
                     exwm--id)))
    (setq k-ocr--watch-window-id win-id
          k-ocr--watch-callback (or callback
                                    (lambda (old new)
                                      (message "OCR watch: text changed (%d → %d chars)"
                                               (length (or old ""))
                                               (length new))))
          k-ocr--watch-timer
          (run-at-time 0 k-ocr-watch-interval
                       #'k-ocr--watch-tick))
    (message "OCR watch started (interval: %.1fs)" k-ocr-watch-interval)))

(defun k-ocr--watch-tick ()
  "Internal tick function for watch mode."
  (let ((file (expand-file-name "ocr-watch.png" k-ocr-temp-dir)))
    (condition-case err
        (progn
          (if k-ocr--watch-window-id
              (k-ocr--screenshot-window file k-ocr--watch-window-id)
            (k-ocr--screenshot-full file))
          (let ((new-text (k-ocr--extract-text file)))
            (unless (equal new-text k-ocr--last-text)
              (let ((old-text k-ocr--last-text))
                (setq k-ocr--last-text new-text)
                (when k-ocr--watch-callback
                  (funcall k-ocr--watch-callback old-text new-text))))))
      (error (message "OCR watch error: %s" (error-message-string err))))))

;;;###autoload
(defun k-ocr-watch-stop ()
  "Stop watching for text changes."
  (interactive)
  (when k-ocr--watch-timer
    (cancel-timer k-ocr--watch-timer)
    (setq k-ocr--watch-timer nil
          k-ocr--watch-callback nil
          k-ocr--watch-window-id nil)
    (message "OCR watch stopped")))

;;;###autoload
(defun k-ocr-wait-for-text (target &optional timeout callback)
  "Wait for TARGET text to appear on screen.
TIMEOUT is max seconds to wait (default 30).  When found, call
CALLBACK with the full OCR text, or just message if CALLBACK is nil.
Useful for scripting: wait for a dialog to appear, then act on it."
  (interactive "sWait for text: ")
  (let ((timeout (or timeout 30))
        (start-time (float-time))
        (file (expand-file-name "ocr-wait.png" k-ocr-temp-dir))
        (win-id (and (derived-mode-p 'exwm-mode)
                     (boundp 'exwm--id)
                     exwm--id)))
    (message "Waiting for '%s' (timeout: %ds)..." target timeout)
    (run-at-time
     0 1.0
     (let ((timer-sym (gensym "ocr-wait-")))
       (set timer-sym nil)
       (set timer-sym
            (lambda ()
              (if (> (- (float-time) start-time) timeout)
                  (progn
                    (cancel-timer (symbol-value timer-sym))
                    (message "Timeout waiting for '%s'" target))
                (if win-id
                    (k-ocr--screenshot-window file win-id)
                  (k-ocr--screenshot-full file))
                (let ((text (k-ocr--extract-text file)))
                  (when (string-match-p (regexp-quote target) text)
                    (cancel-timer (symbol-value timer-sym))
                    (if callback
                        (funcall callback text)
                      (message "Found '%s'!" target)))))))))))

;;;; Interactive Commands — Org Integration

;;;###autoload
(defun k-ocr-to-org-capture ()
  "OCR a region and send to org-capture."
  (interactive)
  (let ((file (expand-file-name "ocr-capture.png" k-ocr-temp-dir)))
    (message "Select region to capture...")
    (k-ocr--screenshot-region file)
    (let ((text (k-ocr--extract-text file)))
      (setq k-ocr--last-text text)
      (org-capture-string text))))

;;;###autoload
(defun k-ocr-table-to-org ()
  "OCR a table region and convert to org-table format."
  (interactive)
  (let ((file (expand-file-name "ocr-table.png" k-ocr-temp-dir)))
    (message "Select table region...")
    (k-ocr--screenshot-region file)
    (let* ((text (k-ocr--run-tesseract file 6)) ; PSM 6 = uniform block
           (lines (split-string (string-trim text) "\n")))
      (with-current-buffer (get-buffer-create "*OCR Table*")
        (erase-buffer)
        (org-mode)
        (dolist (line lines)
          (insert "| "
                  (replace-regexp-in-string "  +" " | " (string-trim line))
                  " |\n"))
        (goto-char (point-min))
        (org-table-align)
        (pop-to-buffer (current-buffer))))))

;;;; Interactive Commands — URL Extraction

;;;###autoload
(defun k-ocr-extract-urls ()
  "OCR a region and extract URLs."
  (interactive)
  (let ((file (expand-file-name "ocr-urls.png" k-ocr-temp-dir)))
    (message "Select region...")
    (k-ocr--screenshot-region file)
    (let* ((text (k-ocr--extract-text file))
           (urls '()))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward "https?://[^\s\n\t\"'<>]+" nil t)
          (push (match-string 0) urls)))
      (if urls
          (let ((url (completing-read "Open URL: " (nreverse urls) nil t)))
            (browse-url url))
        (message "No URLs found")))))

;;;; Interactive Commands — Translation

;;;###autoload
(defun k-ocr-translate (target-lang)
  "OCR a region and translate to TARGET-LANG using translate-shell."
  (interactive "sTranslate to language (e.g., en, es, fr): ")
  (unless (executable-find "trans")
    (user-error "translate-shell not found. Install with: apt install translate-shell"))
  (let ((file (expand-file-name "ocr-translate.png" k-ocr-temp-dir)))
    (message "Select region to translate...")
    (k-ocr--screenshot-region file)
    (let* ((text (k-ocr--extract-text file))
           (translated (shell-command-to-string
                        (format "trans -b :%s %s"
                                (shell-quote-argument target-lang)
                                (shell-quote-argument text)))))
      (with-current-buffer (get-buffer-create "*OCR Translation*")
        (erase-buffer)
        (insert "Original:\n" text "\n\n")
        (insert "Translation:\n" translated)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;;;; Interactive Commands — EXWM Window Integration

;;;###autoload
(defun k-ocr-window-search-and-focus (text)
  "Search for TEXT across all EXWM windows, focus the one containing it.
This lets you find and switch to a window by its visible content."
  (interactive "sSearch for: ")
  (let ((file (expand-file-name "ocr-search-win.png" k-ocr-temp-dir))
        (found nil)
        (results '()))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf (derived-mode-p 'exwm-mode)))
        (with-current-buffer buf
          (when (boundp 'exwm--id)
            (k-ocr--screenshot-window file exwm--id)
            (let ((ocr-text (k-ocr--extract-text file)))
              (when (string-match-p (regexp-quote text) ocr-text)
                (push (cons (buffer-name) buf) results)))))))
    (cond
     ((null results)
      (message "Text '%s' not found in any EXWM window" text))
     ((= (length results) 1)
      (pop-to-buffer (cdar results))
      (message "Found '%s' in %s" text (caar results)))
     (t
      (let* ((choice (completing-read
                      (format "'%s' found in multiple windows: " text)
                      (mapcar #'car results) nil t))
             (buf (cdr (assoc choice results))))
        (pop-to-buffer buf)
        (message "Switched to %s" choice))))))

;;;###autoload
(defun k-ocr-consult-window-text ()
  "Search all EXWM window text with live filtering via completing-read.
OCRs all EXWM windows once, then lets you search/filter through the
combined text with window attribution."
  (interactive)
  (let ((file (expand-file-name "ocr-consult.png" k-ocr-temp-dir))
        (entries '()))
    (message "OCR-ing all EXWM windows...")
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf (derived-mode-p 'exwm-mode)))
        (with-current-buffer buf
          (when (boundp 'exwm--id)
            (k-ocr--screenshot-window file exwm--id)
            (let* ((ocr-text (k-ocr--extract-text file))
                   (lines (split-string ocr-text "\n" t "[ \t]+")))
              (dolist (line lines)
                (unless (string-empty-p line)
                  (push (cons (format "[%s] %s"
                                      (truncate-string-to-width
                                       (buffer-name) 20)
                                      line)
                              buf)
                        entries))))))))
    (if entries
        (let* ((choice (completing-read "Window text: "
                                        (mapcar #'car (nreverse entries))
                                        nil t))
               (buf (cdr (assoc choice entries))))
          (when buf (pop-to-buffer buf)))
      (message "No text found in EXWM windows"))))

;;;; Utility Functions

;;;###autoload
(defun k-ocr-check-dependencies ()
  "Check if all OCR dependencies are installed."
  (interactive)
  (let ((deps '(("tesseract" . "tesseract-ocr")
                ("maim" . "maim")
                ("xdotool" . "xdotool")
                ("xprop" . "x11-utils")
                ("xdg-open" . "xdg-utils")))
        (optional '(("trans" . "translate-shell")))
        (missing '())
        (opt-missing '()))
    (dolist (dep deps)
      (unless (executable-find (car dep))
        (push (cdr dep) missing)))
    (dolist (dep optional)
      (unless (executable-find (car dep))
        (push (cdr dep) opt-missing)))
    (with-current-buffer (get-buffer-create "*OCR Dependencies*")
      (erase-buffer)
      (insert "OCR/Scripting Dependency Check\n"
              (make-string 35 ?─) "\n\n")
      (if missing
          (insert (format "❌ Missing (required): %s\n   Install: sudo apt install %s\n\n"
                          (string-join missing ", ")
                          (string-join missing " ")))
        (insert "✓ All required dependencies installed\n\n"))
      (if opt-missing
          (insert (format "⚠ Missing (optional): %s\n   Install: sudo apt install %s\n"
                          (string-join opt-missing ", ")
                          (string-join opt-missing " ")))
        (insert "✓ All optional dependencies installed\n"))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'k-ocr)
;;; k-ocr.el ends here
