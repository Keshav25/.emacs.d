;; -*- lexical-binding: t -*-
;;; k-ocr.el --- OCR utilities for EXWM -*- lexical-binding: t -*-

;;; Commentary:
;; Provides OCR-powered features for EXWM, enabling text extraction
;; and interaction with X11 windows that would otherwise be opaque.
;;
;; Requirements:
;;   - tesseract-ocr (apt install tesseract-ocr)
;;   - maim (apt install maim)
;;   - xdotool (apt install xdotool)
;;   - Optional: trans (translate-shell) for translation features

;;; Code:

(defgroup k-ocr nil
  "OCR utilities for EXWM."
  :group 'exwm)

(defcustom k-ocr-tesseract-command "tesseract"
  "Path to tesseract executable."
  :type 'string
  :group 'k-ocr)

(defcustom k-ocr-language "eng"
  "Tesseract language code."
  :type 'string
  :group 'k-ocr)

(defcustom k-ocr-screenshot-command "maim"
  "Path to maim executable."
  :type 'string
  :group 'k-ocr)

(defcustom k-ocr-temp-dir "/tmp"
  "Directory for temporary OCR files."
  :type 'string
  :group 'k-ocr)

(defvar k-ocr--last-text nil
  "Last OCR result.")

(defvar k-ocr--watch-timer nil
  "Timer for watch region feature.")

;;; Core Functions

(defun k-ocr--screenshot-region (file)
  "Take screenshot of user-selected region, save to FILE."
  (call-process k-ocr-screenshot-command nil nil nil "-s" file))

(defun k-ocr--screenshot-window (file &optional window-id)
  "Take screenshot of window WINDOW-ID (or current EXWM window), save to FILE."
  (let ((id (or window-id (and (boundp 'exwm--id) exwm--id))))
    (if id
        (call-process k-ocr-screenshot-command nil nil nil
                      "-i" (format "%d" id) file)
      (call-process k-ocr-screenshot-command nil nil nil file))))

(defun k-ocr--screenshot-full (file)
  "Take full screenshot, save to FILE."
  (call-process k-ocr-screenshot-command nil nil nil file))

(defun k-ocr--run-tesseract (image-file &optional tsv)
  "Run tesseract on IMAGE-FILE. If TSV, return TSV format for positioning."
  (let ((args (list image-file "-" "-l" k-ocr-language)))
    (when tsv
      (setq args (append args '("-c" "tessedit_create_tsv=1"))))
    (with-temp-buffer
      (apply #'call-process k-ocr-tesseract-command nil t nil args)
      (buffer-string))))

(defun k-ocr--extract-text (image-file)
  "Extract text from IMAGE-FILE using tesseract."
  (string-trim (k-ocr--run-tesseract image-file)))

;;; Interactive Commands - Text Extraction

;;;###autoload
(defun k-ocr-region ()
  "OCR a user-selected screen region. Result goes to kill-ring."
  (interactive)
  (let ((file (expand-file-name "ocr-region.png" k-ocr-temp-dir)))
    (message "Select region to OCR...")
    (k-ocr--screenshot-region file)
    (let ((text (k-ocr--extract-text file)))
      (setq k-ocr--last-text text)
      (kill-new text)
      (message "Copied: %s" (truncate-string-to-width text 60)))))

;;;###autoload
(defun k-ocr-window ()
  "OCR the current EXWM window. Result goes to kill-ring."
  (interactive)
  (let ((file (expand-file-name "ocr-window.png" k-ocr-temp-dir)))
    (k-ocr--screenshot-window file)
    (let ((text (k-ocr--extract-text file)))
      (setq k-ocr--last-text text)
      (kill-new text)
      (message "Copied %d characters" (length text)))))

;;;###autoload
(defun k-ocr-screen ()
  "OCR the entire screen. Result goes to kill-ring."
  (interactive)
  (let ((file (expand-file-name "ocr-screen.png" k-ocr-temp-dir)))
    (k-ocr--screenshot-full file)
    (let ((text (k-ocr--extract-text file)))
      (setq k-ocr--last-text text)
      (kill-new text)
      (message "Copied %d characters" (length text)))))

;;;###autoload
(defun k-ocr-to-buffer ()
  "OCR a region and display result in a buffer."
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

;;; Interactive Commands - Click on Text

;;;###autoload
(defun k-ocr-click-text (target)
  "Find TARGET text on screen and click on it."
  (interactive "sClick on text: ")
  (let ((file (expand-file-name "ocr-click.png" k-ocr-temp-dir)))
    (message "Taking screenshot...")
    (k-ocr--screenshot-full file)
    (message "Running OCR...")
    (let* ((output (shell-command-to-string
                    (format "tesseract %s - -l %s --psm 11 -c tessedit_create_tsv=1 2>/dev/null"
                            (shell-quote-argument file) k-ocr-language)))
           (lines (cdr (split-string output "\n")))
           (found nil))
      (dolist (line lines)
        (when (and (not found) 
                   (not (string-empty-p line))
                   (string-match-p (regexp-quote target) line))
          (let* ((fields (split-string line "\t")))
            (when (>= (length fields) 12)
              (let ((text (nth 11 fields))
                    (left (string-to-number (or (nth 6 fields) "0")))
                    (top (string-to-number (or (nth 7 fields) "0")))
                    (width (string-to-number (or (nth 8 fields) "0")))
                    (height (string-to-number (or (nth 9 fields) "0"))))
                (when (and text 
                           (not (string-empty-p text))
                           (string-match-p (regexp-quote target) text)
                           (> left 0) (> top 0))
                  (let ((x (+ left (/ width 2)))
                        (y (+ top (/ height 2))))
                    (setq found t)
                    (call-process "xdotool" nil nil nil
                                  "mousemove" (number-to-string x) 
                                  (number-to-string y))
                    (sit-for 0.1)
                    (call-process "xdotool" nil nil nil "click" "1")
                    (message "Clicked on '%s' at (%d, %d)" text x y))))))))
      (unless found
        (message "Text '%s' not found on screen" target)))))

;;;###autoload
(defun k-ocr-move-to-text (target)
  "Find TARGET text on screen and move mouse to it (without clicking)."
  (interactive "sMove to text: ")
  (let ((file (expand-file-name "ocr-move.png" k-ocr-temp-dir)))
    (message "Taking screenshot...")
    (k-ocr--screenshot-full file)
    (message "Running OCR...")
    (let* ((output (shell-command-to-string
                    (format "tesseract %s - -l %s --psm 11 -c tessedit_create_tsv=1 2>/dev/null"
                            (shell-quote-argument file) k-ocr-language)))
           (lines (cdr (split-string output "\n")))
           (found nil))
      (dolist (line lines)
        (when (and (not found) 
                   (not (string-empty-p line))
                   (string-match-p (regexp-quote target) line))
          (let* ((fields (split-string line "\t")))
            (when (>= (length fields) 12)
              (let ((text (nth 11 fields))
                    (left (string-to-number (or (nth 6 fields) "0")))
                    (top (string-to-number (or (nth 7 fields) "0")))
                    (width (string-to-number (or (nth 8 fields) "0")))
                    (height (string-to-number (or (nth 9 fields) "0"))))
                (when (and text 
                           (not (string-empty-p text))
                           (string-match-p (regexp-quote target) text)
                           (> left 0) (> top 0))
                  (let ((x (+ left (/ width 2)))
                        (y (+ top (/ height 2))))
                    (setq found t)
                    (call-process "xdotool" nil nil nil
                                  "mousemove" (number-to-string x) 
                                  (number-to-string y))
                    (message "Moved to '%s' at (%d, %d)" text x y))))))))
      (unless found
        (message "Text '%s' not found on screen" target)))))

;;; Interactive Commands - Org Integration

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
    (let* ((text (with-temp-buffer
                   (call-process k-ocr-tesseract-command nil t nil
                                 file "-" "-l" k-ocr-language "--psm" "6")
                   (buffer-string)))
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

;;; Interactive Commands - URL Extraction

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

;;; Interactive Commands - Translation

;;;###autoload
(defun k-ocr-translate (target-lang)
  "OCR a region and translate to TARGET-LANG using translate-shell."
  (interactive "sTranslate to language (e.g., en, es, fr): ")
  (let ((file (expand-file-name "ocr-translate.png" k-ocr-temp-dir)))
    (message "Select region to translate...")
    (k-ocr--screenshot-region file)
    (let* ((text (k-ocr--extract-text file))
           (translated (shell-command-to-string
                        (format "trans -b :%s '%s'" 
                                target-lang
                                (shell-quote-argument text)))))
      (with-current-buffer (get-buffer-create "*OCR Translation*")
        (erase-buffer)
        (insert "Original:\n" text "\n\n")
        (insert "Translation:\n" translated)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;;; Interactive Commands - EXWM Integration

;;;###autoload
(defun k-ocr-window-search-and-focus (text)
  "Search for TEXT across all EXWM windows, focus the one containing it."
  (interactive "sSearch for: ")
  (let ((file (expand-file-name "ocr-search-win.png" k-ocr-temp-dir))
        (found nil))
    (dolist (buf (buffer-list))
      (when (and (not found)
                 (buffer-live-p buf)
                 (with-current-buffer buf (derived-mode-p 'exwm-mode)))
        (with-current-buffer buf
          (when (boundp 'exwm--id)
            (k-ocr--screenshot-window file exwm--id)
            (let ((ocr-text (k-ocr--extract-text file)))
              (when (string-match-p (regexp-quote text) ocr-text)
                (setq found buf)))))))
    (if found
        (progn
          (pop-to-buffer found)
          (message "Found '%s' in %s" text (buffer-name found)))
      (message "Text '%s' not found in any window" text))))

;;; Utility Functions

;;;###autoload
(defun k-ocr-check-dependencies ()
  "Check if OCR dependencies are installed."
  (interactive)
  (let ((deps '(("tesseract" . "tesseract-ocr")
                ("maim" . "maim")
                ("xdotool" . "xdotool")))
        (missing '()))
    (dolist (dep deps)
      (unless (executable-find (car dep))
        (push (cdr dep) missing)))
    (if missing
        (message "Missing: %s. Install with: sudo apt install %s"
                 (string-join missing ", ")
                 (string-join missing " "))
      (message "All OCR dependencies installed!"))))

(provide 'k-ocr)
;;; k-ocr.el ends here
