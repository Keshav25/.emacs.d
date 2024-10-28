(leaf keycast
  :ensure t
  :require t
  :config
  
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'my-modeline-major-mode)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
	(add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
	(add-to-list 'keycast-substitute-alist `(,event nil)))

  (keycast-mode-line-mode))



(leaf spacious-padding
  :ensure t)

(leaf modeline
  :after (spacious-padding keycast)
  :config
  (setq mode-line-compact nil)
  (setq mode-line-right-align-edge 'right-margin) 

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
  (defface my-modeline-indicator-button nil
	"Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")
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
				 (eq window (minibuffer-selected-window))))))))

;; ;;;;; Copy Pasted
;; (defgroup my-modeline nil
;;   "Custom modeline that is stylistically close to the default."
;;   :group 'mode-line)

;; (defgroup my-modeline-faces nil
;;   "Faces for my custom modeline."
;;   :group 'my-modeline)

;; (defcustom my-modeline-string-truncate-length 9
;;   "String length after which truncation should be done in small windows."
;;   :type 'natnum)

;; ;;;; Faces

;; (defface my-modeline-indicator-button nil
;;   "Generic face used for indicators that have a background.
;; Modify this face to, for example, add a :box attribute to all
;; relevant indicators (combines nicely with my `spacious-padding'
;; package).")

;; (defface my-modeline-indicator-red
;;   '((default :inherit bold)
;;     (((class color) (min-colors 88) (background light))
;;      :foreground "#880000")
;;     (((class color) (min-colors 88) (background dark))
;;      :foreground "#ff9f9f")
;;     (t :foreground "red"))
;;   "Face for modeline indicators (e.g. see my `notmuch-indicator')."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-red-bg
;;   '((default :inherit (bold my-modeline-indicator-button))
;;     (((class color) (min-colors 88) (background light))
;;      :background "#aa1111" :foreground "white")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "#ff9090" :foreground "black")
;;     (t :background "red" :foreground "black"))
;;   "Face for modeline indicators with a background."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-green
;;   '((default :inherit bold)
;;     (((class color) (min-colors 88) (background light))
;;      :foreground "#005f00")
;;     (((class color) (min-colors 88) (background dark))
;;      :foreground "#73fa7f")
;;     (t :foreground "green"))
;;   "Face for modeline indicators (e.g. see my `notmuch-indicator')."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-green-bg
;;   '((default :inherit (bold my-modeline-indicator-button))
;;     (((class color) (min-colors 88) (background light))
;;      :background "#207b20" :foreground "white")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "#77d077" :foreground "black")
;;     (t :background "green" :foreground "black"))
;;   "Face for modeline indicators with a background."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-yellow
;;   '((default :inherit bold)
;;     (((class color) (min-colors 88) (background light))
;;      :foreground "#6f4000")
;;     (((class color) (min-colors 88) (background dark))
;;      :foreground "#f0c526")
;;     (t :foreground "yellow"))
;;   "Face for modeline indicators (e.g. see my `notmuch-indicator')."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-yellow-bg
;;   '((default :inherit (bold my-modeline-indicator-button))
;;     (((class color) (min-colors 88) (background light))
;;      :background "#805000" :foreground "white")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "#ffc800" :foreground "black")
;;     (t :background "yellow" :foreground "black"))
;;   "Face for modeline indicators with a background."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-blue
;;   '((default :inherit bold)
;;     (((class color) (min-colors 88) (background light))
;;      :foreground "#00228a")
;;     (((class color) (min-colors 88) (background dark))
;;      :foreground "#88bfff")
;;     (t :foreground "blue"))
;;   "Face for modeline indicators (e.g. see my `notmuch-indicator')."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-blue-bg
;;   '((default :inherit (bold my-modeline-indicator-button))
;;     (((class color) (min-colors 88) (background light))
;;      :background "#0000aa" :foreground "white")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "#77aaff" :foreground "black")
;;     (t :background "blue" :foreground "black"))
;;   "Face for modeline indicators with a background."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-magenta
;;   '((default :inherit bold)
;;     (((class color) (min-colors 88) (background light))
;;      :foreground "#6a1aaf")
;;     (((class color) (min-colors 88) (background dark))
;;      :foreground "#e0a0ff")
;;     (t :foreground "magenta"))
;;   "Face for modeline indicators (e.g. see my `notmuch-indicator')."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-magenta-bg
;;   '((default :inherit (bold my-modeline-indicator-button))
;;     (((class color) (min-colors 88) (background light))
;;      :background "#6f0f9f" :foreground "white")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "#e3a2ff" :foreground "black")
;;     (t :background "magenta" :foreground "black"))
;;   "Face for modeline indicators with a background."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-cyan
;;   '((default :inherit bold)
;;     (((class color) (min-colors 88) (background light))
;;      :foreground "#004060")
;;     (((class color) (min-colors 88) (background dark))
;;      :foreground "#30b7cc")
;;     (t :foreground "cyan"))
;;   "Face for modeline indicators (e.g. see my `notmuch-indicator')."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-cyan-bg
;;   '((default :inherit (bold my-modeline-indicator-button))
;;     (((class color) (min-colors 88) (background light))
;;      :background "#006080" :foreground "white")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "#40c0e0" :foreground "black")
;;     (t :background "cyan" :foreground "black"))
;;   "Face for modeline indicators with a background."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-gray
;;   '((t :inherit shadow))
;;   "Face for modeline indicators (e.g. see my `notmuch-indicator')."
;;   :group 'my-modeline-faces)

;; (defface my-modeline-indicator-gray-bg
;;   '((default :inherit (bold my-modeline-indicator-button))
;;     (((class color) (min-colors 88) (background light))
;;      :background "#808080" :foreground "white")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "#a0a0a0" :foreground "black")
;;     (t :inverse-video t))
;;   "Face for modeline indicatovrs with a background."
;;   :group 'my-modeline-faces)

;; ;;;; Common helper functions

;; (defun my-modeline--string-truncate-p (str)
;;   "Return non-nil if STR should be truncated."
;;   (if (string-empty-p str)
;;       str
;;     (and (my-common-window-narrow-p)
;;          (> (length str) my-modeline-string-truncate-length)
;;          (not (one-window-p :no-minibuffer)))))

;; (defun my-modeline--truncate-p ()
;;   "Return non-nil if truncation should happen.
;; This is a more general and less stringent variant of
;; `my-modeline--string-truncate-p'."
;;   (and (my-common-window-narrow-p)
;;        (not (one-window-p :no-minibuffer))))

;; (defun my-modeline-string-cut-end (str)
;;   "Return truncated STR, if appropriate, else return STR.
;; Cut off the end of STR by counting from its start up to
;; `my-modeline-string-truncate-length'."
;;   (if (my-modeline--string-truncate-p str)
;;       (concat (substring str 0 my-modeline-string-truncate-length) "...")
;;     str))

;; (defun my-modeline-string-cut-beginning (str)
;;   "Return truncated STR, if appropriate, else return STR.
;; Cut off the beginning of STR by counting from its end up to
;; `my-modeline-string-truncate-length'."
;;   (if (my-modeline--string-truncate-p str)
;;       (concat "..." (substring str (- my-modeline-string-truncate-length)))
;;     str))

;; (defun my-modeline-string-cut-middle (str)
;;   "Return truncated STR, if appropriate, else return STR.
;; Cut off the middle of STR by counting half of
;; `my-modeline-string-truncate-length' both from its beginning
;; and end."
;;   (let ((half (floor my-modeline-string-truncate-length 2)))
;;     (if (my-modeline--string-truncate-p str)
;;         (concat (substring str 0 half) "..." (substring str (- half)))
;;       str)))

;; (defun my-modeline--first-char (str)
;;   "Return first character from STR."
;;   (substring str 0 1))

;; (defun my-modeline-string-abbreviate (str)
;;   "Abbreviate STR individual hyphen or underscore separated words.
;; Also see `my-modeline-string-abbreviate-but-last'."
;;   (if (my-modeline--string-truncate-p str)
;;       (mapconcat #'my-modeline--first-char (split-string str "[_-]") "-")
;;     str))

;; (defun my-modeline-string-abbreviate-but-last (str nthlast)
;;   "Abbreviate STR, keeping NTHLAST words intact.
;; Also see `my-modeline-string-abbreviate'."
;;   (if (my-modeline--string-truncate-p str)
;;       (let* ((all-strings (split-string str "[_-]"))
;;              (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
;;              (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
;;              (first-component (mapconcat #'my-modeline--first-char nbutlast-strings "-"))
;;              (last-component (mapconcat #'identity last-strings "-")))
;;         (if (string-empty-p first-component)
;;             last-component
;;           (concat first-component "-" last-component)))
;;     str))

;; ;;;; Keyboard macro indicator

;; (defvar-local my-modeline-kbd-macro
;;     '(:eval
;;       (when (and (mode-line-window-selected-p) defining-kbd-macro)
;;         (propertize " KMacro " 'face 'my-modeline-indicator-blue-bg)))
;;   "Mode line construct displaying `mode-line-defining-kbd-macro'.
;; Specific to the current window's mode line.")

;; ;;;; Narrow indicator

;; (defvar-local my-modeline-narrow
;;     '(:eval
;;       (when (and (mode-line-window-selected-p)
;;                  (buffer-narrowed-p)
;;                  (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
;;         (propertize " Narrow " 'face 'my-modeline-indicator-cyan-bg)))
;;   "Mode line construct to report the multilingual environment.")

;; ;;;; Input method

;; (defvar-local my-modeline-input-method
;;     '(:eval
;;       (when current-input-method-title
;;         (propertize (format " %s " current-input-method-title)
;;                     'face 'my-modeline-indicator-green-bg
;;                     'mouse-face 'mode-line-highlight)))
;;   "Mode line construct to report the multilingual environment.")

;; ;;;; Buffer status

;; ;; TODO 2023-07-05: What else is there beside remote files?  If
;; ;; nothing, this must be renamed accordingly.
;; (defvar-local my-modeline-buffer-status
;;     '(:eval
;;       (when (file-remote-p default-directory)
;;         (propertize " @ "
;;                     'face 'my-modeline-indicator-red-bg
;;                     'mouse-face 'mode-line-highlight)))
;;   "Mode line construct for showing remote file name.")

;; ;;;; Dedicated window

;; (defvar-local my-modeline-window-dedicated-status
;;     '(:eval
;;       (when (window-dedicated-p)
;;         (propertize " = "
;;                     'face 'my-modeline-indicator-gray-bg
;;                     'mouse-face 'mode-line-highlight)))
;;   "Mode line construct for dedicated window indicator.")

;; ;;;; Buffer name and modified status

;; (defun my-modeline-buffer-identification-face ()
;;   "Return appropriate face or face list for `my-modeline-buffer-identification'."
;;   (let ((file (buffer-file-name)))
;;     (cond
;;      ((and (mode-line-window-selected-p)
;;            file
;;            (buffer-modified-p))
;;       '(italic mode-line-buffer-id))
;;      ((and file (buffer-modified-p))
;;       'italic)
;;      ((mode-line-window-selected-p)
;;       'mode-line-buffer-id))))

;; (defun my-modeline--buffer-name ()
;;   "Return `buffer-name', truncating it if necessary.
;; See `my-modeline-string-cut-middle'."
;;   (when-let ((name (buffer-name)))
;;     (my-modeline-string-cut-middle name)))

;; (defun my-modeline-buffer-name ()
;;   "Return buffer name, with read-only indicator if relevant."
;;   (let ((name (my-modeline--buffer-name)))
;;     (if buffer-read-only
;;         (format "%s %s" (char-to-string #xE0A2) name)
;;       name)))

;; (defun my-modeline-buffer-name-help-echo ()
;;   "Return `help-echo' value for `my-modeline-buffer-identification'."
;;   (concat
;;    (propertize (buffer-name) 'face 'mode-line-buffer-id)
;;    "\n"
;;    (propertize
;;     (or (buffer-file-name)
;;         (format "No underlying file.\nDirectory is: %s" default-directory))
;;     'face 'font-lock-doc-face)))

;; (defvar-local my-modeline-buffer-identification
;;     '(:eval
;;       (propertize (my-modeline-buffer-name)
;;                   'face (my-modeline-buffer-identification-face)
;;                   'mouse-face 'mode-line-highlight
;;                   'help-echo (my-modeline-buffer-name-help-echo)))
;;   "Mode line construct for identifying the buffer being displayed.
;; Propertize the current buffer with the `mode-line-buffer-id'
;; face.  Let other buffers have no face.")

;; ;;;; Major mode

;; (defun my-modeline-major-mode-indicator ()
;;   "Return appropriate propertized mode line indicator for the major mode."
;;   (let ((indicator (cond
;;                     ((derived-mode-p 'text-mode) "§")
;;                     ((derived-mode-p 'prog-mode) "λ")
;;                     ((derived-mode-p 'comint-mode) ">_")
;;                     (t "◦"))))
;;     (propertize indicator 'face 'shadow)))

;; (defun my-modeline-major-mode-name ()
;;   "Return capitalized `major-mode' without the -mode suffix."
;;   (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

;; (defun my-modeline-major-mode-help-echo ()
;;   "Return `help-echo' value for `my-modeline-major-mode'."
;;   (if-let ((parent (get major-mode 'derived-mode-parent)))
;;       (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
;;     (format "Symbol: `%s'." major-mode)))

;; (defvar-local my-modeline-major-mode
;;     (list
;;      (propertize "%[" 'face 'my-modeline-indicator-red)
;;      '(:eval
;;        (concat
;;         (my-modeline-major-mode-indicator)
;;         " "
;;         (propertize
;;          (my-modeline-string-abbreviate-but-last
;;           (my-modeline-major-mode-name)
;;           2)
;;          'mouse-face 'mode-line-highlight
;;          'help-echo (my-modeline-major-mode-help-echo))))
;;      (propertize "%]" 'face 'my-modeline-indicator-red))
;;   "Mode line construct for displaying major modes.")

;; (defvar-local my-modeline-process
;;     (list '("" mode-line-process))
;;   "Mode line construct for the running process indicator.")

;; ;;;; Git branch and diffstat

;; (declare-function vc-git--symbolic-ref "vc-git" (file))

;; (defun my-modeline--vc-branch-name (file backend)
;;   "Return capitalized VC branch name for FILE with BACKEND."
;;   (when-let ((rev (vc-working-revision file backend))
;;              (branch (or (vc-git--symbolic-ref file)
;;                          (substring rev 0 7))))
;;     (capitalize branch)))

;; ;; NOTE 2023-07-27: This is a good idea, but it hardcodes Git, whereas
;; ;; I want a generic VC method.  Granted, I only use Git but I still
;; ;; want it to work as a VC extension.

;; ;; (defun my-modeline-diffstat (file)
;; ;;   "Return shortened Git diff numstat for FILE."
;; ;;   (when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
;; ;;               (stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
;; ;;               (added (nth 0 stats))
;; ;;               (deleted (nth 1 stats)))
;; ;;     (cond
;; ;;      ((and (equal added "0") (equal deleted "0"))
;; ;;       "")
;; ;;      ((and (not (equal added "0")) (equal deleted "0"))
;; ;;       (propertize (format "+%s" added) 'face 'shadow))
;; ;;      ((and (equal added "0") (not (equal deleted "0")))
;; ;;       (propertize (format "-%s" deleted) 'face 'shadow))
;; ;;      (t
;; ;;       (propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

;; (declare-function vc-git-working-revision "vc-git" (file))

;; (defvar my-modeline-vc-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [mode-line down-mouse-1] 'vc-diff)
;;     (define-key map [mode-line down-mouse-3] 'vc-root-diff)
;;     map)
;;   "Keymap to display on VC indicator.")

;; (defun my-modeline--vc-help-echo (file)
;;   "Return `help-echo' message for FILE tracked by VC."
;;   (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
;;           (vc-working-revision file)))

;; (defun my-modeline--vc-text (file branch &optional face)
;;   "Prepare text for Git controlled FILE, given BRANCH.
;; With optional FACE, use it to propertize the BRANCH."
;;   (concat
;;    (propertize (char-to-string #xE0A0) 'face 'shadow)
;;    " "
;;    (propertize branch
;;                'face face
;;                'mouse-face 'mode-line-highlight
;;                'help-echo (my-modeline--vc-help-echo file)
;;                'local-map my-modeline-vc-map)
;;    ;; " "
;;    ;; (my-modeline-diffstat file)
;;    ))

;; (defun my-modeline--vc-details (file branch &optional face)
;;   "Return Git BRANCH details for FILE, truncating it if necessary.
;; The string is truncated if the width of the window is smaller
;; than `split-width-threshold'."
;;   (my-modeline-string-cut-end
;;    (my-modeline--vc-text file branch face)))

;; (defvar my-modeline--vc-faces
;;   '((added . vc-locally-added-state)
;;     (edited . vc-edited-state)
;;     (removed . vc-removed-state)
;;     (missing . vc-missing-state)
;;     (conflict . vc-conflict-state)
;;     (locked . vc-locked-state)
;;     (up-to-date . vc-up-to-date-state))
;;   "VC state faces.")

;; (defun my-modeline--vc-get-face (key)
;;   "Get face from KEY in `my-modeline--vc-faces'."
;;   (alist-get key my-modeline--vc-faces 'up-to-date))

;; (defun my-modeline--vc-face (file backend)
;;   "Return VC state face for FILE with BACKEND."
;;   (my-modeline--vc-get-face (vc-state file backend)))

;; (defvar-local my-modeline-vc-branch
;;     '(:eval
;;       (when-let* (((mode-line-window-selected-p))
;;                   (file (buffer-file-name))
;;                   (backend (vc-backend file))
;;                   ;; ((vc-git-registered file))
;;                   (branch (my-modeline--vc-branch-name file backend))
;;                   (face (my-modeline--vc-face file backend)))
;;         (my-modeline--vc-details file branch face)))
;;   "Mode line construct to return propertized VC branch.")

;; ;;;; Flymake errors, warnings, notes

;; (declare-function flymake--severity "flymake" (type))
;; (declare-function flymake-diagnostic-type "flymake" (diag))

;; ;; Based on `flymake--mode-line-counter'.
;; (defun my-modeline-flymake-counter (type)
;;   "Compute number of diagnostics in buffer with TYPE's severity.
;; TYPE is usually keyword `:error', `:warning' or `:note'."
;;   (let ((count 0))
;;     (dolist (d (flymake-diagnostics))
;;       (when (= (flymake--severity type)
;;                (flymake--severity (flymake-diagnostic-type d)))
;;         (cl-incf count)))
;;     (when (cl-plusp count)
;;       (number-to-string count))))

;; (defvar my-modeline-flymake-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
;;     (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
;;     map)
;;   "Keymap to display on Flymake indicator.")

;; (defmacro my-modeline-flymake-type (type indicator &optional face)
;;   "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
;;   `(defun ,(intern (format "my-modeline-flymake-%s" type)) ()
;;      (when-let ((count (my-modeline-flymake-counter
;;                         ,(intern (format ":%s" type)))))
;;        (concat
;;         (propertize ,indicator 'face 'shadow)
;;         (propertize count
;;                     'face ',(or face type)
;;                     'mouse-face 'mode-line-highlight
;;                     ;; FIXME 2023-07-03: Clicking on the text with
;;                     ;; this buffer and a single warning present, the
;;                     ;; diagnostics take up the entire frame.  Why?
;;                     'local-map my-modeline-flymake-map
;;                     'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

;; (my-modeline-flymake-type error "☣")
;; (my-modeline-flymake-type warning "!")
;; (my-modeline-flymake-type note "·" success)

;; (defvar-local my-modeline-flymake
;;     `(:eval
;;       (when (and (bound-and-true-p flymake-mode)
;;                  (mode-line-window-selected-p))
;;         (list
;;          ;; See the calls to the macro `my-modeline-flymake-type'
;;          '(:eval (my-modeline-flymake-error))
;;          '(:eval (my-modeline-flymake-warning))
;;          '(:eval (my-modeline-flymake-note)))))
;;   "Mode line construct displaying `flymake-mode-line-format'.
;; Specific to the current window's mode line.")

;; ;;;; Eglot

;; (with-eval-after-load 'eglot
;;   (setq mode-line-misc-info
;;         (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

;; (defvar-local my-modeline-eglot
;;     `(:eval
;;       (when (and (featurep 'eglot) (mode-line-window-selected-p))
;;         '(eglot--managed-mode eglot--mode-line-format)))
;;   "Mode line construct displaying Eglot information.
;; Specific to the current window's mode line.")

;; ;;;; Miscellaneous

;; (defvar-local my-modeline-notmuch-indicator
;;     '(notmuch-indicator-mode
;;       (" "
;;        (:eval (when (mode-line-window-selected-p)
;;                 notmuch-indicator--counters))))
;;   "The equivalent of `notmuch-indicator-mode-line-construct'.
;; Display the indicator only on the focused window's mode line.")

;; (defvar-local my-modeline-misc-info
;;     '(:eval
;;       (when (mode-line-window-selected-p)
;;         mode-line-misc-info))
;;   "Mode line construct displaying `mode-line-misc-info'.
;; Specific to the current window's mode line.")

;; ;;;; Risky local variables

;; ;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those
;; ;; variables will not work without it.
;; (dolist (construct '(my-modeline-kbd-macro
;;                      my-modeline-narrow
;;                      my-modeline-input-method
;;                      my-modeline-buffer-status
;;                      my-modeline-window-dedicated-status
;;                      my-modeline-buffer-identification
;;                      my-modeline-major-mode
;;                      my-modeline-process
;;                      my-modeline-vc-branch
;;                      my-modeline-flymake
;;                      my-modeline-eglot
;;                      ;; my-modeline-align-right
;;                      my-modeline-notmuch-indicator
;;                      my-modeline-misc-info))
;;   (put construct 'risky-local-variable t))

(provide 'k-modeline)
