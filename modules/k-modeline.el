(leaf spacious-padding
  :elpaca t
  :custom
  (spacious-padding-widths . '(:internal-border-width 25
													  :header-line-width 0
													  :mode-line-width 0
													  :tab-width 4
													  :right-divider-width 30
													  :scroll-bar-width 8
													  :fringe-width 8))
  :config
  ;; (spacious-padding-mode 1)
  )

(leaf modeline
  :config
  (setq mode-line-compact nil)
  (setq mode-line-right-align-edge 'right-margin) 

  (setq mode-line-format nil)
  (defface k-modeline-indicator-button nil
	"Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")
  (defun k/modeline-spacious-indicators ()
    "Set box attribute to `'k-modeline-indicator-button' if spacious-padding is enabled."
    (if (bound-and-true-p spacious-padding-mode)
        (set-face-attribute 'k-modeline-indicator-button nil :box t)
      (set-face-attribute 'k-modeline-indicator-button nil :box 'unspecified)))

  (k/modeline-spacious-indicators)

  (kill-local-variable 'mode-line-format)
  
  (force-mode-line-update)

  (setq mode-line-format
		'("%e"
		  k-modeline-buffer-name
 		  k-modeline-kbd-macro
          k-modeline-narrow
          k-modeline-input-method
          k-modeline-buffer-status
		  k-modeline-window-dedicated-status
          " "
          k-modeline-buffer-identification
          "  "
          k-modeline-major-mode
		  k-modeline-process
          "  "
          k-modeline-vc-branch
          "  "
          k-modeline-flymake
		  k-modeline-eglot
	      k-modeline-misc-info))

  (defface k-modeline-background
	'((t :background "#3355bb" :foreground "white" :inherit bold))
	"Face with a red background for use on the mode line.")

  (defun k-modeline--buffer-name ()
	"Return `buffer-name' with spaces around it."
	(format " %s " (buffer-name)))

  (defvar-local k-modeline-buffer-name
      '(:eval
		(when (mode-line-window-selected-p)
          (propertize (k-modeline--buffer-name) 'face 'k-modeline-background)))
	"Mode line construct to display the buffer name.")

  (put 'k-modeline-buffer-name 'risky-local-variable t)

  (defun k-modeline--major-mode-name ()
	"Return capitalized `major-mode' as a string."
	(capitalize (symbol-name major-mode)))

  (defvar-local k-modeline-major-mode
      '(:eval
		(list
		 (propertize "λ" 'face 'shadow)
		 " "
		 (propertize (k-modeline--major-mode-name) 'face 'bold)))
	"Mode line construct to display the major mode.")

  (put 'k-modeline-major-mode 'risky-local-variable t)

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

  (defun k-modeline--string-truncate-p (str)
	"Return non-nil if STR should be truncated."
	(cond
	 ((or (not (stringp str))
          (string-empty-p str)
          (string-blank-p str))
      nil)
	 ((and (prot-common-window-narrow-p)
           (> (length str) prot-modeline-string-truncate-length)
           (not (one-window-p :no-minibuffer))))))

  (defun k-modeline--truncate-p ()
	"Return non-nil if truncation should happen.
This is a more general and less stringent variant of
`k-modeline--string-truncate-p'."
	(and (k-common-window-narrow-p)
		 (not (one-window-p :no-minibuffer))))

  (defun k-modeline-string-cut-end (str)
	"Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`k-modeline-string-truncate-length'."
	(if (k-modeline--string-truncate-p str)
		(concat (substring str 0 k-modeline-string-truncate-length) "...")
      str))

  (defun k-modeline-string-cut-beginning (str)
	"Return truncated STR, if appropriate, else return STR.
Cut off the beginning of STR by counting from its end up to
`k-modeline-string-truncate-length'."
	(if (k-modeline--string-truncate-p str)
		(concat "..." (substring str (- k-modeline-string-truncate-length)))
      str))

  (defun k-modeline-string-cut-middle (str)
	"Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`k-modeline-string-truncate-length' both from its beginning
and end."
	(let ((half (floor k-modeline-string-truncate-length 2)))
      (if (k-modeline--string-truncate-p str)
          (concat (substring str 0 half) "..." (substring str (- half)))
		str)))

  (defun k-modeline--first-char (str)
	"Return first character from STR."
	(substring str 0 1))

  (defun k-modeline-string-abbreviate (str)
	"Abbreviate STR individual hyphen or underscore separated words.
Also see `k-modeline-string-abbreviate-but-last'."
	(if (k-modeline--string-truncate-p str)
		(mapconcat #'k-modeline--first-char (split-string str "[_-]") "-")
      str))

  (defun k-modeline-string-abbreviate-but-last (str nthlast)
	"Abbreviate STR, keeping NTHLAST words intact.
Also see `k-modeline-string-abbreviate'."
	(if (k-modeline--string-truncate-p str)
		(let* ((all-strings (split-string str "[_-]"))
               (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
               (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
               (first-component (mapconcat #'k-modeline--first-char nbutlast-strings "-"))
               (last-component (mapconcat #'identity last-strings "-")))
          (if (string-empty-p first-component)
              last-component
			(concat first-component "-" last-component)))
      str))

;;;; Keyboard macro indicator

  (defvar-local k-modeline-kbd-macro
      '(:eval
		(when (and (mode-line-window-selected-p) defining-kbd-macro)
          (propertize " KMacro " 'face 'k-modeline-indicator-blue-bg)))
	"Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

  (put 'k-modeline-kbd-macro 'risky-local-variable t)

;;;; Narrow indicator

  (defvar-local k-modeline-narrow
      '(:eval
		(when (and (mode-line-window-selected-p)
                   (buffer-narrowed-p)
                   (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
          (propertize " Narrow " 'face 'k-modeline-indicator-cyan-bg)))
	"Mode line construct to report the narrowed state of the current buffer.")

  (put 'k-modeline-narrow 'risky-local-variable t)
;;;; Input method

  (defvar-local k-modeline-input-method
      '(:eval
		(when current-input-method-title
          (propertize (format " %s " current-input-method-title)
                      'face 'k-modeline-indicator-green-bg
                      'mouse-face 'mode-line-highlight)))
	"Mode line construct to report the multilingual environment.")

  (put 'k-modeline-input-method 'risky-local-variable t)

;;;; Buffer status

  ;; TODO 2023-07-05: What else is there beside remote files?  If
  ;; nothing, this must be renamed accordingly.
  (defvar-local k-modeline-buffer-status
      '(:eval
		(when (file-remote-p default-directory)
          (propertize " @ "
                      'face 'k-modeline-indicator-red-bg
                      'mouse-face 'mode-line-highlight)))
	"Mode line construct for showing remote file name.")

  (put 'k-modeline-buffer-status 'risky-local-variable t)

;;;; Dedicated window

  (defvar-local k-modeline-window-dedicated-status
      '(:eval
		(when (window-dedicated-p)
          (propertize " = "
                      'face 'k-modeline-indicator-gray-bg
                      'mouse-face 'mode-line-highlight)))
	"Mode line construct for dedicated window indicator.")

  (put 'k-modeline-window-dedicated-status 'risky-local-variable t)
;;;; Buffer name and modified status

  (defun k-modeline-buffer-identification-face ()
	"Return appropriate face or face list for `k-modeline-buffer-identification'."
	(let ((file (buffer-file-name)))
      (cond
       ((and (mode-line-window-selected-p)
			 file
			 (buffer-modified-p))
		'(italic mode-line-buffer-id))
       ((and file (buffer-modified-p))
		'italic)
       ((mode-line-window-selected-p)
		'mode-line-buffer-id))))

  (defun k-modeline--buffer-name ()
	"Return `buffer-name', truncating it if necessary.
See `k-modeline-string-cut-middle'."
	(when-let* ((name (buffer-name)))
      (k-modeline-string-cut-middle name)))

  (defun k-modeline-buffer-name ()
	"Return buffer name, with read-only indicator if relevant."
	(let ((name (k-modeline--buffer-name)))
      (if buffer-read-only
          (format "%s %s" (char-to-string #xE0A2) name)
		name)))

  (defun k-modeline-buffer-name-help-echo ()
	"Return `help-echo' value for `k-modeline-buffer-identification'."
	(concat
	 (propertize (buffer-name) 'face 'mode-line-buffer-id)
	 "\n"
	 (propertize
      (or (buffer-file-name)
          (format "No underlying file.\nDirectory is: %s" default-directory))
      'face 'font-lock-doc-face)))

  (defvar-local k-modeline-buffer-identification
      '(:eval
		(propertize (k-modeline-buffer-name)
					'face (k-modeline-buffer-identification-face)
					'mouse-face 'mode-line-highlight
					'help-echo (k-modeline-buffer-name-help-echo)))
	"Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

  (put 'k-modeline-buffer-identification 'risky-local-variable t)

;;;; Major mode

  (defun k-modeline-major-mode-indicator ()
	"Return appropriate propertized mode line indicator for the major mode."
	(let ((indicator (cond
                      ((derived-mode-p 'text-mode) "§")
                      ((derived-mode-p 'prog-mode) "λ")
                      ((derived-mode-p 'comint-mode) ">_")
                      (t "◦"))))
      (propertize indicator 'face 'shadow)))

  (defun k-modeline-major-mode-name ()
	"Return capitalized `major-mode' without the -mode suffix."
	(capitalize (string-replace "-mode" "" (symbol-name major-mode))))

  (defun k-modeline-major-mode-help-echo ()
	"Return `help-echo' value for `k-modeline-major-mode'."
	(if-let* ((parent (get major-mode 'derived-mode-parent)))
		(format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
      (format "Symbol: `%s'." major-mode)))

  (defvar-local k-modeline-major-mode
      (list
       (propertize "%[" 'face 'k-modeline-indicator-red)
       '(:eval
		 (concat
          (k-modeline-major-mode-indicator)
          " "
          (propertize
           (k-modeline-string-abbreviate-but-last
			(k-modeline-major-mode-name)
			2)
           'mouse-face 'mode-line-highlight
           'help-echo (k-modeline-major-mode-help-echo))))
       (propertize "%]" 'face 'k-modeline-indicator-red))
	"Mode line construct for displaying major modes.")

  (put 'k-modeline-major-mode 'risky-local-variable t)

  (defvar-local k-modeline-process
      (list '("" mode-line-process))
	"Mode line construct for the running process indicator.")

  (put 'k-modeline-process 'risky-local-variable t)

;;;; Git branch and diffstat

  (declare-function vc-git--symbolic-ref "vc-git" (file))

  (defun k-modeline--vc-branch-name (file backend)
	"Return capitalized VC branch name for FILE with BACKEND."
	(when-let* ((rev (vc-working-revision file backend))
				(branch (or (vc-git--symbolic-ref file)
							(substring rev 0 7))))
      (capitalize branch)))

										; NOTE 2023-07-27: This is a good idea, but it hardcodes Git, whereas
  ;; I want a generic VC method.  Granted, I only use Git but I still
  ;; want it to work as a VC extension.

  (defun k-modeline-diffstat (file)
	"Return shortened Git diff numstat for FILE."
	(when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
				(stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
				(added (nth 0 stats))
				(deleted (nth 1 stats)))
      (cond
       ((and (equal added "0") (equal deleted "0"))
		"")
       ((and (not (equal added "0")) (equal deleted "0"))
		(propertize (format "+%s" added) 'face 'shadow))
       ((and (equal added "0") (not (equal deleted "0")))
		(propertize (format "-%s" deleted) 'face 'shadow))
       (t
		(propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

  (declare-function vc-git-working-revision "vc-git" (file))

  (defvar k-modeline-vc-map
	(let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1] 'vc-diff)
      (define-key map [mode-line down-mouse-3] 'vc-root-diff)
      map)
	"Keymap to display on VC indicator.")

  (defun k-modeline--vc-help-echo (file)
	"Return `help-echo' message for FILE tracked by VC."
	(format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
			(vc-working-revision file)))

  (defun k-modeline--vc-text (file branch &optional face)
	"Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
	(concat
	 (propertize (char-to-string #xE0A0) 'face 'shadow)
	 " "
	 (propertize branch
				 'face face
				 'mouse-face 'mode-line-highlight
				 'help-echo (k-modeline--vc-help-echo file)
				 'local-map k-modeline-vc-map)
	 ;; " "
	 ;; (k-modeline-diffstat file)
	 ))

  (defun k-modeline--vc-details (file branch &optional face)
	"Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
	(k-modeline-string-cut-end
	 (k-modeline--vc-text file branch face)))

  (defvar k-modeline--vc-faces
	'((added . vc-locally-added-state)
      (edited . vc-edited-state)
      (removed . vc-removed-state)
      (missing . vc-missing-state)
      (conflict . vc-conflict-state)
      (locked . vc-locked-state)
      (up-to-date . vc-up-to-date-state))
	"VC state faces.")

  (defun k-modeline--vc-get-face (key)
	"Get face from KEY in `k-modeline--vc-faces'."
	(alist-get key k-modeline--vc-faces 'up-to-date))

  (defun k-modeline--vc-face (file backend)
	"Return VC state face for FILE with BACKEND."
	(k-modeline--vc-get-face (vc-state file backend)))

  (defvar-local k-modeline-vc-branch
      '(:eval
		(when-let* (((mode-line-window-selected-p))
					(file (buffer-file-name))
					(backend (vc-backend file))
					;; ((vc-git-registered file))
					(branch (k-modeline--vc-branch-name file backend))
					(face (k-modeline--vc-face file backend)))
          (k-modeline--vc-details file branch face)))
	"Mode line construct to return propertized VC branch.")

  (put 'k-modeline-vc-branch 'risky-local-variable t)

;;;; Flymake errors, warnings, notes

  (declare-function flymake--severity "flymake" (type))
  (declare-function flymake-diagnostic-type "flymake" (diag))

  ;; Based on `flymake--mode-line-counter'.
  (defun k-modeline-flymake-counter (type)
	"Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
	(let ((count 0))
      (dolist (d (flymake-diagnostics))
		(when (= (flymake--severity type)
				 (flymake--severity (flymake-diagnostic-type d)))
          (cl-incf count)))
      (when (cl-plusp count)
		(number-to-string count))))

  (defvar k-modeline-flymake-map
	(let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
      (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
      map)
	"Keymap to display on Flymake indicator.")

  (defmacro k-modeline-flymake-type (type indicator &optional face)
	"Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
	`(defun ,(intern (format "k-modeline-flymake-%s" type)) ()
       (when-let* ((count (k-modeline-flymake-counter
                           ,(intern (format ":%s" type)))))
		 (concat
          (propertize ,indicator 'face 'shadow)
          (propertize count
                      'face ',(or face type)
                      'mouse-face 'mode-line-highlight
                      ;; FIXME 2023-07-03: Clicking on the text with
                      ;; this buffer and a single warning present, the
                      ;; diagnostics take up the entire frame.  Why?
                      'local-map k-modeline-flymake-map
                      'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

  (k-modeline-flymake-type error "☣")
  (k-modeline-flymake-type warning "!")
  (k-modeline-flymake-type note "·" success)

  (defvar-local k-modeline-flymake
      `(:eval
		(when (and (bound-and-true-p flymake-mode)
                   (mode-line-window-selected-p))
          (list
           ;; See the calls to the macro `k-modeline-flymake-type'
           '(:eval (k-modeline-flymake-error))
           '(:eval (k-modeline-flymake-warning))
           '(:eval (k-modeline-flymake-note)))))
	"Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

  (put 'k-modeline-flymake 'risky-local-variable t)

;;;; Eglot

  (with-eval-after-load 'eglot
	(setq mode-line-misc-info
          (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

  (defvar-local k-modeline-eglot
      `(:eval
		(when (and (featurep 'eglot) (mode-line-window-selected-p))
          '(eglot--managed-mode eglot--mode-line-format)))
	"Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

  (put 'k-modeline-eglot 'risky-local-variable t)

;;;; Miscellaneous

  (defvar-local k-modeline-notmuch-indicator
      '(notmuch-indicator-mode
		(" "
		 (:eval (when (mode-line-window-selected-p)
                  notmuch-indicator--counters))))
	"The equivalent of `notmuch-indicator-mode-line-construct'.
Display the indicator only on the focused window's mode line.")

  (put 'k-modeline-notmuch-indicator 'risky-local-variable t)

  (defvar-local k-modeline-misc-info
      '(:eval
		(when (mode-line-window-selected-p)
          mode-line-misc-info))
	"Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line."))

(put 'k-modeline-misc-info 'risky-local-variable t)

;; ;;;; Right side alignment
;;
(defun prot-modeline--right-align-rest ()
  "Return string if everything after `prot-modeline-align-right'."
  (format-mode-line
   `(""
     ,@(cdr (memq 'prot-modeline-align-right mode-line-format)))))

(defun prot-modeline--right-align-width ()
  "Return pixel width of `prot-modeline--right-align-rest'."
  (string-pixel-width (prot-modeline--right-align-rest)))

(defun prot-modeline--box-p ()
  "Return non-nil if the `mode-line' has a box attribute."
  (when-let ((box (face-attribute 'mode-line :box))
             ((null (eq (face-attribute 'mode-line :box) 'unspecified))))
    (or (plist-get box :line-width)
        t)))
;;
;; ;; NOTE 2023-07-13: I could also do what I am doing in
;; ;; `fontaine--family-list-variable-pitch' and check if the family is a
;; ;; member of those, but I don't need that as I always inherit
;; ;; `variable-pitch' in my themes instead of hardcoding the family.
(defun prot-modeline--variable-pitch-p ()
  "Return non-nil if the `mode-line' inherits `variable-pitch'."
  (when-let* ((mode-line-inherit (face-attribute 'mode-line :inherit))
              ((string-match-p "variable-pitch" (symbol-name mode-line-inherit)))
              (family-face (face-attribute mode-line-inherit :inherit))
              (variable-pitch
               (if (listp family-face)
                   (memq 'variable-pitch family-face)
                 (eq 'variable-pitch family-face))))
    variable-pitch))

;; ;; I just came up with this experimentally, but I am not sure if it is
;; ;; the best approach.
(defun prot-modeline--magic-number ()
  "Return constant for use in `prot-modeline-align-right'."
  (let ((height (face-attribute 'mode-line :height nil 'default))
        (m-width (string-pixel-width (propertize "m" 'face 'mode-line))))
    (round height (* m-width (* height m-width 0.001)))))

(defvar-local k-modeline-align-right
    '(:eval
      (propertize
       " "
       'display
       (let* ((box (prot-modeline--box-p))
              (box-natnum-p (natnump box))
              (variable-pitch-p (prot-modeline--variable-pitch-p))
              (magic-number (prot-modeline--magic-number)))
         `(space
           :align-to
           (- right
              right-fringe
              right-margin
              ,(ceiling
                (prot-modeline--right-align-width)
                (string-pixel-width (propertize "m" 'face 'mode-line)))
              ,(cond
                ;; FIXME 2023-07-13: These hardcoded numbers are
                ;; probably wrong in some case.  I am still testing.
                ((and box-natnum-p variable-pitch-p)
                 (+ (* box 2.375) magic-number))
                (box-natnum-p
                 (* magic-number (* box 1.15)))
                ((and variable-pitch-p box)
                 (* magic-number 0.5))
                ((and (not variable-pitch-p) box)
                 (* magic-number 0.25))
                ((and variable-pitch-p (not box))
                 0)
                ;; No box, no variable pitch, but I am keeping it as
                ;; the fallback for the time being.
                (t (* magic-number -0.1))))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.")

(put 'k-modeline-align-right 'risky-local-variable t)



(leaf keycast
  :after vim-tab-bar
  :elpaca t
  :require t
  :config
  ;; (setq keycast-mode-line-format "%2s%k%c%R")
  ;; (setq keycast-mode-line-insert-after 'k-modeline-major-mode)
  ;; (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  ;; (setq keycast-mode-line-remove-tail-elements nil)
  ;; (keycast-mode-line-mode)
  (keycast-tab-bar-mode 1)
  (dolist (input '(self-insert-command org-self-insert-command))
	(add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
	(add-to-list 'keycast-substitute-alist `(,event nil))))

(leaf mlscroll
  :elpaca t
  :config
  (mlscroll-mode 1))

(leaf mode-line-keyboard
  :disabled t
  :elpaca t)

(provide 'k-modeline)
