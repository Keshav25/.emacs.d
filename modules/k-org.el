;; -*- lexical-binding: t -*-

(leaf org
  :bind (("C-c l" . org-store-link)
		 ("C-c C-M-l" . org-toggle-link-display))
  :hook (org-mode-hook . (turn-on-visual-line-mode))
  :custom
  (org-ellipsis . " ▾")
  (org-startup-numerated . 1)
  (org-startup-truncated . 1)
  (org-startup-indented . 1)
  (org-startup-with-inline-images . t)
  (org-image-actual-width . '(450))
  (org-appear-mode . 1)
  (org-use-speed-commands . 1)
  (org-id-link-to-org-use-id . t)
  (org-pretty-entities . t)
  (org-special-ctrl-a . 1)
  (org-special-ctrl-k . 1)
  (org-fontify-done-headline . t)
  (org-fontify-quote-and-verse-blocks . t)
  (org-fontify-whole-heading-line . t)
  (org-hidden-keywords . nil)
  (org-hide-emphasis-markers . t)
  (org-hide-leading-stars . t)
  (org-export-with-toc . nil)
  (org-agenda-search-view-always-boolean . t)
  (org-agenda-timegrid-use-ampm . nil)
  (org-agenda-span . 'month)
  (org-export-with-drawers . nil)
  (org-export-with-todo-keywords . nil)
  (org-export-with-smart-quotes . t)
  (org-export-date-timestamp-format . "%e %B %Y")
  (org-return-follows-link . t)
  (org-mouse-1-follows-link . t)
  (org-refile-use-outline-path . 'file)
  ;; (org-agenda-time-grid . ((daily today require-timed)
  ;; (600 630 700 730 800 830 900 930 1000 1030 1100 1130 1200 1230 1300 1330 1400 1430 1500 1530 1600 1630 1700 1730 1800 1830 2000 2030 2100 2130 2200 2230 2300 2330)
  ;; " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-log-done . 'time)
  (org-log-into-drawer . t)
  ;; (org-todo-keywords . ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
  ;; (sequence "BACKLOG(b)" "ACTIVE(a)"
  ;; "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
  ;; "|" "DELEGATED(D)" "CANCELLED(c)")))
  
  (org-highlight-latex-and-related '(native))
  (org-use-sub-superscripts . '{})
  (org-directory . "~/Documents/org")
  (org-file-apps . '((auto-mode . emacs)))
  (org-src-ask-before-returning-to-edit-buffer . nil)
  (org-src-window-setup . 'current-window)
  :config
  ;; TODO read https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Bemacs/org
  (defun k/org-summary-todo-naive-auto (n-done n-not-done)
	"Switch entry to DONE when all usbentries are done, to TODO otherwise."
	(org-todo (if (= n-note-done 0) "DONE" "TODO")))

  (defun k/org-summary-todo-semiauto (n-done n-not-done)
	"Prompt to change entry state when the state of the subentries imply it."
	(and (org-get-todo-state) ;; don't force a todo state if there is none yet
		 (if (or (and (org-entry-is-todo-p) (= n-not-done 0)) ;; if it should be in a todo state
				 (and (org-entry-is-todo-p) (> n-not-done 0))) ;; if it should be in a done state
			 (org-todo))))

  (defun k/with-save-excursion (orig-fun &rest args)
	"Execute the given function with save excursion."
	(save-excursion
	  (apply orig-fun args))))


(leaf org-speed-commands
  :custom
  (org-speed-commands .
					  '(("Outline Navigation")
						("n" . (org-speed-move-safe 'org-next-visible-heading))
						("p" . (org-speed-move-safe 'org-previous-visible-heading))
						("f" . (org-speed-move-safe 'org-forward-heading-same-level))
						("b" . (org-speed-move-safe 'org-backward-heading-same-level))
						("F" . org-next-block)
						("B" . org-previous-block)
						("u" . (org-speed-move-safe 'outline-up-heading))
						("j" . org-goto)
						("g" . (org-refile '(4)))
						("Outline Visibility")
						("c" . org-cycle)
						("C" . org-shifttab)
						(" " . org-display-outline-path)
						("s" . org-toggle-narrow-to-subtree)
						("k" . org-cut-subtree)
						("=" . org-columns)
						("Outline Structure Editing")
						("U" . org-metaup)
						("D" . org-metadown)
						("r" . org-metaright)
						("l" . org-metaleft)
						("R" . org-shiftmetaright)
						("L" . org-shiftmetaleft)
						("i" . (progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content)))
						("^" . org-sort)
						("w" . org-refile)
						("a" . org-archive-subtree-default-with-confirmation)
						("@" . org-mark-subtree)
						("#" . org-toggle-comment)
						("Clock Commands")
						("I" . org-clock-in)
						("O" . org-clock-out)
						("Meta Data Editing")
						("t" . org-todo)
						("," . (org-priority))
						("0" . (org-priority ?\ ))
						("1" . (org-priority ?A))
						("2" . (org-priority ?B))
						("3" . (org-priority ?C))
						(":" . org-set-tags-command)
						("e" . org-set-effort)
						("E" . org-inc-effort)
						("W" . (lambda (m) (interactive "sMinutes before warning: ") (org-entry-put (point) "APPT_WARNTIME" m)))
						("Agenda Views etc")
						("v" . org-agenda)
						("/" . org-sparse-tree)
						("Misc")
						("o" . org-open-at-point)
						("?" . org-speed-command-help)
						("<" . (org-agenda-set-restriction-lock 'subtree))
						(">" . (org-agenda-remove-restriction-lock))
						("A" . unpackaged/org-agenda-current-subtree-or-region))
					  ))

(leaf org-drawers
  :config
  (defun k-org-insert-notes-drawer ()
	"Generate or open a NOTES drawer under the current heading.
If a drawer exists for this section, a new line is created at the end of the
current note."
	(interactive)
	(push-mark)
	(org-previous-visible-heading 1)
	(forward-line)
	(if (looking-at-p "^[ \t]*:NOTES:")
		(progn
          (org-fold-hide-drawer-toggle 'off)
          (re-search-forward "^[ \t]*:END:" nil t)
          (forward-line -1)
          (org-end-of-line)
          (org-return))
      (org-insert-drawer nil "NOTES")))
  (defun k-org-count-words ()
	"Add word count to each heading property drawer in an Org mode buffer."
	(interactive)
	(org-map-entries
	 (lambda ()
       (let* ((start (point))
              (end (save-excursion (org-end-of-subtree)))
              (word-count (count-words start end)))
		 (org-set-property "WORDCOUNT" (number-to-string word-count)))))))

(leaf org-agenda-preview
  :config
  (require 'ov)
  (defface unpackaged/org-agenda-preview
	'((t (:background "black")))
	"Face for Org Agenda previews."
	:group 'org)

  (defun unpackaged/org-agenda-toggle-preview ()
	"Toggle overlay of current item in agenda."
	(interactive)
	(if-let* ((overlay (ov-in 'unpackaged/org-agenda-preview t (line-end-position) (line-end-position))))
		;; Hide existing preview
		(ov-reset overlay)
      ;; Show preview
      (let* ((entry-contents (--> (org-agenda-with-point-at-orig-entry
									  nil (buffer-substring (save-excursion
															  (unpackaged/org-forward-to-entry-content t)
															  (point))
															(org-entry-end-position)))
                                  s-trim
                                  (concat "\n" it "\n"))))
		(add-face-text-property 0 (length entry-contents)
								'unpackaged/org-agenda-preview nil entry-contents)
		(ov (line-end-position) (line-end-position)
			'unpackaged/org-agenda-preview t
			'before-string entry-contents))))

  (defun unpackaged/org-forward-to-entry-content (&optional unsafe)
	"Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
	(unless unsafe
      ;; To improve performance in loops (e.g. with `org-map-entries')
      (org-back-to-heading))
	(cl-loop for element = (org-element-at-point)
			 for pos = (pcase element
						 (`(headline . ,_) (org-element-property :contents-begin element))
						 (`(,(or 'planning 'property-drawer 'drawer) . ,_) (org-element-property :end element)))
			 while pos
			 do (goto-char pos))))

(leaf org-emphasis-kbd
  :disabled t
  :config
  (add-to-list 'org-emphasis-alist '("%" org-kbd verbatim))
  (add-to-list 'org-html-text-markup-alist '(kbd . "<kbd>%s</kbd>"))

  (defun org-html-kbd (_kbd contents info)
	"Transcode KBD from Org to HTML.
CONTENTS is the text with kbd markup. INFO is a plist holding
contextual information."
	(format (or (cdr (assq 'kbd (plist-get info :html-text-markup-alist))) "%s")
			contents))
  ;; Not sure if this is necessary.  See `org-element-update-syntax'.
  (org-set-emph-re 'org-emphasis-alist org-emphasis-alist)

  (defface org-kbd
	'((t (:inherit org-verbatim)))
	"Face for Org keyboard emphasis.")

  (defun org-element-kbd-parser ()
	"Parse kbd object at point, if any.
When at a kbd object, return a list whose car is `kbd' and cdr
is a plist with `:begin', `:end', `:contents-begin' and
`:contents-end' and `:post-blank' keywords. Otherwise, return
nil.
Assume point is at the first star marker."
	(save-excursion
      (unless (bolp) (backward-char 1))
      (when (looking-at org-emph-re)
		(let ((begin (match-beginning 2))
              (contents-begin (match-beginning 4))
              (contents-end (match-end 4))
              (post-blank (progn (goto-char (match-end 2))
								 (skip-chars-forward " \t")))
              (end (point)))
          (list 'kbd
				(list :begin begin
                      :end end
                      :contents-begin contents-begin
                      :contents-end contents-end
                      :post-blank post-blank))))))

  ;; MAYBE: Define a derived backend rather than replacing the HTML one.
  (org-export-define-backend 'html
							 '((kbd . org-html-kbd)
							   (bold . org-html-bold)
							   (center-block . org-html-center-block)
							   (clock . org-html-clock)
							   (code . org-html-code)
							   (drawer . org-html-drawer)
							   (dynamic-block . org-html-dynamic-block)
							   (entity . org-html-entity)
							   (example-block . org-html-example-block)
							   (export-block . org-html-export-block)
							   (export-snippet . org-html-export-snippet)
							   (fixed-width . org-html-fixed-width)
							   (footnote-definition . org-html-footnote-definition)
							   (footnote-reference . org-html-footnote-reference)
							   (headline . org-html-headline)
							   (horizontal-rule . org-html-horizontal-rule)
							   (inline-src-block . org-html-inline-src-block)
							   (inlinetask . org-html-inlinetask)
							   (inner-template . org-html-inner-template)
							   (italic . org-html-italic)
							   (item . org-html-item)
							   (keyword . org-html-keyword)
							   (latex-environment . org-html-latex-environment)
							   (latex-fragment . org-html-latex-fragment)
							   (line-break . org-html-line-break)
							   (link . org-html-link)
							   (node-property . org-html-node-property)
							   (paragraph . org-html-paragraph)
							   (plain-list . org-html-plain-list)
							   (plain-text . org-html-plain-text)
							   (planning . org-html-planning)
							   (property-drawer . org-html-property-drawer)
							   (quote-block . org-html-quote-block)
							   (radio-target . org-html-radio-target)
							   (section . org-html-section)
							   (special-block . org-html-special-block)
							   (src-block . org-html-src-block)
							   (statistics-cookie . org-html-statistics-cookie)
							   (strike-through . org-html-strike-through)
							   (subscript . org-html-subscript)
							   (superscript . org-html-superscript)
							   (table . org-html-table)
							   (table-cell . org-html-table-cell)
							   (table-row . org-html-table-row)
							   (target . org-html-target)
							   (template . org-html-template)
							   (timestamp . org-html-timestamp)
							   (underline . org-html-underline)
							   (verbatim . org-html-verbatim)
							   (verse-block . org-html-verse-block))
							 :filters-alist '((:filter-options . org-html-infojs-install-script)
											  (:filter-final-output . org-html-final-function))
							 :menu-entry
							 '(?h "Export to HTML"
								  ((?H "As HTML buffer" org-html-export-as-html)
								   (?h "As HTML file" org-html-export-to-html)
								   (?o "As HTML file and open"
									   (lambda (a s v b)
										 (if a (org-html-export-to-html t s v b)
										   (org-open-file (org-html-export-to-html nil s v b)))))))
							 :options-alist
							 '((:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
							   (:html-container "HTML_CONTAINER" nil org-html-container-element)
							   (:description "DESCRIPTION" nil nil newline)
							   (:keywords "KEYWORDS" nil nil space)
							   (:html-html5-fancy nil "html5-fancy" org-html-html5-fancy)
							   (:html-link-use-abs-url nil "html-link-use-abs-url" org-html-link-use-abs-url)
							   (:html-link-home "HTML_LINK_HOME" nil org-html-link-home)
							   (:html-link-up "HTML_LINK_UP" nil org-html-link-up)
							   (:html-mathjax "HTML_MATHJAX" nil "" space)
							   (:html-postamble nil "html-postamble" org-html-postamble)
							   (:html-preamble nil "html-preamble" org-html-preamble)
							   (:html-head "HTML_HEAD" nil org-html-head newline)
							   (:html-head-extra "HTML_HEAD_EXTRA" nil org-html-head-extra newline)
							   (:subtitle "SUBTITLE" nil nil parse)
							   (:html-head-include-default-style
								nil "html-style" org-html-head-include-default-style)
							   (:html-head-include-scripts nil "html-scripts" org-html-head-include-scripts)
							   (:html-allow-name-attribute-in-anchors
								nil nil org-html-allow-name-attribute-in-anchors)
							   (:html-divs nil nil org-html-divs)
							   (:html-checkbox-type nil nil org-html-checkbox-type)
							   (:html-extension nil nil org-html-extension)
							   (:html-footnote-format nil nil org-html-footnote-format)
							   (:html-footnote-separator nil nil org-html-footnote-separator)
							   (:html-footnotes-section nil nil org-html-footnotes-section)
							   (:html-format-drawer-function nil nil org-html-format-drawer-function)
							   (:html-format-headline-function nil nil org-html-format-headline-function)
							   (:html-format-inlinetask-function
								nil nil org-html-format-inlinetask-function)
							   (:html-home/up-format nil nil org-html-home/up-format)
							   (:html-indent nil nil org-html-indent)
							   (:html-infojs-options nil nil org-html-infojs-options)
							   (:html-infojs-template nil nil org-html-infojs-template)
							   (:html-inline-image-rules nil nil org-html-inline-image-rules)
							   (:html-link-org-files-as-html nil nil org-html-link-org-files-as-html)
							   (:html-mathjax-options nil nil org-html-mathjax-options)
							   (:html-mathjax-template nil nil org-html-mathjax-template)
							   (:html-metadata-timestamp-format nil nil org-html-metadata-timestamp-format)
							   (:html-postamble-format nil nil org-html-postamble-format)
							   (:html-preamble-format nil nil org-html-preamble-format)
							   (:html-table-align-individual-fields
								nil nil org-html-table-align-individual-fields)
							   (:html-table-caption-above nil nil org-html-table-caption-above)
							   (:html-table-data-tags nil nil org-html-table-data-tags)
							   (:html-table-header-tags nil nil org-html-table-header-tags)
							   (:html-table-use-header-tags-for-first-column
								nil nil org-html-table-use-header-tags-for-first-column)
							   (:html-tag-class-prefix nil nil org-html-tag-class-prefix)
							   (:html-text-markup-alist nil nil org-html-text-markup-alist)
							   (:html-todo-kwd-class-prefix nil nil org-html-todo-kwd-class-prefix)
							   (:html-toplevel-hlevel nil nil org-html-toplevel-hlevel)
							   (:html-use-infojs nil nil org-html-use-infojs)
							   (:html-validation-link nil nil org-html-validation-link)
							   (:html-viewport nil nil org-html-viewport)
							   (:html-inline-images nil nil org-html-inline-images)
							   (:html-table-attributes nil nil org-html-table-default-attributes)
							   (:html-table-row-open-tag nil nil org-html-table-row-open-tag)
							   (:html-table-row-close-tag nil nil org-html-table-row-close-tag)
							   (:html-xml-declaration nil nil org-html-xml-declaration)
							   (:infojs-opt "INFOJS_OPT" nil nil)
							   ;; Redefine regular options.
							   (:creator "CREATOR" nil org-html-creator-string)
							   (:with-latex nil "tex" org-html-with-latex)
							   ;; Retrieve LaTeX header for fragments.
							   (:latex-header "LATEX_HEADER" nil nil newline)))

  (defun org-element--object-lex (restriction)
	"Return next object in current buffer or nil.
RESTRICTION is a list of object types, as symbols, that should be
looked after. This function assumes that the buffer is narrowed
to an appropriate container (e.g., a paragraph)."
	(if (memq 'table-cell restriction) (org-element-table-cell-parser)
      (let* ((start (point))
			 (limit
              (save-excursion
				(cond ((not org-target-link-regexp) nil)
                      ((not (memq 'link restriction)) nil)
                      ((progn
						 (unless (bolp) (forward-char -1))
						 (not (re-search-forward org-target-link-regexp nil t)))
                       nil)
                      ;; Since we moved backward, we do not want to
                      ;; match again an hypothetical 1-character long
                      ;; radio link before us. Realizing that this can
                      ;; only happen if such a radio link starts at
                      ;; beginning of line, we prevent this here.
                      ((and (= start (1+ (line-beginning-position)))
							(= start (match-end 1)))
                       (and (re-search-forward org-target-link-regexp nil t)
							(match-beginning 1)))
                      (t (match-beginning 1)))))
			 found)
		(save-excursion
          (while (and (not found)
                      (re-search-forward org-element--object-regexp limit 'move))
			(goto-char (match-beginning 0))
			(let ((result (match-string 0)))
              (setq found
					(cond
					 ((string-prefix-p "call_" result t)
                      (and (memq 'inline-babel-call restriction)
                           (org-element-inline-babel-call-parser)))
					 ((string-prefix-p "src_" result t)
                      (and (memq 'inline-src-block restriction)
                           (org-element-inline-src-block-parser)))
					 (t
                      (pcase (char-after)
						(?^ (and (memq 'superscript restriction)
								 (org-element-superscript-parser)))
						(?_ (or (and (memq 'subscript restriction)
									 (org-element-subscript-parser))
								(and (memq 'underline restriction)
									 (org-element-underline-parser))))
						(?* (and (memq 'bold restriction)
								 (org-element-bold-parser)))
						(?/ (and (memq 'italic restriction)
								 (org-element-italic-parser)))
						(?~ (and (memq 'code restriction)
								 (org-element-code-parser)))
						(?= (and (memq 'verbatim restriction)
								 (org-element-verbatim-parser)))
						(?+ (and (memq 'strike-through restriction)
								 (org-element-strike-through-parser)))
						(?@ (and (memq 'export-snippet restriction)
								 (org-element-export-snippet-parser)))
						(?{ (and (memq 'macro restriction)
								 (org-element-macro-parser)))
						(?$ (and (memq 'latex-fragment restriction)
								 (org-element-latex-fragment-parser)))
						(?<
						 (if (eq (aref result 1) ?<)
							 (or (and (memq 'radio-target restriction)
                                      (org-element-radio-target-parser))
								 (and (memq 'target restriction)
                                      (org-element-target-parser)))
                           (or (and (memq 'timestamp restriction)
									(org-element-timestamp-parser))
                               (and (or (memq 'link restriction)
										(memq 'simple-link restriction))
									(org-element-link-parser)))))
						(?\\
						 (if (eq (aref result 1) ?\\)
							 (and (memq 'line-break restriction)
                                  (org-element-line-break-parser))
                           (or (and (memq 'entity restriction)
									(org-element-entity-parser))
                               (and (memq 'latex-fragment restriction)
									(org-element-latex-fragment-parser)))))
						(?\[
						 (if (eq (aref result 1) ?\[)
							 (and (memq 'link restriction)
                                  (org-element-link-parser))
                           (or (and (memq 'footnote-reference restriction)
									(org-element-footnote-reference-parser))
                               (and (memq 'timestamp restriction)
									(org-element-timestamp-parser))
                               (and (memq 'statistics-cookie restriction)
									(org-element-statistics-cookie-parser)))))
						;; NOTE: This is the new code.  I wish this didn't require modifying this function.
						(?% (and (memq 'kbd restriction)
								 (org-element-kbd-parser)))
						;; This is probably a plain link.
						(_ (and (or (memq 'link restriction)
									(memq 'simple-link restriction))
								(org-element-link-parser)))))))
              (or (eobp) (forward-char))))
          (cond (found)
				(limit (org-element-link-parser))	;radio link
				(t nil)))))
	))

(leaf org-insert-date
  :init
  (defun k-org-insert-date ()
	(interactive)
	(insert (shell-command-to-string "date")))
  :bind
  ("C-c i d" . k-org-insert-date))

(leaf org-inline-anim
  :elpaca t
  :hook (org-mode-hook . org-inline-anim-mode))

(leaf ob-rust :elpaca t)

(leaf ob-http
  :elpaca t)

(leaf ob-mermaid
  :elpaca t)

(leaf ob-csharp
  :elpaca (ob-csharp :host github :repo "samwdp/ob-csharp"))

(leaf ob-elixir :elpaca t)

(leaf ob-compile :elpaca t)

(leaf org-babel
  :after org
  :config
  (setq org-confirm-babel-evaluate nil)
  (require 'outline)
  (setq scimax-src-block-keymaps
		;; `(("ipython" . ,(let ((map (make-composed-keymap
		;; 							`(,elpy-mode-map ,python-mode-map ,pyvenv-mode-map)
		;; 							org-mode-map)))
        ;;                   ;; In org-mode I define RET so we f
        ;;                   (define-key map (kbd "<return>") 'newline)
        ;;                   (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
        ;;                   map))
        ;;   ("python" . ,(let ((map (make-composed-keymap
        ;;                            `(,elpy-mode-map ,python-mode-map ,pyvenv-mode-map)
        ;;                            org-mode-map)))
		;; 				 ;; In org-mode I define RET so we f
		;; 				 (define-key map (kbd "<return>") 'newline)
		;; 				 (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
		;; 				 map))
        `(("emacs-lisp" . ,(let ((map (make-composed-keymap `(
                                                              ,emacs-lisp-mode-map
                                                              ,outline-minor-mode-map)
															org-mode-map)))
							 (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
							 map))))
  (defun scimax-add-keymap-to-src-blocks (limit)
	"Add keymaps to src-blocks defined in `scimax-src-block-keymaps'."
	(let ((case-fold-search t)
          lang)
      (while (re-search-forward org-babel-src-block-regexp limit t)
		(let ((lang (match-string 2))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (if (assoc (org-no-properties lang) scimax-src-block-keymaps)
              (progn
				(add-text-properties
				 beg end `(local-map ,(cdr (assoc
											(org-no-properties lang)
											scimax-src-block-keymaps))))
				(add-text-properties
				 beg end `(cursor-sensor-functions
                           ((lambda (win prev-pos sym)
                              ;; This simulates a mouse click and makes a menu change
                              (org-mouse-down-mouse nil)))))))))))
  (defun scimax-add-keymap-to-src-blocks (limit)
	"Add keymaps to src-blocks defined in `scimax-src-block-keymaps'."
	(let ((case-fold-search t)
          lang)
      (while (re-search-forward org-babel-src-block-regexp limit t)
		(let ((lang (match-string 2))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (if (assoc (org-no-properties lang) scimax-src-block-keymaps)
              (progn
				(add-text-properties
				 beg end `(local-map ,(cdr (assoc
											(org-no-properties lang)
											scimax-src-block-keymaps))))
				(add-text-properties
				 beg end `(cursor-sensor-functions
                           ((lambda (win prev-pos sym)
                              ;; This simulates a mouse click and makes a menu change
                              (org-mouse-down-mouse nil)))))))))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
	 (dot . t)
	 (emacs-lisp . t)
	 (shell . t)
	 (scheme . t)
	 (go . t)
	 (rust . t)
	 (clojure . t)
	 (csharp .t )
	 (elixir . t)
	 (java . t)
	 (compile . t)
	 (C . t)
	 (cpp . t)
	 (R . t))))

(leaf org-agenda
  :bind (("C-c a" . org-agenda))
  :config
  ;; (setq org-agenda-files (directory-files org-directory nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
  (setq org-cycle-separator-lines 1)
  (defvar org-agenda-overriding-header)
  (defvar org-agenda-sorting-strategy)
  (defvar org-agenda-restrict)
  (defvar org-agenda-restrict-begin)
  (defvar org-agenda-restrict-end)
  (setq org-agenda-files (directory-files-recursively "~/Documents/notes/" (rx ".org" eos) nil
													  (lambda (subdir)
														(not (eq ?.
																 (string-to-char (file-name-nondirectory subdir)))))))
  (defun unpackaged/org-agenda-current-subtree-or-region (only-todos)
	"Display an agenda view for the current subtree or region.
 With prefix, display only TODO-keyword items."
	(interactive "P")
	(let ((starting-point (point))
          header)
      (with-current-buffer (or (buffer-base-buffer (current-buffer))
                               (current-buffer))
		(if (use-region-p)
			(progn
              (setq header "Region")
              (put 'org-agenda-files 'org-restrict (list (buffer-file-name (current-buffer))))
              (setq org-agenda-restrict (current-buffer))
              (move-marker org-agenda-restrict-begin (region-beginning))
              (move-marker org-agenda-restrict-end
                           (save-excursion
							 ;; If point is at beginning of line, include
							 ;; heading on that line by moving forward 1.
							 (goto-char (1+ (region-end)))
							 (org-end-of-subtree))))
          ;; No region; restrict to subtree.
          (save-excursion
			(save-restriction
              ;; In case the command was called from an indirect buffer, set point
              ;; in the base buffer to the same position while setting restriction.
              (widen)
              (goto-char starting-point)
              (setq header "Subtree")
              (org-agenda-set-restriction-lock))))
		;; NOTE: Unlike other agenda commands, binding `org-agenda-sorting-strategy'
		;; around `org-search-view' seems to have no effect.
		(let ((org-agenda-sorting-strategy '(priority-down timestamp-up))
              (org-agenda-overriding-header header))
          (org-search-view (if only-todos t nil) "*"))
		(org-agenda-remove-restriction-lock t)
		(message nil))))
  
  (defun unpackaged/org-agenda-olp (outline-path &optional file only-todos)
	"Show an agenda restricted to subtree at OUTLINE-PATH.
FILE may be a filename to search in, or nil to look in the
current buffer.  If ONLY-TODOS is non-nil, show only to-do
items. OUTLINE-PATH is a list of strings which are outline
headings.  See function `org-find-olp'."
	(when file
      (push file outline-path))
	(let ((marker (org-find-olp outline-path (not file))))
      (with-current-buffer (marker-buffer marker)
		(org-with-wide-buffer
		 (goto-char marker)
		 (unpackaged/org-agenda-current-subtree-or-region only-todos)))))
  
  :custom
  (org-refile-targets . '((org-agenda-files :maxlevel . 3)))
  (org-agenda-window-setup . 'current-window)
  (org-agenda-custom-commands .
							  '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
								("e" "Agenda, next actions and waiting"
								 ((agenda "" ((org-agenda-overriding-header "Next three days:")
											  (org-agenda-span 3)
											  (org-agenda-start-on-weekday nil)))
								  (todo "NEXT" ((org-agenda-overriding-header "Next Actions:")))
								  (todo "WAIT" ((org-agenda-overriding-header "Waiting:")))))))
  :hook ((org-agenda-finalize-hook . org-modern-agenda)
		 (org-agenda-finalize-hook . hl-line-mode)))

(leaf org-publish
  :custom
  (org-publish-project-alist .
							 '(("journal"
								:base-directory "~/Documents/journal/"
								:base-extension "org"
								:publishing-directory "~/org/neocities/journal"
								:recursive t
								:publishing-function org-html-publish-to-html
								:headline-levels 4
								:section-numbers nil
								:html-head nil
								:html-head-include-default-style nil
								:html-head-include-scripts nil
								:html-preamble my-blog-header
								:html-postamble my-blog-footer)
							   ("posts"
								:base-directory "~/Documents/posts/"
								:base-extension "org"
								:publishing-directory "~/org/neocities/posts"
								:recursive t
								:publishing-function org-html-publish-to-html
								:headline-levels 4
								:section-numbers nil
								:html-head nil
								:html-head-include-default-style nil
								:html-head-include-scripts nil
								:html-preamble my-blog-header
								:html-postamble my-blog-footer)
							   ("roam"
								:base-directory "~/org/roam/"
								:base-extension "org"
								:publishing-directory "~/org/zettelkasten/"
								:recursive t
								:publishing-function org-html-publish-to-html
								:headline-levels 4
								:section-numbers nil
								:html-head nil
								:html-head-include-default-style nil
								:html-head-include-scripts nil
								:html-preamble my-blog-header
								:html-postamble my-blog-footer)
							   )))



;; https://tech.toryanderson.com/2020/08/18/orgmode-system-notifications-with-dunst/
;; (setq appt-display-format 'window)

;; (setq appt-disp-window-function (function tsa/appt-disp-window))
;; (defun tsa/appt-disp-window (min-to-app new-time msg)
;;   (save-window-excursion
;;     (shell-command
;;      (concat
;;       "notify-send \"Orgmode: " msg "\"") nil nil)))

;; Org-Journal
(leaf org-journal
  :doc "using denote journal features instead now"
  :disabled t
  :elpaca t
  :custom
  (org-journal-file-format . "%Y%m%d.org")
  :bind
  ("C-c C-j" . org-journal-new-entry))

;; PDF Tools
(leaf pdf-tools
  :elpaca t
  :hook ((doc-view-mode-hook . (lambda () (require 'pdf-tools))))
  :config
  (pdf-tools-install)
  :setq-default
  (pdf-view-display-size . 'fit-width))

;; Calibre integration
(leaf calibredb
  :elpaca t
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(
								  ("~/Calibre Library")
								  )))

;; read epub files
(leaf nov
  :elpaca t)

;; https://depp.brause.cc/nov.el/
;; (use-package nov-xwidget
;;   :elpaca t
;;   :demand t
;;   :after nov
;;   :config
;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

;; Org-Noter
(leaf org-noter :elpaca t)
(leaf org-noter-pdftools :elpaca t)

;; Org-Roam
(leaf org-roam
  :disabled t
  :elpaca t
  :custom
  (org-roam-directory . "~/org/roam")
  (org-roam-completion-everywhere . t)
  (org-roam-completion-system . 'default)
  (org-roam-dailies-directory . "~/org/roam/daily")
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n g" . org-roam-graph)
		 ;; ("C-c n i" . org-roam-node-insert)
		 ("C-c n c" . org-roam-capture)
		 ;; Dailies
		 ("C-c n d" . org-roam-dailies-capture-today))
  
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)

  ;; Disable Warning About Org-Roamv2
  (setq org-roam-v2-ack t))

(leaf org-roam-ui
  :disabled t
  :after (org-roam)
  :elpaca t
  :config
  (setq org-roam-ui-sync-theme t
		org-roam-ui-follow t
		org-roam-ui-update-on-save t
		org-roam-ui-open-on-start t))

;; Org Roam UI
;; Commented due to the fact that I have not yet installed straigt.el
;; (use-package org-roam-ui
;;   :straight
;;     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;     :after org-roam
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;; Org-Modern-Mode now
;; Org-Bullets
;; (leaf org-bullets
;; :after org
;; :hook (org-mode . org-bullets-mode)
;; :custom
;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Org-Starter
(leaf org-starter
  :elpaca t)

(leaf org-timeblock
  :elpaca t)

(leaf org-context :elpaca t)
(leaf org-hyperscheduler :elpaca t)
(leaf org-custom-cookies :elpaca t)
(leaf org-wild-notifier :elpaca t)
(leaf org-transclusion :elpaca t)
(leaf org-time-budgets :elpaca t)
(leaf org-ref-prettify :elpaca t)
(leaf org-drill-table :elpaca t)
(leaf org-auto-tangle :elpaca t)
(leaf org-tree-slide :elpaca t)
(leaf org-tanglesync :elpaca t)
(leaf org-randomnote :elpaca t)
(leaf org-inline-pdf :elpaca t)
(leaf citar-org-roam :disabled t :elpaca t)
(leaf org-web-tools :elpaca t)
(leaf org-treeusage :elpaca t)
(leaf org-rich-yank :elpaca t)
(leaf org-review :elpaca t)

(leaf org-re-reveal
  :elpaca t
  :setq
  (org-re-reveal-root . "~/src/reveal.js")
  (org-re-reveal-subtree-with-title-slide . t)
  :config
  (add-to-list 'org-structure-template-alist '("R" . "#+REVEAL_HTML: ?\n")))


(leaf org-pdftools :elpaca t)
(leaf org-mind-map :elpaca t)
(leaf org-make-toc :elpaca t)

(leaf org-download
  :elpaca t
  :hook ((org-mode-hook . org-download-enable)))

(leaf org-contacts :elpaca t)

(leaf org-tagged
  :elpaca t
  :emacs>= 28.1)

(leaf org-kanban :elpaca t)
(leaf org-appear
  :elpaca t
  :hook ((org-mode-hook . org-appear-mode)))
(leaf org-emms :elpaca t)
(leaf org-edna :elpaca t)
(leaf org-ref :elpaca t)
(leaf org-msg :elpaca t)
(leaf org-gtd :elpaca t)
(leaf org-wc :elpaca t)
(leaf org-if :elpaca t)
(leaf org-fancy-priorities :elpaca t)

(leaf org-modern
  :elpaca t
  :after org
  :custom
  (org-modern-star . 'replace)
  :config
  (global-org-modern-mode))

(leaf org-present
  :elpaca t)

(leaf cdlatex
  :elpaca t
  :hook ((org-mode-hook . turn-on-org-cd-latex)))

(leaf org-mime :elpaca t)

(leaf writing-function
  :config
  (defun turn-on-writing ()
	(interactive)
	(display-line-numbers-mode 0)
	(visual-line-mode 1)
	(org-modern-mode 1))
  (defun turn-off-writing ()
	(interactive)
	(display-line-numbers-mode 1)
	(visual-line-mode 0)
	(org-modern-mode 0)))

(leaf pandoc :elpaca t)
(leaf pandoc-mode :elpaca t)
(leaf ox-pandoc :elpaca t)
(leaf copyit-pandoc :elpaca t)

(leaf gnuplot :elpaca t)
(leaf htmlize :elpaca t)

(leaf org-novelist
  :elpaca (org-novelist :host github :repo "sympodius/org-novelist")
  :setq
  (org-novelist-language-tag ."en-US")
  (org-novelist-author . user-full-name)
  (org-novelist-author-email . user-mail-address))

(leaf ankifier
  :after (expand-region)
  :elpaca (ankifier :host github :repo "adham-omran/ankifier")
  :setq
  (ankifier-insert-elsewhere . t)
  (ankifier-anki-basic-note-type . "E-Basic")
  (ankifier-anki-cloze-note-type . "E-Cloze")
  (ankifier-feedback . t)
  (ankifier-context-question . t))

(leaf org-pretty-tags
  :after org
  :elpaca t
  :config
  (org-pretty-tags-global-mode 1))

;; (leaf org-unique-id
;; :elpaca t
;; :after (org)
;; :hook ((before-save-hook . org-unique-id))

(leaf ox-haunt
  :elpaca t)

(leaf hyperbole
  :disabled t
  :elpaca t
  :config
  (hyperbole-mode 1))

(leaf org-ql
  :elpaca (org-ql :host github :repo "alphapapa/org-ql"
				  :files (:defaults (:exclude "helm-org-ql.el"))))

(leaf org-agenda-files-track-ql
  :elpaca t)

(leaf org-sticky-header
  :elpaca t)

(leaf org-bookmark-heading
  :elpaca t)

(leaf hammy
  :elpaca (hammy :host github :repo "alphapapa/hammy.el"))

(leaf weblorg
  :elpaca t)

;; from https://www.reddit.com/r/emacs/comments/d54ogp/emacs_doom_e17_org_mode_checkboxes/
;; (add-hook 'org-mode-hook
;; 		  ;; TODO: Use add-to-list prettify-symbols-alist instead of add-to-list"", to avoid duplicates
;; 		  (lambda () ")Beautify Org Checkbox Symbol"
;;             ;; These are nice unicode characters for checkboxes: ☐ ☑ ☒
;;             (add-to-list prettify-symbols-alist '("TODO" . "☐") )
;;             (add-to-list prettify-symbols-alist '("NEXT" . "Δ" ) )
;;             (add-to-list prettify-symbols-alist '("DONE" . "☑" ) )
;;             (add-to-list prettify-symbols-alist '("CANCELED" . "☒" ) )

;;             ;; This changed to be case sensitive in a recent update of doom
;;             (add-to-list prettify-symbols-alist '("#+BEGIN_SRC" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+END_SRC" . "⇤" ) )
;;             (add-to-list prettify-symbols-alist '("#+BEGIN_EXAMPLE" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+END_EXAMPLE" . "⇤" ) )
;;             (add-to-list prettify-symbols-alist '("#+BEGIN_QUOTE" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+END_QUOTE" . "⇤" ) )

;;             (add-to-list prettify-symbols-alist '("#+begin_quote" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+end_quote" . "⇤" ) )
;;             (add-to-list prettify-symbols-alist '("#+begin_example" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+end_example" . "⇤" ) )
;;             (add-to-list prettify-symbols-alist '("#+begin_src" . "↦" ) )
;;             (add-to-list prettify-symbols-alist '("#+end_src" . "⇤" ) )

;;             ;; Monday 2021-11-01 not working
;;             ;; (add-to-list prettify-symbols-alist '("+ [ ]" . "☐") )
;;             ;; (add-to-list prettify-symbols-alist '("+ [x]" . "☑" ) )
;;             ;; (add-to-list prettify-symbols-alist '("+ []" . "☒" ) )

;;             (prettify-symbols-mode)))

(leaf corg
  :elpaca (corg :host github :repo "isamert/corg.el"))

(leaf org-media-note
  :elpaca (org-media-note :host github :repo "yuchen-lea/org-media-note")
  :config
  (setq org-media-note-screenshot-image-dir "~/Document/notes/imgs/'"))

(leaf org-latest-git-commits
  :config
  (defun org-dblock-write:github-commits (params)
	(cl-destructuring-bind (&key (user "Keshav25") (repo ".emacs.d/") (n 5)
								 &allow-other-keys) params
      (let* ((num (max n 1))
			 (url (format "https://api.github.com/repos/%s/%s/commits?per_page=%d" user repo num))
			 (json (with-temp-buffer
					 (url-insert-file-contents url)
					 (json-parse-buffer :array-type 'list))))
		(insert "* Latest commits\n")
		(dolist (commit json)
          (let ((sha (substring (gethash "sha" commit) 0 7))
				(message (gethash "message" (gethash "commit" commit))))
			(insert (format "- [[https://github.com/%s/%s/commit/%s][%s]] - %s\n"
							user repo sha sha message))))))))

(leaf org-sprint
  :doc "If this works well then generalize it"
  :config
  (defun sprint-points-get-row-data ()
	(let ((row-plist '()))
      (setq row-plist (plist-put row-plist :title (org-get-heading t t t t))
			row-plist (plist-put row-plist :points (org-entry-get (point) "POINTS")))
      row-plist))

  (defun sprint-points-print-rows (rows)
	(let ((row (car rows)))
      (if row
          (let ((title (plist-get row :title))
				(points (plist-get row :points)))
			(insert-before-markers "| " title " | " points " |\n")
			(sprint-points-print-rows (cdr rows))))))

  (defun sprint-points-get-total (rows)
	(apply '+ (mapcar
               (lambda (row) (string-to-number (plist-get row :points))) rows)))

  (defun org-dblock-write:sprint-points (params)
	(let ((entries (org-map-entries
					'sprint-points-get-row-data
					"+POINTS>0/!-DONE"
					'file)))
      (insert "| Headline | Points |\n")
      (insert "|-||\n")
      (sprint-points-print-rows entries)
      (insert "|-||\n")
      (insert "| Total | "
              (number-to-string (sprint-points-get-total entries)) " |"))
	(org-table-align)))

(leaf org-special-block-extras
  :elpaca t
  :hook (org-mode-hook . #'org-special-block-extras-mode))

(leaf mermaid-mode
  :elpaca t)

(leaf calfw
  :elpaca t )

(leaf calfw-org
  :after calfw
  :elpaca t)

(leaf flyspell
  :disabled t
  :elpaca t
  :custom
  (ispell-program-name . "hunspell")
  (ispell-dictionary . "en_US")
  (flyspell-mark-duplications-flag . nil)
  (org-fold-core-style . 'overlays)
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US")
  :hook
  (text-mode-hook . flyspell-mode))

(leaf org-pomodoro
  :elpaca t)

(leaf org-supertag
  :disabled t
  :elpaca (org-supertag :host github :repo "yibie/org-supertag")
  :after org
  :config
  (org-supertag-setup))

(leaf org-embed
  :elpaca (org-embed :host github :repo "yibie/org-embed"))

(leaf clever-cite
  :elpaca (clever-cite :host github :repo "Hugo-Heagren/clever-cite")
  :config
  (defun my/org-clever-cite-quote-string (str cite-key &optional ref)
    "Insert STR inside an org quote block, ending with a CITE-KEY citation.
If REF is non-nil, inlude it in the citation. Fill the text with
`org-fill-paragraph'."
    (insert "#+begin_quote\n")
    (let ((beg (point))
		  (_ (progn (insert str "\n"))))
      (insert
       (if cite-key
		   (format "\n[cite:@%s%s]\n"
				   cite-key
				   (if ref (format " %s" ref) "")))
       "#+end_quote\n")
      (save-mark-and-excursion
		(set-mark beg)
		;; Use Org's own filling machinery
		(org-fill-paragraph nil 'region)))
    ;; Signal that we have handled insertion
    t))


;; TODO: Configure Org-Invoice-Table
(leaf org-invoice-table
  :disabled t
  :elpaca t)

(leaf org-feh
  :require 'ol

  :config
  (defun feh-org-link-init ()
	"Setup org link with `mpv' prefix."
	(require 'org)
	(org-link-set-parameters "feh"
							 :follow #'org-feh-open)
	)
  (org-link-set-parameters  "feh"
							:follow #'org-feh-open)

  (defun org-feh-open (path _)
	(async-shell-command  (concat  "gthumb " path))))

(leaf org-workbench
  :elpaca (org-workbench :host github :repo "yibie/org-workbench")
  :commands org-workbench-show
  ;; :after org-supertag
  :config
  (org-workbench-setup))

(leaf org-social
  :disabled t
  :elpaca (org-social :host github :repo "tanrax/org-social.el")
  :custom
  (org-social-file . "~/Documents/notes/social.org")
  (org-social-relay . "https://org-social-relay.andros.dev/")
  (org-social-my-public-url . "https://thepoetlogician.neocities.org"))

(provide 'k-org)
