(leaf org
  :ensure t
  :bind (("C-c l" . org-store-link)
		 ("C-c C-M-l" . org-toggle-link-display))
  :hook (org-mode . (turn-on-visual-line-mode))
  :custom
  (org-ellipsis . " ▾")
  (org-startup-numerated . 1)
  (org-startup-truncated . 1)
  (org-startup-indented . 1)
  (org-appear-mode . 1)
  (org-use-speed-commands . 1)
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
  (org-agenda-span . 'day)
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
  )

(leaf org-emphasis-kbd
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
  :config
  (defun k-org-insert-date ()
	(interactive)
	(insert (shell-command-to-string "date")))
  :bind
  ("C-c i d" . k-org-insert-date))

(leaf org-babel
  :init
  (leaf ob-go
	:ensure t)
  (leaf ob-csharp
	:quelpa (ob-csharp :fetcher github :repo "samwdp/ob-csharp"))

  (setq org-confirm-babel-evaluate nil)
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
	 (emacs-lisp . t)
	 (shell . t)
	 (scheme . t)
	 (go . t)
	 (clojure . t)
	 (csharp .t))))

(leaf org-agenda
  :bind (("C-c a" . org-agenda))
  :config
  (defun k/reload-org-agenda-files ()
	"Refreshes org-agenda-files directory to get latest updates"
	(interactive)
	(setq org-agenda-files (directory-files "~/Documents/notes/" 'full (rx ".org" eos))))
  
  (k/reload-org-agenda-files)
  ;; (setq org-agenda-files (directory-files org-directory nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
  (setq org-cycle-separator-lines 1)
  :custom
  (org-refile-targets . '((org-agenda-files :maxlevel . 3)))
  (org-agenda-window-setup . 'current-window)
  :hook ((org-agenda-mode . k/reload-org-agenda-files)
		 (org-agenda-finalize . org-modern-agenda)
		 (org-agenda-finalize . hl-line-mode)))

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

(leaf org-capture
  :bind (("C-c c" . org-capture))
  :custom
  (org-capture-templates .
						 '(("n" "New note with Denote" plain
							(file denote-last-path)
							#'denote-org-capture
							:no-save t
							:immediate-finish nil
							:kill-buffer t
							:jump-to-captured t)
						   ("f" "Fleeting notes" plain
							(file denote-last-path)
							#'denote-org-capture
							:no-save t
							:immediate-finish nil
							:kill-buffer t
							:jump-to-captured t)
						   ("b" "Books" plain
							(file denote-last-path)
							#'denote-org-capture
							:no-save t
							:immediate-finish nil
							:kill-buffer t
							:jump-to-captured t))))

;; :custom
;; (org-capture-templates
;;  .
;;  '(
;;    ("L" "Protocol Link" entry (file+headline "inbox.org" "Inbox")
;; 	"* %? %:annotation\n")
;;    ("i" "inbox" entry
;; 	(file "~/Documents/org/inbox.org")
;; 	"* %?\n %T\n %a\n %i\n"
;; 	:empty-lines 1 )
;;    ("e" "emacs"
;; 	entry (file "~/Documents/org/emacs-notes.org")
;; 	"* %?\n %T\n %i\n"
;; 	:empty-lines 1)
;;    ("c" "compsol"
;; 	entry (file "~/Documents/org/compsol.org")
;; 	"* %?\n %T\n %i\n"
;; 	:empty-lines 1)
;;    ("s" "school"
;; 	entry (file "~/Documents/org/school.org")
;; 	"* %?\n %T\n %i\n"
;; 	:empty-lines 1)
;;    ("f" "food"
;; 	entry (file "~/Documents/org/food-journal.org")
;; 	"* %?\n %T\n %i\n"
;; 	:empty-lines 1)
;;    ("d" "daily-template"
;; 	entry
;; 	(file+olp+datetree "daily.org")
;; 	"%[~/Documents/org/daily-template]"
;; 	;; :unnarrowed 1
;; 	:tree-type week
;; 	;; :time-prompt t
;; 	))))

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
  :ensure t
  :custom
  (org-journal-file-format . "%Y%m%d.org")
  :bind
  ("C-c C-j" . org-journal-new-entry))

;; Evil Org
(leaf evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :setq (evil-want-C-i-jump . nil))

;; PDF Tools
(leaf pdf-tools
  :ensure t
  :hook ((doc-view-mode-hook . (lambda () (require 'pdf-tools))))
  :config
  (pdf-tools-install)
  :setq-default
  (pdf-view-display-size . 'fit-width))

;; Calibre integration
(leaf calibredb
  :ensure t
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(
								  ("~/Calibre Library")
								  )))

;; read epub files
(leaf nov
  :ensure t)

;; https://depp.brause.cc/nov.el/
;; (use-package nov-xwidget
;;   :ensure t
;;   :demand t
;;   :after nov
;;   :config
;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

;; Org-Noter
(leaf org-noter :ensure t)
(leaf org-noter-pdftools :ensure t)

(leaf org-ehtml
  :require t
  :ensure t
  :setq
  (org-ehtml-docroot . '(expand-file-name "~/org/roam"))
  (org-ehtml-everything-editable . t))

(defun start-ehtml ()
  (interactive)
  (ws-start org-ethml-handler 8888))

;; Org-Roam
(leaf org-roam
  :disabled t
  :ensure t
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
  (require 'org-roam-protocol))

;; Disable Warning About Org-Roamv2
(setq org-roam-v2-ack t)

(leaf org-roam-ui
  :ensure t
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
  :ensure t)

(leaf org-context :ensure t)
(leaf org-hyperscheduler :ensure t)
(leaf org-custom-cookies :ensure t)
(leaf org-wild-notifier :ensure t)
(leaf org-transclusion :ensure t)
(leaf org-time-budgets :ensure t)
(leaf org-ref-prettify :ensure t)
(leaf org-drill-table :ensure t)
(leaf org-auto-tangle :ensure t)
(leaf org-tree-slide :ensure t)
(leaf org-tanglesync :ensure t)
(leaf org-randomnote :ensure t)
(leaf org-projectile :ensure t)
(leaf org-inline-pdf :ensure t)
(leaf citar-org-roam :ensure t)
(leaf org-web-tools :ensure t)
(leaf org-treeusage :ensure t)
(leaf org-rich-yank :ensure t)
(leaf org-review :ensure t)

(leaf org-re-reveal
  :ensure t
  :setq
  (org-re-reveal-root . "~/src/reveal.js")
  (org-re-reveal-subtree-with-title-slide . t)
  :config
  (add-to-list 'org-structure-template-alist '("R" . "#+REVEAL_HTML: ?\n")))


(leaf org-pdftools :ensure t)
(leaf org-mind-map :ensure t)
(leaf org-make-toc :ensure t)

(leaf org-download
  :ensure t
  :hook ((Dir . org-download-enable)
		 (org-mode . org-download-enable)))

(leaf org-contacts :ensure t)

(leaf org-tagged
  :ensure t
  :emacs>= 28.1)

(leaf org-kanban :ensure t)
(leaf org-appear :ensure t)
(leaf org-emms :ensure t)
(leaf org-evil :after evil :ensure t)
(leaf org-edna :ensure t)
(leaf org-ref :ensure t)
(leaf org-msg :ensure t)
(leaf org-gtd :ensure t)
(leaf org-wc :ensure t)
(leaf org-if :ensure t)
(leaf org-fancy-priorities :ensure t)

(leaf org-modern
  :ensure t
  :init
  (global-org-modern-mode))

(leaf org-present
  :ensure t)

(leaf cdlatex
  :ensure t
  :hook ((org-mode . turn-on-org-cd-latex)))

(leaf org-mime :ensure t)

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

(leaf pandoc :ensure t)
(leaf pandoc-mode :ensure t)
(leaf ox-pandoc :ensure t)
(leaf copyit-pandoc :ensure t)

(leaf gnuplot :ensure t)
(leaf htmlize :ensure t)

(leaf org-novelist
  :quelpa (org-novelist :fetcher github :repo "sympodius/org-novelist")
  :setq
  (org-novelist-language-tag ."en-US")
  (org-novelist-author . user-full-name)
  (org-novelist-author-email . user-mail-address))

(leaf ankifier
  :after (expand-region)
  :quelpa (ankifier :fetcher github :repo "adham-omran/ankifier")
  :setq
  (ankifier-insert-elsewhere . t)
  (ankifier-anki-basic-note-type . "E-Basic")
  (ankifier-anki-cloze-note-type . "E-Cloze")
  (ankifier-feedback . t)
  (ankifier-context-question . t))

(leaf org-pretty-tags
  :ensure t
  :init
  (org-pretty-tags-global-mode 1))

(leaf org-unique-id
  :ensure t
  :require t
  :after (org)
  :hook ((before-save-hook . org-unique-id)))

(leaf ox-haunt
  :ensure t)

(leaf hyperbole
  :disabled t
  :ensure t
  :config
  (hyperbole-mode 1))

(leaf org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
				  :files (:defaults (:exclude "helm-org-ql.el"))))

(leaf org-sticky-header
  :ensure t)

(leaf org-bookmark-heading
  :ensure t)

(leaf hammy
  :quelpa (hammy :fetcher github :repo "alphapapa/hammy.el"))

(leaf weblorg
  :ensure t
  :require t)

(leaf denote
  :ensure t
  :require t
  :init
  (denote-rename-buffer-mode 1)
  :bind
  ("C-c n i" . denote-link-or-create)
  ("C-c n c" . denote-open-or-create)
  ("C-c n j" . denote-journal-extras-new-or-existing-entry)
  ("C-c C-j" . denote-journal-extras-new-or-existing-entry)
  :custom
  (denote-directory . "~/Documents/notes/")
  (denote-save-buffer-after-creation . nil)
  (denote-known-keywords . '("emacs"
							 "philosophy"
							 "politics"
							 "economics"
							 "astrology"
							 "journal"
							 "books"))
  (denote-infer-keywords . t)
  (denote-prompts . '(title keywords template))
  (denote-templates . nil)
  (denote-backlinks-show-context . t)
  (denote-org-capture-specifiers . "%?")
  (denote-date-prompt-use-org-read-date . t)
  (denote-journal-extras-title-format . 'day-date-month-year)
  :hook
  (dired-mode-hook . denote-dired-mode)
  (after-save-hook . k/denote-always-rename-on-save)
  :config
  (require 'denote-journal-extras)
  (defun k/publish-denote ()
	(interactive)
	(mapc (lambda (file) (org-ehtml-export-file file))
		  (seq-filter (apply-partially #'string-match-p "_programming")
					  (denote-directory-files)))
	(shell-command (concat "mv "
						   (concat (denote-directory) "*.html ")
						   (concat (denote-directory) "blog/posts/"))))
  (defun k/denote-always-rename-on-save ()
	"Rename the current Denote file upon saving the file.
    Add this to `after-save-hook'."
	(let ((denote-rename-confirmations nil)
          (denote-save-buffers t)) ; to save again post-rename
      (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
		(ignore-errors (denote-rename-file-using-front-matter buffer-file-name))))))

(leaf denote-menu
  :after (denote)
  :ensure t)

(leaf denote-refs
  :after (denote)
  :ensure t)

(leaf citar-denote
  :after (denote citar)
  :ensure t)

(leaf denote-explore
  :after (denote)
  :ensure t)

(leaf consult-denote
  :after (denote consult)
  :ensure t)

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
  :quelpa (corg :fetcher github :repo "isamert/corg.el"))

(leaf org-media-note
  :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
  :config
  (setq org-media-note-screenshot-image-dir "~/Document/notes/imgs/'"))

(leaf org-latest-git-commits
  :config
  (defun org-dblock-write:github-commits (params)
    (let* ((user (or (plist-get params :user) "TON-UTILISATEUR"))
           (repo (or (plist-get params :repo) "TON-DEPOT"))
           (num (or (plist-get params :n) 5))
           (url (format "https://api.github.com/repos/%s/%s/commits?per_page=%d" user repo num))
           (json (with-temp-buffer
                   (url-insert-file-contents url)
                   (json-parse-buffer :array-type 'list))))
      (insert "* Latest commits\n")
      (dolist (commit json)
        (let ((sha (substring (gethash "sha" commit) 0 7))
              (message (gethash "message" (gethash "commit" commit))))
          (insert (format "- [[https://github.com/%s/%s/commit/%s][%s]] - %s\n"
                          user repo sha sha message)))))))

(provide 'k-org)
