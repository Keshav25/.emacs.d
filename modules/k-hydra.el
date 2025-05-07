(leaf hydra
  :require t
  :elpaca t
  :config
  (leaf major-mode-hydra
	:elpaca t
	:bind
	("M-SPC" . major-mode-hydra)
	("C-S-o" . k-window-movement/body)
	:config
	(major-mode-hydra-define emacs-lisp-mode
	  (:quit-key "q")
	  ("Eval"
	   (("b" eval-buffer "buffer")
		("e" eval-defun "defun")
		("r" eval-region "region"))
	   "REPL"
	   (("I" ielm "ielm"))
	   "Test"
	   (("t" ert "prompt")
		("T" (ert t) "all")
		("F" (ert :failed) "failed"))
	   "Doc"
	   (("d" describe-foo-at-point "thing-at-pt")
		("f" describe-function "function")
		("v" describe-variable "variable")
		("i" info-lookup-symbol "info lookup")))))
  (pretty-hydra-define k-window-movement
	(:color pink :quit-key "q" :title "Window Management")
	("Navigation"
	 (("b" windmove-left "Move to Left Window")
	  ("f" windmove-right "Move to Right Window")
	  ("n" windmove-down "Move Down a Window")
	  ("p" windmove-up "Move Up a Window")
	  ("M" get-mru-window "Return to Most Used Window"))
	 "Manipulation"
	 (("s" split-and-follow-vertically "Split Window Horizontally")
	  ("v" split-and-follow-horizontally "Split Window Vertically")
	  ("d" delete-window "Delete Window")
	  ("o" ace-window "Swith Window")
	  ("m" k-toggle-fullscreen "Un/Maximize a Window")
	  ("R" evil-window-rotate-upwards "Rotate Windows")
	  ("r" evil-window-rotate-downwards "Reverse Rotate Windows")
	  ("u" winner-undo "Undo Window Manipulation")
	  ("U" winner-redo "Redo Window Manipulation"))
	 "Size"
	 (("+" evil-window-increase-height "Increase Height")
	  ("-" evil-window-decrease-height "Decrease Heigth")
	  ("<" evil-window-decrease-width "Decrease Width")
	  (">" evil-window-increase-width "Increase Width")
	  ("=" balance-windows "Balance Windows")
	  (";" enlarge-window "Enlarge Window"))
	 "Buffer"
	 (("l" consult-buffer "Change Buffer")
	  ("c" centered-window-mode "Un/Center Window")
	  ("M-o" ace-window-prefix "Command in a select window"))
	 "Swap Windows"
	 (("B" windmove-swap-states-left "Move Window Left")
	  ("N" windmove-swap-states-down "Move Window Down")
	  ("P" windmove-swap-states-up "Move Window Up")
	  ("F" windmove-swap-states-right "Move Window Right"))
	 "Text"
	 (("C-=" text-scale-increase "zoom in")
	  ("C--" text-scale-decrease "zoom out")))))

(leaf main-hydra
  :config 
  (defvar navy-l 'forward-char
	"The next item in a forward sense.")

  (defvar navy-j 'backward-char
	"The previous item in a backward sense.")

  (defvar navy-i 'previous-line
	"The previous item in an up sense.")

  (defvar navy-k 'next-line
	"The next item in a down sense.")

  (defvar navy-semicolon 'avy-goto-char
	"Command bound to ;.")

  (defvar navy-quote 'avy-goto-line
	"Command bound to '.")

  (defvar navy-comma 'avy-goto-char-2
	"Command bound to ,")

  (defvar navy-period 'avy-goto-word-0
	"Command bound to .")

  (defvar navy-slash 'end-of-visual-line
	"The end of an item.")

  (defvar navy-h 'beginning-of-visual-line
	"Command bound to h, usually a beginning of command.")

  (defvar navy-mode "char"
	"The active mode.")


  (defhydra navy (:color red :hint nil)
	"
%s(format \"%s-mode\" navy-mode)
%s(make-string (length (symbol-name navy-j)) ? )     _i_: %`navy-i
%`navy-j :_j_     _l_: %`navy-l     _;_: %`navy-semicolon  _'_: %`navy-quote
%s(make-string (length (symbol-name navy-j)) ? )     _k_: %`navy-k
  _,_: %`navy-comma _._: %`navy-period _/_: %`navy-slash
  point-min: _<_    _>_: point-max

"
	("j" (funcall navy-j))
	("l" (funcall navy-l))
	("i" (funcall navy-i))
	("k" (funcall navy-k))

	("q" nil "quit" :color blue)

	("h" (call-interactively navy-h))

	(";" (call-interactively navy-semicolon))
	("'" (call-interactively navy-quote))

	("," (call-interactively navy-comma))
	("." (call-interactively navy-period))
	("/" (call-interactively navy-slash))

	("<" beginning-of-buffer)
	(">" end-of-buffer)
	;; these are different modes
	;; char

	("c" (lambda ()
		   (interactive)
		   (setq navy-mode "char"
				 navy-j 'backward-char
				 navy-i 'previous-line
				 navy-l 'forward-char
				 navy-k 'next-line
				 navy-semicolon 'avy-goto-char-2
				 navy-quote 'avy-goto-line
				 navy-comma 'avy-goto-char-in-line
				 navy-period 'avy-goto-word-1))
	 "char mode")

	("w" (lambda ()
		   (interactive)
		   (setq navy-mode "word"
				 navy-j 'backward-word
				 navy-i 'previous-line
				 navy-l 'forward-word
				 navy-k 'next-
				 navy-semicolon 'avy-goto-char-2
				 navy-quote 'avy-goto-line
				 navy-comma 'avy-goto-word-1
				 navy-period 'avy-goto-word-or-subword-1))
	 "word mode")

	("s" (lambda ()
		   (interactive)
		   (setq navy-mode "sentence"
				 navy-j 'backward-sentence
				 navy-i 'previous-line
				 navy-k 'next-line
				 navy-l 'forward-sentence
				 navy-semicolon 'avy-goto-char-2
				 navy-quote 'avy-goto-line
				 navy-comma 'avy-goto-word-1
				 navy-period 'avy-goto-word-or-subword-1))
	 "sentence mode")

	("p" (lambda ()
		   (interactive)
		   (setq navy-mode "paragraph"
				 navy-j 'backward-paragraph
				 navy-l 'forward-paragraph
				 navy-i 'previous-line
				 navy-k 'next-line
				 navy-semicolon 'avy-goto-char-2
				 navy-quote 'avy-goto-line
				 navy-comma 'avy-goto-word-1
				 navy-period 'avy-goto-word-or-subword-1))
	 "paragraph mode")

	("g" (lambda ()
		   (interactive)
		   (setq navy-mode "page"
				 navy-j 'backward-page
				 navy-l 'forward-page
				 navy-i 'backward-page
				 navy-k 'forward-page
				 navy-semicolon 'avy-goto-char-2
				 navy-quote 'avy-goto-line
				 navy-comma 'avy-goto-word-1
				 navy-period 'avy-goto-word-or-subword-1))
	 "page mode")

	("n" (lambda ()
		   (interactive)
		   (setq navy-mode "line"
				 navy-i 'avy-goto-line-above
				 navy-k 'avy-goto-line-below
				 navy-l 'next-line
				 navy-j 'previous-line
				 navy-semicolon 'avy-goto-char-2
				 navy-quote 'avy-goto-line
				 navy-comma 'avy-goto-word-1
				 navy-period 'avy-goto-word-or-subword-1))
	 "line mode")

	("x" (lambda ()
		   (interactive)
		   (setq navy-mode "sexp"
				 navy-j 'backward-sexp
				 navy-l 'forward-sexp
				 navy-i 'previous-line
				 navy-k 'next-line
				 navy-semicolon 'avy-goto-char-2
				 navy-quote 'avy-goto-line
				 navy-comma 'lispy-ace-symbol
				 navy-period 'lispy-ace-paren))
	 "sexp mode")

	("a" swiper-all "swiper-all")
	("r" counsel-git-grep "git grep")
	("t" avy-goto-char-timer "char timer"))


  (defun navy ()
	"Run the `navy/body' hydra."
	(interactive)
	(setq navy-mode "char"
		  navy-j 'backward-char
		  navy-i 'previous-line
		  navy-l 'forward-char
		  navy-k 'next-line
		  navy-quote 'avy-goto-line
		  navy-comma 'avy-goto-char-2
		  navy-period 'avy-goto-char-in-line
		  navy-h 'beginning-of-visual-line
		  navy-semicolon 'avy-goto-char)
	(navy/body))
  )

(leaf elfeed-hydra
  :after (hydra elfeed)
  :config
  (defvar elfeed-search-filter)

  (cl-defmacro unpackaged/elfeed-search-view-hydra-define (name body views)
	"Define a pretty hydra named NAME with BODY and VIEWS.
VIEWS is a plist: in it, each property is a string which becomes
a column header in the hydra, and each value is a list of lists
in this format: (KEY COMPONENT &optional LABEL).

The KEY is a key sequence passed to `kbd', like \"s\" or \"S
TAB\".  The COMPONENT is an Elfeed filter component, which may
begin with \"+\" or \"=\", and in which spaces are automatically
escaped as required by Elfeed.  The LABEL, if present, is a
string displayed next to the KEY; if absent, COMPONENT is
displayed.

In the resulting hydra, when KEY is pressed, the COMPONENT is
toggled in `elfeed-search-filter'.  It is toggled between three
states: normal, inverse, and absent.  For example, the component
\"+tag\" cycles between three states in the filter: \"+tag\",
\"-tag\", and \"\".  The appropriate inverse prefix is used
according to the component's prefix (i.e. for \"=\", the inverse
is \"~\", and for \"\" (a plain regexp), \"!\" is used).

These special components may be used to read choices from the
Elfeed database with completion and toggle them:

  :complete-age   Completes and sets the age token.
  :complete-feed  Completes and toggles a feed token.
  :complete-tag   Completes and toggles a tag token.
  nil             Sets default filter.

A complete example:

  (unpackaged/elfeed-search-view-hydra-define my/elfeed-search-view-hydra
    (:foreign-keys warn)
    (\"Views\"
     ((\"@\" :complete-age \"Date\")
      (\"d\" nil))
     \"Status\"
     ((\"su\" \"+unread\"))
     \"Feed\"
     ((\"f TAB\" :complete-feed \"Choose\")
      (\"fE\" \"=Planet Emacslife\" \"Planet Emacslife\"))
     \"Tags\"
     ((\"t TAB\" :complete-tag \"Choose\")
      (\"te\" \"+Emacs\"))
     \"\"
     ((\"tn\" \"+news\"))))"
	(declare (indent defun))
	(cl-labels ((escape-spaces (string)
                  ;; Return STRING with spaces escaped with "\s-".  Necessary
                  ;; because Elfeed treats all literal spaces as separating tokens.
                  (replace-regexp-in-string (rx space) "\\s-" string t t)))
      (let* ((completion-fns
              (list (cons :complete-age
                          (lambda ()
							(interactive)
							(save-match-data
                              (let* ((date-regexp (rx (group (or bos blank) "@" (1+ digit) (1+ (not blank)))))
									 (date-tag (when (string-match date-regexp elfeed-search-filter)
												 (match-string 1 elfeed-search-filter))))
								(elfeed-search-set-filter
								 (replace-regexp-in-string date-regexp (read-string "Date: " date-tag)
                                                           elfeed-search-filter t t))))))
					(cons :complete-feed
                          '(concat "=" (replace-regexp-in-string
										(rx space) "\\s-"
										(->> (hash-table-values elfeed-db-feeds)
											 (--map (elfeed-meta it :title))
											 (completing-read "Feed: ")
											 regexp-quote) t t)))
					(cons :complete-tag
                          '(concat "+" (completing-read "Tag: " (elfeed-db-get-all-tags))))))
			 (body (append '(:title elfeed-search-filter :color pink :hint t :quit-key "q")
                           body))
			 (heads (cl-loop for (heading views) on views by #'cddr
							 collect heading
							 collect (cl-loop for (key component label) in views
                                              collect
                                              `(,key
												,(cl-typecase component
                                                   ((and function (not null))
													;; I don't understand why nil matches
													;; (or lambda function), but it does,
													;; so we have to account for it.  See
													;; (info-lookup-symbol 'cl-typep).
													`(funcall ,component))
                                                   (string
													`(elfeed-search-set-filter
                                                      (unpackaged/elfeed-search-filter-toggle-component
                                                       elfeed-search-filter ,(escape-spaces component))))
                                                   (otherwise
													`(elfeed-search-set-filter
                                                      ,(when component
														 `(unpackaged/elfeed-search-filter-toggle-component
                                                           elfeed-search-filter ,component)))))
												,(or label component "Default"))))))
		;; I am so glad I discovered `cl-sublis'.  I tried several variations of `cl-labels' and
		;; `cl-macrolet' and `cl-symbol-macrolet', but this is the only way that has worked.
		(setf heads (cl-sublis completion-fns heads))
		`(pretty-hydra-define ,name ,body
           ,heads))))

  (cl-defun unpackaged/elfeed-search-filter-toggle-component (string component)
	"Return STRING (which should be `elfeed-search-filter') having toggled COMPONENT.
Tries to intelligently handle components based on their prefix:
+tag, =feed, regexp."
	(save-match-data
      (cl-labels ((toggle (component +prefix -prefix string)
					(let ((+pat (rx-to-string `(seq (or bos blank)
													(group ,+prefix ,component)
													(or eos blank))))
                          (-pat (rx-to-string `(seq (group (or bos (1+ blank)) ,-prefix ,component)
													(or eos blank)))))
                      ;; TODO: In newer Emacs versions, the `rx' pattern `literal'
                      ;; evaluates at runtime in `pcase' expressions.
                      (pcase string
						((pred (string-match +pat)) (rm (concat -prefix component) string))
						((pred (string-match -pat)) (rm "" string))
						(_ (concat string " " +prefix component)))))
                  (rm (new string) (replace-match new t t string 1)))
		(pcase component
          ((rx bos "+" (group (1+ anything)))
           (toggle (match-string 1 component) "+" "-" string))
          ((rx bos "=" (group (1+ anything)))
           (toggle (match-string 1 component) "=" "~" string))
          (_ (toggle component "" "!" string))))))

  (unpackaged/elfeed-search-view-hydra-define my/elfeed-search-view-hydra
	(:foreign-keys warn)
	("Views"
	 (("@" :complete-age "Date")
      ("d" nil))
	 "Status"
	 (("su" "+unread"))
	 "Feed"
	 (("f TAB" :complete-feed "Choose")
      ("fE" "=Planet Emacslife" "Planet Emacslife"))
	 "Tags"
	 (("t TAB" :complete-tag "Choose")
      ("te" "+Emacs"))
	 ""
	 (("tn" "+news")))))


(defhydra hydra-vi (:pre (progn (set-cursor-color "#40e0d0")
								(cursory-set-preset "block"))
						 :post (progn
								 (set-cursor-color "#ffffff")
								 (cursory-set-preset "underscore-thin-other-window")
								 (message
								  "Thank you, come again.")))
  "vi"
  ("a" forward-char)
  ("p" backward-char)
  ("n" next-line)
  ("e" previous-line)
  ("I" end-of-line :color blue)
  ("q" nil "quit"))

;; (define-prefix-command 'endless/toggle-map)
;; ;; The manual recommends C-c for user keys, but C-x t is
;; ;; always free, whereas C-c t is used by some modes.
;; (define-key ctl-x-map "t" 'endless/toggle-map)
;; (define-key endless/toggle-map "c" #'column-number-mode)
;; (define-key endless/toggle-map "d" #'toggle-debug-on-error)
;; (define-key endless/toggle-map "e" #'toggle-debug-on-error)
;; (define-key endless/toggle-map "f" #'auto-fill-mode)
;; (define-key endless/toggle-map "l" #'toggle-truncate-lines)
;; (define-key endless/toggle-map "q" #'toggle-debug-on-quit)
;; (define-key endless/toggle-map "t" #'endless/toggle-theme)
;; ;;; Generalized version of `read-only-mode'.
;; (define-key endless/toggle-map "r" #'dired-toggle-read-only)
;; (autoload 'dired-toggle-read-only "dired" nil t)
;; (define-key endless/toggle-map "w" #'whitespace-mode)

(provide 'k-hydra)

