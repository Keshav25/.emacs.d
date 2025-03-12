(leaf hydra
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
	  ("p" windmove-up "Move Up a Window"))
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

(provide 'k-hydra)
