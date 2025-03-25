;; font
;; (set-frame-parameter (selected-frame) 'font "JetBrains Mono Bold-8")

;; (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)

;; theme packages
(leaf kaolin-themes :elpaca t)
(leaf doom-themes :elpaca t)
(leaf ef-themes
  :elpaca t
  :config
  (setq ef-bio-palette-overrides '((variable fg-main)
								   (string green-faint)))
  (setq ef-autumn-palette-overrides '((variable fg-main)
									  (bg-main bg-dim)))
  (setq ef-cyprus-palette-overrides '((variable fg-main)
									  (bg-main bg-dim)
									  (string green-faint))))
(leaf yabaki-theme :elpaca t)
(leaf haki-theme :elpaca t)
(leaf sweet-theme :elpaca t)

(leaf material-ocean
  :elpaca (material-ocean-dark-theme :host github :repo "Patrick-Poitras/emacs-material-ocean"
									 ))

;; Solaire
(leaf solaire-mode
  :disabled t
  :elpaca t
  :config
  (solaire-global-mode 1))

;; Diminish
(leaf diminish
  :elpaca t)

(leaf all-the-icons
  :elpaca t
  :config
  (leaf all-the-icons-ibuffer
	:elpaca t)
  (leaf all-the-icons-completion
	:elpaca t))

;; (leaf nerd-icons
;;   :elpaca t
;;   :require t
;;   :custom
;;   (nerd-icons-font-family . "JetbrainsMono Nerd Font"))

;; (leaf all-the-icons-nerd-fonts
;;   :require t
;;   :elpaca
;;   (all-the-icons-nerd-fonts :host github :repo "mohkale/all-the-icons-nerd-fonts")
;;   :require t
;;   :after (all-the-icons)
;;   :config
;;   (all-the-icons-nerd-fonts-prefer))

;; (leaf nerd-fonts
;;   :require t
;;   :elpaca
;;   (nerd-fonts :host github :repo "twlz0ne/nerd-fonts.el"))

;; (leaf lambda-line
;;   :elpaca (lambda-line :host github :repo "lambda-emacs/lambda-line") 
;;   :custom
;;   (lambda-line-icon-time . t) ;; requires ClockFace font (see below)
;;   (lambda-line-clockface-update-fontset . "ClockFaceRect") ;; set clock icon
;;   (lambda-line-position . 'top) ;; Set position of status-line 
;;   (lambda-line-abbrev . t) ;; abbreviate major modes
;;   (lambda-line-hspace . "  ")  ;; add some cushion
;;   (lambda-line-prefix . t) ;; use a prefix symbol
;;   (lambda-line-prefix-padding . nil) ;; no extra space for prefix 
;;   (lambda-line-status-invert . nil)  ;; no invert colors
;;   (lambda-line-gui-ro-symbol .  " ⨂") ;; symbols
;;   (lambda-line-gui-mod-symbol . " ⬤") 
;;   (lambda-line-gui-rw-symbol .  " ◯") 
;;   (lambda-line-space-top . +.50)  ;; padding on top and bottom of line
;;   (lambda-line-space-bottom . -.50)
;;   (lambda-line-symbol-position . 0.1) ;; adjust the vertical placement of symbol
;;   :config
;;   ;; activate lambda-line 
;;   (lambda-line-mode) 
;;   ;; set divider line in footer
;;   (when (eq lambda-line-position 'top)
;; 	(setq-default mode-line-format (list "%_"))
;; 	(setq mode-line-format (list "%_"))))

;; (customize-set-variable 'flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
;; (customize-set-variable 'flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))

;;(use-package fontset
;;			 :straight (:type built-in) ;; only include this if you use straight
;;			 :config
;;			 ;; Use symbola for proper unicode
;;			 (when (member "Symbola" (font-family-list))
;;			   (set-fontset-font
;;				t 'symbol "Symbola" nil)))

(leaf highlight-numbers
  :elpaca t
  :config
  (highlight-numbers-mode 1))

(leaf highlight-quoted
  :elpaca t
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(leaf highlight-defined
  :elpaca t
  :hook
  (emacs-lisp-mode-hook . highlight-defined-mode))

(leaf hl-todo
  :elpaca t
  :custom
  (hl-todo-color-background . t)
  (hl-todo-highlight-punctuation . ":")
  (hl-todo-keyword-faces . `(("TODO" warning bold)
							 ("FIXME" error bold)
							 ("HACK" font-lock-constant-face bold)
							 ("REVIEW" font-lock-keyword-face bold)
							 ("NOTE" success bold)
							 ("DECPRECATED" font-lock-doc-face bold)))
  :config
  (global-hl-todo-mode 1))

(leaf fontaine
  :elpaca t)

(leaf cursory
  :elpaca t
  :config
  (cursory-mode 1)
  (cursory-set-preset 'underscore-thin-other-window))

(leaf lin
  :elpaca t
  :custom
  (lin-face . "lin-green"))

(set-face-background 'default nil)
(set-face-attribute 'default nil)

(leaf doom-modeline
  :elpaca t
  :config
  ;; slows down emacs
  ;;(doom-modeline-mode 1)
  )

(leaf visual-line
  :hook ((visual-line-mode . menu-bar--display-line-numbers-mode-visual)
		 (text-mode . visual-line-mode)
		 (prog-mode . visual-line-mode)
		 (org-mode . visual-line-mode))
  :config
  (global-visual-line-mode 1))

(leaf ultra-scroll
  :elpaca (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :setq
  (scroll-conservatively . 101)
  (scroll-margin . 0)
  :config
  (ultra-scroll-mode 1))

(leaf touchpad
  :disabled nil
  :elpaca (touchpad :host github :repo "awu7/touchpad-scroll-mode")
  :config
  (load "touchpad")
  (touchpad-scroll-mode)
  (setq touchpad-scroll-speed 3)
  (setq touchpad-pixel-scroll t)
  (setq touchpad--ultra-scroll t))


(leaf pretty-symbols
  :elpaca t
  :config
  (defvar double-struck-letters
	'(("|A|" . ?𝔸)
      ("|B|" . ?𝔹)
      ("|C|" . ?ℂ)
      ("|D|" . ?𝔻)
      ("|E|" . ?𝔼)
      ("|F|" . ?𝔽)
      ("|G|" . ?𝔾)
      ("|H|" . ?ℍ)
      ("|I|" . ?𝕀)
      ("|J|" . ?𝕁)
      ("|K|" . ?𝕂)
      ("|L|" . ?𝕃)
      ("|M|" . ?𝕄)
      ("|N|" . ?ℕ)
      ("|O|" . ?𝕆)
      ("|P|" . ?ℙ)
      ("|Q|" . ?ℚ)
      ("|R|" . ?ℝ)
      ("|S|" . ?𝕊)
      ("|T|" . ?𝕋)
      ("|U|" . ?𝕌)
      ("|V|" . ?𝕍)
      ("|W|" . ?𝕎)
      ("|X|" . ?𝕏)
      ("|Y|" . ?𝕐)
      ("|Z|" . ?ℤ)
      ("|gamma|" . ?ℽ)
      ("|Gamma|" . ?ℾ)
      ("|pi|" . ?ℼ)
      ("|Pi|" . ?ℿ)))

  (defvar arrows
	'(("->" . ?→)
      ("-->" . ?⟶)
      ("<-" . ?←)
      ("<--" . ?⟵)
      ("<->" . ?↔)
      ("<-->" . ?⟷)
      ("=>" . ?⇒)
      ("==>" . ?⟹)
      ("<==" . ?⟸)
      ("<=>" . ?⇔)
      ("<==>" . ?⟺)
      ("|->" . ?↦)
      ("|-->" . ?⟼)
      ("<-|" . ?↤)
      ("<--|" . ?⟻)
      ("|=>" . ?⤇)
      ("|==>" . ?⟾)
      ("<=|" . ?⤆)
      ("<==|" . ?⟽)
      ("~>" . ?⇝)
      ("<~" . ?⇜)
      (">->" . ?↣)
      ("<-<" . ?↢)
      ("->>" . ?↠)
      ("<<-" . ?↞)
      (">->>" . ?⤖)
      ("<<-<" . ?⬻)
      ("<|-" . ?⇽)
      ("-|>" . ?⇾)
      ("<|-|>" . ?⇿)
      ("<-/-" . ?↚)
      ("-/->" . ?↛)
      ("<-|-" . ?⇷)
      ("-|->" . ?⇸)
      ("<-|->" . ?⇹)
      ("<-||-" . ?⇺)
      ("-||->" . ?⇻)
      ("<-||->" . ?⇼)
      ("-o->" . ?⇴)
      ("<-o-" . ?⬰)))

  (defvar mathematical-symbols
	'(("forall" . ?∀)
      ("exists" . ?∃)
      ("not" . ?¬)
      ("&&" . ?∧)
      ("||" . ?∨)
      ("==" . ?≡)
      ("/=" . ?≠)
      ("!=" . ?≠)
      ("<=" . ?≤)
      (">=" . ?≥)
      ("/<" . ?≮)
      ("/>" . ?≯)
      ("++" . ?⧺)
      ("+++" . ?⧻)
      ("|||" . ?⫴)
      ("empty" . ?∅)
      ("elem" . ?∈)
      ("notElem" . ?∉)
      ("member" . ?∈)
      ("notMember" . ?∉)
      ("union" . ?∪)
      ("intersection" . ?∩)
      ("subsetOf" . ?⊆)
      ("properSubsetOf" . ?⊂)
      ("<<" . ?≪)
      (">>" . ?≫)
      ("<<<" . ?⋘)
      (">>>" . ?⋙)
      ("<|" . ?⊲)
      ("|>" . ?⊳)
      ("><" . ?⋈)
      (":=" . ?≔)
      ("=:" . ?≕)
      ("<+>" . ?⊕)
      ("<*>" . ?⊛)))

  (defvar greek-letters
	'(("alpha" . ?α)
      ("beta" . ?β)
      ("gamma" . ?γ)
      ("delta" . ?δ)
      ("epsilon" . ?ε)
      ("zeta" . ?ζ)
      ("eta" . ?η)
      ("theta" . ?θ)
      ("iota" . ?ι)
      ("kappa" . ?κ)
      ("lambda" . ?λ)
      ("mu" . ?μ)
      ("nu" . ?ν)
      ("xi" . ?ξ)
      ("omicron" . ?ο)
      ("pi" . ?π)
      ("rho" . ?ρ)
      ("sigma_final" . ?ς)
      ("sigma" . ?σ)
      ("tau" . ?τ)
      ("upsilon" . ?υ)
      ("phi" . ?φ)
      ("chi" . ?χ)
      ("psi" . ?ψ)
      ("omega" . ?ω)
      ("Sigma" . ?Σ)))

  (defun add-pretty-symbols-list (symbols)
	"Add a list of prettified symbol-pairs to the buffer-local prettify-symbols-alist"
	(setq-local prettify-symbols-alist (append prettify-symbols-alist
                                               symbols)))
  (defun add-pretty-symbols-hook (mode-hook symbols)
	"Add a list of prettified symbol-pairs to a given mode."
	(add-hook mode-hook `(lambda ()
                           (add-pretty-symbols-list (quote ,symbols)))))

  (global-prettify-symbols-mode)
  :hook ((prog-mode . (lambda () (add-pretty-symbols-list (append
														   arrows
														   greek-letters
														   mathematical-symbols
														   double-struck-letters))))))

(leaf page-break-lines
  :elpaca t
  :config
  (page-break-lines-mode 1))

(leaf breadcrumb
  :elpaca t
  :config
  (breadcrumb-mode 1))

(leaf mixed-pitch
  :elpaca t
  :hook
  (org-mode . mixed-pitch-mode))

(load-theme 'doom-tokyo-night t)
(set-background-color "#0f111a")
(set-foreground-color "lightblue")
(set-face-foreground 'font-lock-comment-face "dark cyan")
(global-hl-line-mode 1)

(provide 'k-themes)
