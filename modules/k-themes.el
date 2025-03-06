;; font
;; (set-frame-parameter (selected-frame) 'font "JetBrains Mono Bold-8")

;; (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)

(leaf theme
  :init
  ;; theme packages
  (leaf kaolin-themes :ensure t)
  (leaf doom-themes :ensure t)
  (leaf ef-themes
	:ensure t
	:config
	(setq ef-bio-palette-overrides '((variable fg-main)
									 (string green-faint)))
	(setq ef-autumn-palette-overrides '((variable fg-main)
										(bg-main bg-dim)))
	(setq ef-cyprus-palette-overrides '((variable fg-main)
										(bg-main bg-dim)
										(string green-faint)))
	)
  (leaf yabaki-theme :ensure t)
  (leaf haki-theme :ensure t)
  (leaf sweet-theme :ensure t)
  :config
  ;; load the actual theme
  (load-theme 'ef-dark t)
  (set-foreground-color "lightblue")
  (global-hl-line-mode 1))

;; ;; Option 1: Simply do (make sure it is in 'custom-theme-directory')
;; (setq haki-region "#2e8b6d")
;; (load-theme 'haki t)

;; ;; Option 2: with 'use-package'
;; (use-package haki-theme
;; 			 :config
;; 			 (setq haki-region "#2e8b6d"
;; 				   ;; If you skip setting this, it will use 'default' font.
;; 				   haki-heading-font "Comic Mono"
;; 				   haki-sans-font "Iosevka Comfy Motion"
;; 				   haki-title-font "Impress BT"
;; 				   haki-link-font "VictorMono Nerd Font" ;; or Maple Mono looks good
;; 				   haki-code-font "Maple Mono") ;; inline code/verbatim (org,markdown..)

;; 			 ;; For meow/evil users (change border of mode-line according to modal states)
;; 			 (add-hook 'post-command-hook #'haki-modal-mode-line)

;; 			 (load-theme 'haki t))

;; Solaire
(leaf solaire-mode
  :disabled t
  :ensure t
  :config
  (solaire-global-mode 1))

;; Diminish
(leaf diminish
  :ensure t)

(leaf all-the-icons
  :ensure t
  :config
  (leaf all-the-icons-ibuffer
	:ensure t)
  (leaf all-the-icons-completion
	:ensure t))

;; (leaf nerd-icons
;;   :ensure t
;;   :require t
;;   :custom
;;   (nerd-icons-font-family . "JetbrainsMono Nerd Font"))

;; (leaf all-the-icons-nerd-fonts
;;   :require t
;;   :quelpa
;;   (all-the-icons-nerd-fonts :fetcher github :repo "mohkale/all-the-icons-nerd-fonts")
;;   :require t
;;   :after (all-the-icons)
;;   :config
;;   (all-the-icons-nerd-fonts-prefer))

;; (leaf nerd-fonts
;;   :require t
;;   :quelpa
;;   (nerd-fonts :fetcher github :repo "twlz0ne/nerd-fonts.el"))

;; (leaf lambda-line
;;   :quelpa (lambda-line :fetcher github :repo "lambda-emacs/lambda-line") 
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
  :ensure t
  :config
  (highlight-numbers-mode 1))

(leaf highlight-quoted
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(leaf highlight-defined
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(leaf hl-todo
  :ensure t
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
  :ensure t)

(leaf cursory
  :ensure t
  :config
  (cursory-mode 1)
  (cursory-set-preset 'underscore-thin-other-window))

(leaf lin
  :ensure t
  :custom
  (lin-face . "lin-green"))

(set-face-background 'default nil)
(set-face-attribute 'default nil)

(leaf doom-modeline
  :ensure t
  :config
  ;; slows down emacs
  ;;(doom-modeline-mode 1)
  )

(leaf visual-line
  :hook ((visual-line-mode . menu-bar--display-line-numbers-mode-visual)
		 (text-mode . visual-line-mode)
		 (prog-mode . visual-line-mode))
  :config
  (global-visual-line-mode 1))

(leaf ultra-scroll
  :quelpa (ultra-scroll :fetcher github :repo "jdtsmith/ultra-scroll")
  :setq
  (scroll-conservatively . 101)
  (scroll-margin . 0)
  :config
  (ultra-scroll-mode 1))


(leaf pretty-symbols
  :ensure t
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
  :ensure t)

(provide 'k-themes)

