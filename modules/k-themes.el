;; -*- lexical-binding: t -*-

;; font
;; (set-frame-parameter (selected-frame) 'font "JetBrains Mono Bold-8")

;; (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)

;; theme packages
(leaf kaolin-themes :elpaca t)
(leaf doom-themes :elpaca t)
(leaf ef-themes
  :elpaca t
  :config
  (setopt ef-bio-palette-overrides '((variable fg-main)
									 (string green-faint)))
  (setopt ef-autumn-palette-overrides '((variable fg-main)
										(bg-main bg-dim)))
  (setopt ef-cyprus-palette-overrides '((variable fg-main)
										(bg-main bg-dim)
										(string green-faint)))
  (load-theme 'ef-winter t))

(leaf yabaki-theme :elpaca t)
(leaf haki-theme :elpaca t)
(leaf sweet-theme :elpaca t)

(leaf modus-themes
  :doc "modified to be catpuccin"
  :elpaca t
  :custom
  (modus-themes-italic-constructs . t)
  (modus-themes-bold-constructs . t)
  (modus-themes-mixed-fonts . nil)
  (modus-themes-prompts . '(bold intense))
  (modus-themes-common-palette-overrides .
										 `((accent-0 "#89b4fa")
										   (accent-1 "#89dceb")
										   (bg-active bg-main)
										   (bg-added "#364144")
										   (bg-added-refine "#4A5457")
										   (bg-changed "#3e4b6c")
										   (bg-changed-refine "#515D7B")
										   (bg-completion "#45475a")
										   (bg-completion-match-0 "#1e1e2e")
										   (bg-completion-match-1 "#1e1e2e")
										   (bg-completion-match-2 "#1e1e2e")
										   (bg-completion-match-3 "#1e1e2e")
										   (bg-hl-line "#2a2b3d")
										   (bg-hover-secondary "#585b70")
										   (bg-line-number-active unspecified)
										   (bg-line-number-inactive "#1e1e2e")
										   (bg-main "#1e1e2e")
										   (bg-mark-delete "#443245")
										   (bg-mark-select "#3e4b6c")
										   (bg-mode-line-active "#181825")
										   (bg-mode-line-inactive "#181825")
										   (bg-prominent-err "#443245")
										   (bg-prompt unspecified)
										   (bg-prose-block-contents "#313244")
										   (bg-prose-block-delimiter bg-prose-block-contents)
										   (bg-region "#585b70")
										   (bg-removed "#443245")
										   (bg-removed-refine "#574658")
										   (bg-tab-bar      "#1e1e2e")
										   (bg-tab-current  bg-main)
										   (bg-tab-other    "#1e1e2e")
										   (border-mode-line-active nil)
										   (border-mode-line-inactive nil)
										   (builtin "#89b4fa")
										   (comment "#9399b2")
										   (constant  "#f38ba8")
										   (cursor  "#f5e0dc")
										   (date-weekday "#89b4fa")
										   (date-weekend "#fab387")
										   (docstring "#a6adc8")
										   (err     "#f38ba8")
										   (fg-active fg-main)
										   (fg-completion "#cdd6f4")
										   (fg-completion-match-0 "#89b4fa")
										   (fg-completion-match-1 "#f38ba8")
										   (fg-completion-match-2 "#a6e3a1")
										   (fg-completion-match-3 "#fab387")
										   (fg-heading-0 "#f38ba8")
										   (fg-heading-1 "#fab387")
										   (fg-heading-2 "#f9e2af")
										   (fg-heading-3 "#a6e3a1")
										   (fg-heading-4 "#74c7ec")
										   (fg-line-number-active "#b4befe")
										   (fg-line-number-inactive "#7f849c")
										   (fg-link  "#89b4fa")
										   (fg-main "#cdd6f4")
										   (fg-mark-delete "#f38ba8")
										   (fg-mark-select "#89b4fa")
										   (fg-mode-line-active "#bac2de")
										   (fg-mode-line-inactive "#585b70")
										   (fg-prominent-err "#f38ba8")
										   (fg-prompt "#cba6f7")
										   (fg-prose-block-delimiter "#9399b2")
										   (fg-prose-verbatim "#a6e3a1")
										   (fg-region "#cdd6f4")
										   (fnname    "#89b4fa")
										   (fringe "#1e1e2e")
										   (identifier "#cba6f7")
										   (info    "#94e2d5")
										   (keyword   "#cba6f7")
										   (keyword "#cba6f7")
										   (name "#89b4fa")
										   (number "#fab387")
										   (property "#89b4fa")
										   (string "#a6e3a1")
										   (type      "#f9e2af")
										   (variable  "#fab387")
										   (warning "#f9e2af")))
  :config
  (modus-themes-with-colors
	(custom-set-faces
	 `(change-log-acknowledgment ((,c :foreground "#b4befe")))
	 `(change-log-date ((,c :foreground "#a6e3a1")))
	 `(change-log-name ((,c :foreground "#fab387")))
	 `(diff-context ((,c :foreground "#89b4fa")))
	 `(diff-file-header ((,c :foreground "#f5c2e7")))
	 `(diff-header ((,c :foreground "#89b4fa")))
	 `(diff-hunk-header ((,c :foreground "#fab387")))
	 `(gnus-button ((,c :foreground "#8aadf4")))
	 `(gnus-group-mail-3 ((,c :foreground "#8aadf4")))
	 `(gnus-group-mail-3-empty ((,c :foreground "#8aadf4")))
	 `(gnus-header-content ((,c :foreground "#7dc4e4")))
	 `(gnus-header-from ((,c :foreground "#cba6f7")))
	 `(gnus-header-name ((,c :foreground "#a6e3a1")))
	 `(gnus-header-subject ((,c :foreground "#8aadf4")))
	 `(log-view-message ((,c :foreground "#b4befe")))
	 `(match ((,c :background "#3e5768" :foreground "#cdd6f5")))
	 `(modus-themes-search-current ((,c :background "#f38ba8" :foreground "#11111b" ))) ;; :foreground "#cdd6f4" -- Catppuccin default, not that visible...
	 `(modus-themes-search-lazy ((,c :background "#3e5768" :foreground "#cdd6f5")))     ;; :foreground "#cdd6f4" :background "#94e2d5" -- Catppuccin default, not that visible...
	 `(newsticker-extra-face ((,c :foreground "#9399b2" :height 0.8 :slant italic)))
	 `(newsticker-feed-face ((,c :foreground "#f38ba8" :height 1.2 :weight bold)))
	 `(newsticker-treeview-face ((,c :foreground "#cdd6f4")))
	 `(newsticker-treeview-selection-face ((,c :background "#3e5768" :foreground "#cdd6f5")))
	 `(tab-bar ((,c :background "#1e1e2e" :foreground "#bac2de")))
	 `(tab-bar-tab ((,c :background "#1e1e2e" :underline t)))
	 `(tab-bar-tab-group-current ((,c :background "#1e1e2e" :foreground "#bac2de" :underline t)))
	 `(tab-bar-tab-group-inactive ((,c :background "#1e1e2e" :foreground "#9399b2"))))
	`(tab-bar-tab-inactive ((,c :background "#1e1e2e" :foreground "#a6adc8")))
	`(vc-dir-file ((,c :foreground "#89b4fa")))
	`(vc-dir-header-value ((,c :foreground "#b4befe"))))
  :init
  (load-theme 'modus-vivendi t))

(leaf automagic-dark-mode
  :elpaca (automagic-dark-mode :host github :repo "sstraust/automagic-dark-mode")
  :require t)

(leaf material-ocean
  :elpaca (material-ocean-dark-theme :host github :repo "Patrick-Poitras/emacs-material-ocean"
									 )
  :config
  ;; (load-theme 'material-ocean-dark t)
  )

;; Solaire
;; (leaf solaire-mode
;;   :disabled t
;;   :elpaca t
;;   :config
;;   (solaire-global-mode 1))

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
  (doom-modeline-mode 1)
  )

(leaf visual-line
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

(defun k-theme-load ()
  (interactive)
  (load-theme 'material-ocean t)
  (set-background-color "#0f111a")
  (set-foreground-color "lightblue")
  (set-face-foreground 'font-lock-comment-face "dark cyan")
  (global-hl-line-mode 1))

(defun k-streaming-theme ()
  (interactive)
  (load-theme 'haki t)
  (set-foreground-color "green")
  (set-face-foreground 'font-lock-comment-face "dark cyan")
  (set-frame-parameter nil 'alpha-background 30))

(provide 'k-themes)
