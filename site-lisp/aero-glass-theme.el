;;; aero-glass-theme.el --- Windows Vista Aero Glass, perfected -*- lexical-binding: t -*-

;; Author: Keshav Italia
;; Version: 1.0
;; Keywords: faces themes
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A dark theme inspired by Windows Vista's Aero Glass aesthetic—deep
;; midnight-blue tinted glass backgrounds, ice-blue accents, frosted
;; modeline panels, warm amber highlights—refined to a degree Vista
;; never achieved.  Designed for EXWM with picom transparency/blur.

;;; Code:

(deftheme aero-glass "Aero Glass — Vista perfected.")

(let (;; ── Backgrounds: layers of tinted glass ──────────
      (bg-deep      "#060a16")
      (bg-main      "#0a1020")
      (bg-alt       "#0e1830")
      (bg-panel     "#121e3a")
      (bg-highlight "#172850")
      (bg-selection "#1c3468")
      (bg-hover     "#224080")
      (bg-accent    "#283e78")

      ;; ── Glass edges ────────────────────────────────────
      (glass-edge   "#2a4488")
      (glass-bright "#3d5afe")
      (glass-frost  "#1a2d5c")
      (glass-glow   "#4070c0")

      ;; ── Foregrounds ────────────────────────────────────
      (fg-main      "#dce4f8")
      (fg-dim       "#9aa8cc")
      (fg-faint     "#6a7ca0")
      (fg-ghost     "#4a5c80")

      ;; ── Ice blue accents (signature Aero) ──────────────
      (ice           "#64b5f6")
      (ice-bright    "#90caf9")
      (ice-deep      "#42a5f5")
      (sky           "#5c9ce6")
      (azure         "#82b1ff")
      (cyan          "#26c6da")
      (teal          "#4dd0e1")
      (teal-dim      "#80cbc4")
      (electric      "#448aff")

      ;; ── Warm accents (Aero hover glow) ─────────────────
      (amber         "#ffb74d")
      (gold          "#ffd54f")
      (orange        "#ff9800")
      (peach         "#ffcc80")

      ;; ── Semantic ───────────────────────────────────────
      (green         "#69f0ae")
      (green-dim     "#4caf50")
      (green-faint   "#2e7d32")
      (red           "#ff5252")
      (red-dim       "#d32f2f")
      (red-faint     "#3a1010")
      (magenta       "#e040fb")
      (violet        "#b388ff")
      (pink          "#f48fb1")

      ;; ── Diff ───────────────────────────────────────────
      (diff-add-bg    "#0a2a18")
      (diff-add-fg    "#69f0ae")
      (diff-remove-bg "#2a0a10")
      (diff-remove-fg "#ff5252")
      (diff-change-bg "#1a1a30")
      (diff-change-fg "#82b1ff"))

  (custom-theme-set-faces
   'aero-glass

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Core faces                                     ║
   ;; ╚══════════════════════════════════════════════════╝
   `(default                          ((t (:background ,bg-main :foreground ,fg-main))))
   `(cursor                           ((t (:background ,ice))))
   `(region                           ((t (:background ,bg-selection :extend t))))
   `(highlight                        ((t (:background ,bg-highlight))))
   `(hl-line                          ((t (:background ,bg-alt :extend t))))
   `(lazy-highlight                   ((t (:background ,bg-hover :foreground ,fg-main))))
   `(secondary-selection              ((t (:background ,glass-frost))))
   `(shadow                           ((t (:foreground ,fg-faint))))
   `(link                             ((t (:foreground ,ice :underline t))))
   `(link-visited                     ((t (:foreground ,violet :underline t))))
   `(button                           ((t (:foreground ,ice :underline t))))
   `(error                            ((t (:foreground ,red :weight bold))))
   `(warning                          ((t (:foreground ,amber :weight bold))))
   `(success                          ((t (:foreground ,green :weight bold))))
   `(escape-glyph                     ((t (:foreground ,cyan))))
   `(homoglyph                        ((t (:foreground ,cyan))))
   `(match                            ((t (:background ,bg-selection :foreground ,ice-bright :weight bold))))
   `(trailing-whitespace              ((t (:background ,red-dim))))
   `(nobreak-space                    ((t (:foreground ,fg-ghost :underline t))))
   `(nobreak-hyphen                   ((t (:foreground ,fg-ghost))))
   `(widget-field                     ((t (:background ,bg-panel :box (:line-width 1 :color ,glass-edge)))))
   `(widget-button                    ((t (:foreground ,ice :weight bold))))

   ;; ── Minibuffer prompt ────────────────────────────────
   `(minibuffer-prompt                ((t (:foreground ,ice :weight bold))))

   ;; ── Line numbers ─────────────────────────────────────
   `(line-number                      ((t (:foreground ,fg-ghost :background ,bg-deep))))
   `(line-number-current-line         ((t (:foreground ,ice :background ,bg-alt :weight bold))))
   `(line-number-major-tick           ((t (:foreground ,fg-faint :background ,bg-deep :weight bold))))
   `(line-number-minor-tick           ((t (:foreground ,fg-ghost :background ,bg-deep))))

   ;; ── Fringe ───────────────────────────────────────────
   `(fringe                           ((t (:background ,bg-deep :foreground ,fg-ghost))))
   `(fill-column-indicator            ((t (:foreground ,glass-frost))))

   ;; ── Mode line: the frosted glass taskbar ─────────────
   `(mode-line                        ((t (:background ,bg-panel :foreground ,fg-main
                                          :box (:line-width 1 :color ,glass-edge :style flat-button)))))
   `(mode-line-inactive               ((t (:background ,bg-deep :foreground ,fg-faint
                                          :box (:line-width 1 :color ,bg-alt :style flat-button)))))
   `(mode-line-emphasis               ((t (:foreground ,ice :weight bold))))
   `(mode-line-highlight              ((t (:foreground ,amber))))
   `(mode-line-buffer-id              ((t (:foreground ,ice-bright :weight bold))))

   ;; ── Header line ──────────────────────────────────────
   `(header-line                      ((t (:background ,bg-panel :foreground ,fg-dim
                                          :box (:line-width 1 :color ,glass-edge)))))
   `(header-line-highlight            ((t (:foreground ,ice))))

   ;; ── Tab bar ──────────────────────────────────────────
   `(tab-bar                          ((t (:background ,bg-deep :foreground ,fg-faint))))
   `(tab-bar-tab                      ((t (:background ,bg-panel :foreground ,ice
                                          :box (:line-width 1 :color ,glass-edge)))))
   `(tab-bar-tab-inactive             ((t (:background ,bg-deep :foreground ,fg-ghost
                                          :box (:line-width 1 :color ,bg-alt)))))

   ;; ── Window dividers ──────────────────────────────────
   `(window-divider                   ((t (:foreground ,glass-frost))))
   `(window-divider-first-pixel       ((t (:foreground ,bg-deep))))
   `(window-divider-last-pixel        ((t (:foreground ,bg-deep))))
   `(internal-border                  ((t (:background ,bg-deep))))
   `(vertical-border                  ((t (:foreground ,glass-frost))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Font lock — syntax highlighting                ║
   ;; ╚══════════════════════════════════════════════════╝
   `(font-lock-keyword-face           ((t (:foreground ,azure :weight bold))))
   `(font-lock-function-name-face     ((t (:foreground ,ice))))
   `(font-lock-function-call-face     ((t (:foreground ,ice))))
   `(font-lock-variable-name-face     ((t (:foreground ,fg-main))))
   `(font-lock-variable-use-face      ((t (:foreground ,fg-main))))
   `(font-lock-string-face            ((t (:foreground ,teal-dim))))
   `(font-lock-doc-face               ((t (:foreground ,fg-faint :slant italic))))
   `(font-lock-comment-face           ((t (:foreground ,fg-ghost :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-ghost :slant italic))))
   `(font-lock-type-face              ((t (:foreground ,violet))))
   `(font-lock-constant-face          ((t (:foreground ,peach))))
   `(font-lock-builtin-face           ((t (:foreground ,teal))))
   `(font-lock-preprocessor-face      ((t (:foreground ,cyan))))
   `(font-lock-negation-char-face     ((t (:foreground ,red :weight bold))))
   `(font-lock-warning-face           ((t (:foreground ,amber :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,cyan :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,magenta :weight bold))))
   `(font-lock-number-face            ((t (:foreground ,peach))))
   `(font-lock-operator-face          ((t (:foreground ,sky))))
   `(font-lock-property-name-face     ((t (:foreground ,ice-bright))))
   `(font-lock-property-use-face      ((t (:foreground ,fg-dim))))
   `(font-lock-punctuation-face       ((t (:foreground ,fg-faint))))
   `(font-lock-bracket-face           ((t (:foreground ,fg-faint))))
   `(font-lock-delimiter-face         ((t (:foreground ,fg-faint))))
   `(font-lock-escape-face            ((t (:foreground ,cyan))))
   `(font-lock-misc-punctuation-face  ((t (:foreground ,fg-faint))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Search / replace                               ║
   ;; ╚══════════════════════════════════════════════════╝
   `(isearch                          ((t (:background ,electric :foreground ,fg-main :weight bold))))
   `(isearch-fail                     ((t (:background ,red-faint :foreground ,red))))
   `(isearch-group-1                  ((t (:background ,bg-selection :foreground ,ice-bright))))
   `(isearch-group-2                  ((t (:background ,glass-frost :foreground ,teal))))
   `(query-replace                    ((t (:background ,amber :foreground ,bg-deep :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Completions / minibuffer                       ║
   ;; ╚══════════════════════════════════════════════════╝
   `(completions-common-part          ((t (:foreground ,ice :weight bold))))
   `(completions-first-difference     ((t (:foreground ,amber :weight bold))))
   `(completions-annotations          ((t (:foreground ,fg-faint :slant italic))))
   `(completions-group-title          ((t (:foreground ,ice-bright :weight bold :underline t))))
   `(completions-group-separator      ((t (:foreground ,glass-edge :strike-through t))))
   `(completions-highlight            ((t (:background ,bg-highlight))))

   ;; ── Vertico ──────────────────────────────────────────
   `(vertico-current                  ((t (:background ,bg-highlight :extend t))))
   `(vertico-group-title              ((t (:foreground ,ice-bright :weight bold :slant italic))))
   `(vertico-group-separator          ((t (:foreground ,glass-edge :strike-through t))))
   `(vertico-multiline                ((t (:foreground ,fg-faint))))

   ;; ── Orderless ────────────────────────────────────────
   `(orderless-match-face-0           ((t (:foreground ,ice :weight bold))))
   `(orderless-match-face-1           ((t (:foreground ,amber :weight bold))))
   `(orderless-match-face-2           ((t (:foreground ,green :weight bold))))
   `(orderless-match-face-3           ((t (:foreground ,magenta :weight bold))))

   ;; ── Marginalia ───────────────────────────────────────
   `(marginalia-key                   ((t (:foreground ,ice))))
   `(marginalia-documentation         ((t (:foreground ,fg-faint :slant italic))))
   `(marginalia-file-priv-dir         ((t (:foreground ,ice))))
   `(marginalia-file-priv-read        ((t (:foreground ,green))))
   `(marginalia-file-priv-write       ((t (:foreground ,amber))))
   `(marginalia-file-priv-exec        ((t (:foreground ,red))))
   `(marginalia-file-priv-other       ((t (:foreground ,fg-ghost))))
   `(marginalia-date                  ((t (:foreground ,fg-faint))))
   `(marginalia-size                  ((t (:foreground ,fg-faint))))
   `(marginalia-number                ((t (:foreground ,peach))))
   `(marginalia-type                  ((t (:foreground ,violet))))
   `(marginalia-value                 ((t (:foreground ,teal-dim))))
   `(marginalia-modified              ((t (:foreground ,amber))))
   `(marginalia-installed             ((t (:foreground ,green))))
   `(marginalia-mode                  ((t (:foreground ,fg-faint))))

   ;; ── Consult ──────────────────────────────────────────
   `(consult-preview-line             ((t (:background ,bg-highlight :extend t))))
   `(consult-preview-match            ((t (:background ,bg-selection :foreground ,ice-bright :weight bold))))
   `(consult-preview-cursor           ((t (:background ,ice :foreground ,bg-deep))))
   `(consult-file                     ((t (:foreground ,ice))))
   `(consult-bookmark                 ((t (:foreground ,violet))))
   `(consult-buffer                   ((t (:foreground ,fg-main))))
   `(consult-line-number              ((t (:foreground ,fg-ghost))))
   `(consult-line-number-wrapped      ((t (:foreground ,fg-ghost :slant italic))))
   `(consult-grep-context             ((t (:foreground ,fg-faint))))
   `(consult-highlight-match          ((t (:foreground ,ice :weight bold))))

   ;; ── Embark ───────────────────────────────────────────
   `(embark-keybinding                ((t (:foreground ,ice :weight bold))))
   `(embark-target                    ((t (:foreground ,amber :weight bold))))
   `(embark-collect-marked            ((t (:foreground ,green :weight bold))))

   ;; ── Corfu / Company ─────────────────────────────────
   `(corfu-default                    ((t (:background ,bg-panel))))
   `(corfu-current                    ((t (:background ,bg-highlight :foreground ,fg-main))))
   `(corfu-bar                        ((t (:background ,glass-edge))))
   `(corfu-border                     ((t (:background ,glass-frost))))
   `(corfu-annotations                ((t (:foreground ,fg-faint))))
   `(corfu-deprecated                 ((t (:foreground ,fg-ghost :strike-through t))))
   `(company-tooltip                  ((t (:background ,bg-panel :foreground ,fg-main))))
   `(company-tooltip-selection        ((t (:background ,bg-highlight))))
   `(company-tooltip-common           ((t (:foreground ,ice :weight bold))))
   `(company-tooltip-annotation       ((t (:foreground ,fg-faint))))
   `(company-scrollbar-bg             ((t (:background ,bg-alt))))
   `(company-scrollbar-fg             ((t (:background ,glass-edge))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Org mode                                       ║
   ;; ╚══════════════════════════════════════════════════╝
   `(org-level-1                      ((t (:foreground ,ice-bright :weight bold :height 1.3))))
   `(org-level-2                      ((t (:foreground ,azure :weight bold :height 1.2))))
   `(org-level-3                      ((t (:foreground ,teal :weight bold :height 1.1))))
   `(org-level-4                      ((t (:foreground ,violet :weight bold))))
   `(org-level-5                      ((t (:foreground ,sky :weight bold))))
   `(org-level-6                      ((t (:foreground ,peach :weight bold))))
   `(org-level-7                      ((t (:foreground ,pink :weight bold))))
   `(org-level-8                      ((t (:foreground ,fg-dim :weight bold))))
   `(org-document-title               ((t (:foreground ,ice-bright :weight bold :height 1.4))))
   `(org-document-info                ((t (:foreground ,fg-dim))))
   `(org-document-info-keyword        ((t (:foreground ,fg-ghost))))
   `(org-meta-line                    ((t (:foreground ,fg-ghost))))
   `(org-block                        ((t (:background ,bg-deep :extend t))))
   `(org-block-begin-line             ((t (:foreground ,fg-ghost :background ,bg-deep :extend t))))
   `(org-block-end-line               ((t (:foreground ,fg-ghost :background ,bg-deep :extend t))))
   `(org-code                         ((t (:foreground ,teal :background ,bg-deep))))
   `(org-verbatim                     ((t (:foreground ,peach :background ,bg-deep))))
   `(org-table                        ((t (:foreground ,fg-dim))))
   `(org-formula                      ((t (:foreground ,amber))))
   `(org-todo                         ((t (:foreground ,red :weight bold))))
   `(org-done                         ((t (:foreground ,green :weight bold))))
   `(org-headline-done                ((t (:foreground ,fg-faint))))
   `(org-priority                     ((t (:foreground ,amber))))
   `(org-tag                          ((t (:foreground ,fg-faint :weight normal))))
   `(org-date                         ((t (:foreground ,ice :underline t))))
   `(org-special-keyword              ((t (:foreground ,fg-ghost))))
   `(org-drawer                       ((t (:foreground ,fg-ghost))))
   `(org-property-value               ((t (:foreground ,fg-faint))))
   `(org-checkbox                     ((t (:foreground ,ice :weight bold))))
   `(org-list-dt                      ((t (:foreground ,ice :weight bold))))
   `(org-ellipsis                     ((t (:foreground ,fg-ghost :underline nil))))
   `(org-link                         ((t (:foreground ,ice :underline t))))
   `(org-footnote                     ((t (:foreground ,cyan :underline t))))
   `(org-scheduled                    ((t (:foreground ,fg-main))))
   `(org-scheduled-today              ((t (:foreground ,ice :weight bold))))
   `(org-scheduled-previously         ((t (:foreground ,amber))))
   `(org-upcoming-deadline            ((t (:foreground ,red))))
   `(org-agenda-done                  ((t (:foreground ,green-dim))))
   `(org-agenda-structure             ((t (:foreground ,ice :weight bold))))
   `(org-agenda-date                  ((t (:foreground ,fg-dim))))
   `(org-agenda-date-today            ((t (:foreground ,ice :weight bold :underline t))))
   `(org-agenda-date-weekend          ((t (:foreground ,fg-faint))))
   `(org-agenda-current-time          ((t (:foreground ,amber))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Magit / Diff                                   ║
   ;; ╚══════════════════════════════════════════════════╝
   `(magit-section-heading            ((t (:foreground ,ice :weight bold))))
   `(magit-section-highlight          ((t (:background ,bg-alt :extend t))))
   `(magit-section-heading-selection  ((t (:foreground ,amber :weight bold))))
   `(magit-branch-local               ((t (:foreground ,ice))))
   `(magit-branch-remote              ((t (:foreground ,green))))
   `(magit-branch-remote-head         ((t (:foreground ,green :weight bold))))
   `(magit-branch-current             ((t (:foreground ,ice :weight bold :box t))))
   `(magit-tag                        ((t (:foreground ,amber))))
   `(magit-hash                       ((t (:foreground ,fg-faint))))
   `(magit-log-author                 ((t (:foreground ,fg-dim))))
   `(magit-log-date                   ((t (:foreground ,fg-faint))))
   `(magit-log-graph                  ((t (:foreground ,fg-ghost))))
   `(magit-dimmed                     ((t (:foreground ,fg-ghost))))
   `(magit-filename                   ((t (:foreground ,fg-main))))
   `(magit-diff-file-heading          ((t (:foreground ,fg-main :weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,bg-alt :weight bold))))
   `(magit-diff-hunk-heading          ((t (:background ,bg-panel :foreground ,fg-dim))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,bg-highlight :foreground ,fg-main))))
   `(magit-diff-context               ((t (:foreground ,fg-faint :extend t))))
   `(magit-diff-context-highlight     ((t (:background ,bg-alt :foreground ,fg-dim :extend t))))
   `(magit-diff-added                 ((t (:background ,diff-add-bg :foreground ,diff-add-fg :extend t))))
   `(magit-diff-added-highlight       ((t (:background ,diff-add-bg :foreground ,diff-add-fg :extend t :weight bold))))
   `(magit-diff-removed               ((t (:background ,diff-remove-bg :foreground ,diff-remove-fg :extend t))))
   `(magit-diff-removed-highlight     ((t (:background ,diff-remove-bg :foreground ,diff-remove-fg :extend t :weight bold))))
   `(magit-diffstat-added             ((t (:foreground ,green))))
   `(magit-diffstat-removed           ((t (:foreground ,red))))

   `(diff-added                       ((t (:background ,diff-add-bg :foreground ,diff-add-fg :extend t))))
   `(diff-removed                     ((t (:background ,diff-remove-bg :foreground ,diff-remove-fg :extend t))))
   `(diff-changed                     ((t (:background ,diff-change-bg :foreground ,diff-change-fg :extend t))))
   `(diff-header                      ((t (:background ,bg-panel :foreground ,fg-dim :extend t))))
   `(diff-file-header                 ((t (:background ,bg-panel :foreground ,ice :weight bold :extend t))))
   `(diff-hunk-header                 ((t (:background ,bg-alt :foreground ,ice))))
   `(diff-refine-added                ((t (:background "#1a4a28" :foreground ,green :weight bold))))
   `(diff-refine-removed              ((t (:background "#4a1a1a" :foreground ,red :weight bold))))
   `(diff-refine-changed              ((t (:background "#2a2a50" :foreground ,azure :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Flycheck / Flymake                             ║
   ;; ╚══════════════════════════════════════════════════╝
   `(flycheck-error                   ((t (:underline (:color ,red :style wave)))))
   `(flycheck-warning                 ((t (:underline (:color ,amber :style wave)))))
   `(flycheck-info                    ((t (:underline (:color ,ice :style wave)))))
   `(flycheck-fringe-error            ((t (:foreground ,red))))
   `(flycheck-fringe-warning          ((t (:foreground ,amber))))
   `(flycheck-fringe-info             ((t (:foreground ,ice))))
   `(flycheck-error-list-error        ((t (:foreground ,red))))
   `(flycheck-error-list-warning      ((t (:foreground ,amber))))
   `(flycheck-error-list-info         ((t (:foreground ,ice))))
   `(flymake-error                    ((t (:underline (:color ,red :style wave)))))
   `(flymake-warning                  ((t (:underline (:color ,amber :style wave)))))
   `(flymake-note                     ((t (:underline (:color ,ice :style wave)))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Elfeed                                         ║
   ;; ╚══════════════════════════════════════════════════╝
   `(elfeed-search-feed-face          ((t (:foreground ,ice))))
   `(elfeed-search-tag-face           ((t (:foreground ,fg-faint))))
   `(elfeed-search-title-face         ((t (:foreground ,fg-dim))))
   `(elfeed-search-unread-title-face  ((t (:foreground ,fg-main :weight bold))))
   `(elfeed-search-date-face          ((t (:foreground ,fg-ghost))))
   `(elfeed-search-unread-count-face  ((t (:foreground ,ice))))
   `(elfeed-log-info-level-face       ((t (:foreground ,ice))))
   `(elfeed-log-warn-level-face       ((t (:foreground ,amber))))
   `(elfeed-log-error-level-face      ((t (:foreground ,red))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Dired                                          ║
   ;; ╚══════════════════════════════════════════════════╝
   `(dired-directory                  ((t (:foreground ,ice :weight bold))))
   `(dired-symlink                    ((t (:foreground ,cyan))))
   `(dired-ignored                    ((t (:foreground ,fg-ghost))))
   `(dired-flagged                    ((t (:foreground ,red :weight bold))))
   `(dired-marked                     ((t (:foreground ,amber :weight bold))))
   `(dired-perm-write                 ((t (:foreground ,amber))))
   `(dired-header                     ((t (:foreground ,ice :weight bold :underline t))))
   `(dired-mark                       ((t (:foreground ,amber :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Which-key                                      ║
   ;; ╚══════════════════════════════════════════════════╝
   `(which-key-key-face               ((t (:foreground ,ice :weight bold))))
   `(which-key-command-description-face ((t (:foreground ,fg-main))))
   `(which-key-group-description-face ((t (:foreground ,amber))))
   `(which-key-separator-face         ((t (:foreground ,fg-ghost))))
   `(which-key-note-face              ((t (:foreground ,fg-faint))))
   `(which-key-special-key-face       ((t (:foreground ,amber :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Doom modeline                                  ║
   ;; ╚══════════════════════════════════════════════════╝
   `(doom-modeline-bar                ((t (:background ,glass-glow))))
   `(doom-modeline-bar-inactive       ((t (:background ,bg-alt))))
   `(doom-modeline-buffer-file        ((t (:foreground ,ice-bright :weight bold))))
   `(doom-modeline-buffer-modified    ((t (:foreground ,amber :weight bold))))
   `(doom-modeline-buffer-major-mode  ((t (:foreground ,fg-dim :weight bold))))
   `(doom-modeline-buffer-minor-mode  ((t (:foreground ,fg-faint))))
   `(doom-modeline-buffer-path        ((t (:foreground ,fg-dim))))
   `(doom-modeline-project-dir        ((t (:foreground ,ice :weight bold))))
   `(doom-modeline-info               ((t (:foreground ,green))))
   `(doom-modeline-warning            ((t (:foreground ,amber))))
   `(doom-modeline-urgent             ((t (:foreground ,red))))
   `(doom-modeline-evil-insert-state  ((t (:foreground ,green))))
   `(doom-modeline-evil-normal-state  ((t (:foreground ,ice))))
   `(doom-modeline-evil-visual-state  ((t (:foreground ,amber))))
   `(doom-modeline-evil-emacs-state   ((t (:foreground ,violet))))
   `(doom-modeline-lsp-running        ((t (:foreground ,green))))
   `(doom-modeline-lsp-success        ((t (:foreground ,green))))
   `(doom-modeline-lsp-warning        ((t (:foreground ,amber))))
   `(doom-modeline-lsp-error          ((t (:foreground ,red))))
   `(doom-modeline-persp-name         ((t (:foreground ,ice :slant italic))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Perspective / persp-mode                       ║
   ;; ╚══════════════════════════════════════════════════╝
   `(persp-selected-face              ((t (:foreground ,ice :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Ace-window                                     ║
   ;; ╚══════════════════════════════════════════════════╝
   `(aw-leading-char-face             ((t (:foreground ,amber :weight bold :height 2.5))))
   `(aw-background-face               ((t (:foreground ,fg-ghost))))
   `(aw-minibuffer-leading-char-face  ((t (:foreground ,amber :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Eshell / Term / VTerm                          ║
   ;; ╚══════════════════════════════════════════════════╝
   `(eshell-prompt                    ((t (:foreground ,ice :weight bold))))
   `(eshell-ls-archive                ((t (:foreground ,violet))))
   `(eshell-ls-backup                 ((t (:foreground ,fg-faint))))
   `(eshell-ls-clutter                ((t (:foreground ,fg-ghost))))
   `(eshell-ls-directory              ((t (:foreground ,ice :weight bold))))
   `(eshell-ls-executable             ((t (:foreground ,green))))
   `(eshell-ls-missing                ((t (:foreground ,red))))
   `(eshell-ls-product                ((t (:foreground ,fg-dim))))
   `(eshell-ls-readonly               ((t (:foreground ,peach))))
   `(eshell-ls-special                ((t (:foreground ,magenta))))
   `(eshell-ls-symlink                ((t (:foreground ,cyan))))
   `(eshell-ls-unreadable             ((t (:foreground ,fg-ghost))))

   ;; ── Ansi colors (term/vterm) ─────────────────────────
   `(ansi-color-black                 ((t (:foreground ,bg-deep :background ,bg-deep))))
   `(ansi-color-red                   ((t (:foreground ,red :background ,red))))
   `(ansi-color-green                 ((t (:foreground ,green :background ,green))))
   `(ansi-color-yellow                ((t (:foreground ,amber :background ,amber))))
   `(ansi-color-blue                  ((t (:foreground ,ice :background ,ice))))
   `(ansi-color-magenta               ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-cyan                  ((t (:foreground ,cyan :background ,cyan))))
   `(ansi-color-white                 ((t (:foreground ,fg-main :background ,fg-main))))
   `(ansi-color-bright-black          ((t (:foreground ,fg-ghost :background ,fg-ghost))))
   `(ansi-color-bright-red            ((t (:foreground ,red :background ,red))))
   `(ansi-color-bright-green          ((t (:foreground ,green :background ,green))))
   `(ansi-color-bright-yellow         ((t (:foreground ,gold :background ,gold))))
   `(ansi-color-bright-blue           ((t (:foreground ,ice-bright :background ,ice-bright))))
   `(ansi-color-bright-magenta        ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-bright-cyan           ((t (:foreground ,teal :background ,teal))))
   `(ansi-color-bright-white          ((t (:foreground "#ffffff" :background "#ffffff"))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Evil                                           ║
   ;; ╚══════════════════════════════════════════════════╝
   `(evil-ex-info                     ((t (:foreground ,red :slant italic))))
   `(evil-ex-search                   ((t (:background ,electric :foreground ,fg-main :weight bold))))
   `(evil-ex-substitute-matches       ((t (:background ,red-faint :foreground ,red :strike-through t))))
   `(evil-ex-substitute-replacement   ((t (:foreground ,green :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Hydra                                          ║
   ;; ╚══════════════════════════════════════════════════╝
   `(hydra-face-red                   ((t (:foreground ,red :weight bold))))
   `(hydra-face-blue                  ((t (:foreground ,ice :weight bold))))
   `(hydra-face-amaranth              ((t (:foreground ,magenta :weight bold))))
   `(hydra-face-pink                  ((t (:foreground ,pink :weight bold))))
   `(hydra-face-teal                  ((t (:foreground ,teal :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Treesitter                                     ║
   ;; ╚══════════════════════════════════════════════════╝
   `(tree-sitter-hl-face:function         ((t (:foreground ,ice))))
   `(tree-sitter-hl-face:function.call    ((t (:foreground ,ice))))
   `(tree-sitter-hl-face:function.builtin ((t (:foreground ,teal))))
   `(tree-sitter-hl-face:function.macro   ((t (:foreground ,cyan :weight bold))))
   `(tree-sitter-hl-face:method           ((t (:foreground ,ice))))
   `(tree-sitter-hl-face:method.call      ((t (:foreground ,ice))))
   `(tree-sitter-hl-face:type             ((t (:foreground ,violet))))
   `(tree-sitter-hl-face:type.builtin     ((t (:foreground ,violet :slant italic))))
   `(tree-sitter-hl-face:variable         ((t (:foreground ,fg-main))))
   `(tree-sitter-hl-face:variable.builtin ((t (:foreground ,amber))))
   `(tree-sitter-hl-face:variable.parameter ((t (:foreground ,fg-dim :slant italic))))
   `(tree-sitter-hl-face:property         ((t (:foreground ,ice-bright))))
   `(tree-sitter-hl-face:keyword          ((t (:foreground ,azure :weight bold))))
   `(tree-sitter-hl-face:operator         ((t (:foreground ,sky))))
   `(tree-sitter-hl-face:string           ((t (:foreground ,teal-dim))))
   `(tree-sitter-hl-face:string.special   ((t (:foreground ,cyan))))
   `(tree-sitter-hl-face:constant         ((t (:foreground ,peach))))
   `(tree-sitter-hl-face:constant.builtin ((t (:foreground ,peach :weight bold))))
   `(tree-sitter-hl-face:number           ((t (:foreground ,peach))))
   `(tree-sitter-hl-face:comment          ((t (:foreground ,fg-ghost :slant italic))))
   `(tree-sitter-hl-face:punctuation      ((t (:foreground ,fg-faint))))
   `(tree-sitter-hl-face:punctuation.bracket  ((t (:foreground ,fg-faint))))
   `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,fg-faint))))
   `(tree-sitter-hl-face:tag              ((t (:foreground ,ice))))
   `(tree-sitter-hl-face:attribute        ((t (:foreground ,amber))))
   `(tree-sitter-hl-face:label            ((t (:foreground ,fg-dim))))
   `(tree-sitter-hl-face:embedded         ((t (:foreground ,teal))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  LSP                                            ║
   ;; ╚══════════════════════════════════════════════════╝
   `(lsp-face-highlight-textual       ((t (:background ,bg-highlight))))
   `(lsp-face-highlight-read          ((t (:background ,bg-highlight))))
   `(lsp-face-highlight-write         ((t (:background ,bg-selection))))
   `(lsp-headerline-breadcrumb-path-face         ((t (:foreground ,fg-faint))))
   `(lsp-headerline-breadcrumb-path-error-face   ((t (:foreground ,red :underline t))))
   `(lsp-headerline-breadcrumb-path-warning-face ((t (:foreground ,amber))))
   `(lsp-headerline-breadcrumb-path-hint-face    ((t (:foreground ,fg-faint))))
   `(lsp-headerline-breadcrumb-symbols-face      ((t (:foreground ,ice :weight bold))))
   `(lsp-headerline-breadcrumb-symbols-error-face ((t (:foreground ,red))))
   `(lsp-headerline-breadcrumb-separator-face    ((t (:foreground ,fg-ghost))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Show-paren / Rainbow delimiters                ║
   ;; ╚══════════════════════════════════════════════════╝
   `(show-paren-match                 ((t (:foreground ,amber :background ,bg-highlight :weight bold))))
   `(show-paren-mismatch              ((t (:foreground ,fg-main :background ,red :weight bold))))
   `(show-paren-match-expression      ((t (:background ,bg-alt))))
   `(rainbow-delimiters-depth-1-face  ((t (:foreground ,ice))))
   `(rainbow-delimiters-depth-2-face  ((t (:foreground ,amber))))
   `(rainbow-delimiters-depth-3-face  ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-4-face  ((t (:foreground ,magenta))))
   `(rainbow-delimiters-depth-5-face  ((t (:foreground ,teal))))
   `(rainbow-delimiters-depth-6-face  ((t (:foreground ,peach))))
   `(rainbow-delimiters-depth-7-face  ((t (:foreground ,violet))))
   `(rainbow-delimiters-depth-8-face  ((t (:foreground ,cyan))))
   `(rainbow-delimiters-depth-9-face  ((t (:foreground ,pink))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  hl-todo                                        ║
   ;; ╚══════════════════════════════════════════════════╝
   `(hl-todo                          ((t (:foreground ,amber :weight bold :slant italic))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Highlight numbers/quoted/defined                ║
   ;; ╚══════════════════════════════════════════════════╝
   `(highlight-numbers-number         ((t (:foreground ,peach))))
   `(highlight-quoted-symbol          ((t (:foreground ,teal))))
   `(highlight-quoted-quote           ((t (:foreground ,cyan))))
   `(highlight-defined-function-name-face ((t (:foreground ,ice))))
   `(highlight-defined-variable-name-face ((t (:foreground ,fg-main))))
   `(highlight-defined-builtin-name-face  ((t (:foreground ,teal))))
   `(highlight-defined-face-name-face     ((t (:foreground ,violet))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Dashboard                                      ║
   ;; ╚══════════════════════════════════════════════════╝
   `(dashboard-banner-logo-title      ((t (:foreground ,ice-bright :weight bold))))
   `(dashboard-heading                ((t (:foreground ,ice :weight bold))))
   `(dashboard-items-face             ((t (:foreground ,fg-dim))))
   `(dashboard-no-items-face          ((t (:foreground ,fg-ghost))))
   `(dashboard-footer-face            ((t (:foreground ,fg-faint :slant italic))))
   `(dashboard-navigator              ((t (:foreground ,ice))))
   `(dashboard-footer-icon-face       ((t (:foreground ,ice))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Denote                                         ║
   ;; ╚══════════════════════════════════════════════════╝
   `(denote-faces-link                ((t (:foreground ,ice :underline t))))
   `(denote-faces-date                ((t (:foreground ,fg-faint))))
   `(denote-faces-keywords            ((t (:foreground ,teal :slant italic))))
   `(denote-faces-title               ((t (:foreground ,fg-main :weight bold))))
   `(denote-faces-extension           ((t (:foreground ,fg-ghost))))
   `(denote-faces-subdirectory        ((t (:foreground ,ice))))
   `(denote-faces-signature           ((t (:foreground ,amber))))
   `(denote-faces-delimiter           ((t (:foreground ,fg-ghost))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Info / Help / Man                              ║
   ;; ╚══════════════════════════════════════════════════╝
   `(info-title-1                     ((t (:foreground ,ice-bright :weight bold :height 1.3))))
   `(info-title-2                     ((t (:foreground ,azure :weight bold :height 1.2))))
   `(info-title-3                     ((t (:foreground ,teal :weight bold :height 1.1))))
   `(info-title-4                     ((t (:foreground ,violet :weight bold))))
   `(info-header-xref                 ((t (:foreground ,ice :underline t))))
   `(info-header-node                 ((t (:foreground ,ice-bright :weight bold))))
   `(info-menu-star                   ((t (:foreground ,amber))))
   `(info-xref                        ((t (:foreground ,ice :underline t))))
   `(info-xref-visited                ((t (:foreground ,violet :underline t))))
   `(Man-overstrike                   ((t (:foreground ,ice :weight bold))))
   `(Man-underline                    ((t (:foreground ,teal :underline t))))
   `(woman-bold                       ((t (:foreground ,ice :weight bold))))
   `(woman-italic                     ((t (:foreground ,teal :slant italic))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Bookmarks / Registers                          ║
   ;; ╚══════════════════════════════════════════════════╝
   `(bookmark-face                    ((t (:foreground ,amber :background ,bg-alt))))
   `(bookmark-menu-bookmark           ((t (:foreground ,ice :weight bold))))

   ;; ╔══════════════════════════════════════════════════╗
   ;; ║  Miscellaneous                                  ║
   ;; ╚══════════════════════════════════════════════════╝
   `(tooltip                          ((t (:background ,bg-panel :foreground ,fg-main))))
   `(menu                             ((t (:background ,bg-panel :foreground ,fg-main))))
   `(fixed-pitch                      ((t (:family "JetBrainsMono"))))
   `(fixed-pitch-serif                ((t (:family "JetBrainsMono"))))
   `(variable-pitch                   ((t (:family "sans-serif"))))
   `(italic                           ((t (:slant italic))))
   `(bold                             ((t (:weight bold))))
   `(bold-italic                      ((t (:weight bold :slant italic))))
   `(underline                        ((t (:underline t))))
   `(compilation-error                ((t (:foreground ,red))))
   `(compilation-warning              ((t (:foreground ,amber))))
   `(compilation-info                 ((t (:foreground ,ice))))
   `(compilation-line-number          ((t (:foreground ,fg-faint))))
   `(compilation-column-number        ((t (:foreground ,fg-faint))))
   `(compilation-mode-line-exit       ((t (:foreground ,green :weight bold))))
   `(compilation-mode-line-fail       ((t (:foreground ,red :weight bold))))
   `(compilation-mode-line-run        ((t (:foreground ,amber :weight bold))))
   `(whitespace-space                 ((t (:foreground ,bg-panel))))
   `(whitespace-tab                   ((t (:foreground ,bg-panel))))
   `(whitespace-newline               ((t (:foreground ,bg-panel))))
   `(whitespace-trailing              ((t (:background ,red-faint))))
   `(whitespace-line                  ((t (:background ,bg-alt))))
   `(whitespace-indentation           ((t (:foreground ,bg-panel))))))

;; ── Glass activation helper ──────────────────────────────
;;;###autoload
(defun aero-glass-activate (&optional transparency)
  "Load aero-glass theme and set up glass frame parameters.
TRANSPARENCY is the alpha-background value (0-100), default 88."
  (interactive)
  (let ((alpha (or transparency 88)))
    (load-theme 'aero-glass t)
    ;; Frame transparency — the compositor (picom) does the blur
    (set-frame-parameter nil 'alpha-background alpha)
    (add-to-list 'default-frame-alist `(alpha-background . ,alpha))
    ;; Internal border gives breathing room
    (set-frame-parameter nil 'internal-border-width 12)
    (add-to-list 'default-frame-alist '(internal-border-width . 12))
    ;; Right divider for glass panel separation
    (set-frame-parameter nil 'right-divider-width 12)
    (add-to-list 'default-frame-alist '(right-divider-width . 12))
    (message "Aero Glass activated (transparency: %d%%)" alpha)))

;;;###autoload
(defun aero-glass-deactivate ()
  "Disable glass effects and restore opaque frame."
  (interactive)
  (disable-theme 'aero-glass)
  (set-frame-parameter nil 'alpha-background 100)
  (message "Aero Glass deactivated"))

(provide-theme 'aero-glass)

;;; aero-glass-theme.el ends here
