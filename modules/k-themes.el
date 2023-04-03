;; font
;; (set-frame-parameter (selected-frame) 'font "JetBrains Mono Bold-8")
  
(set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)

;; theme packages
(leaf kaolin-themes :ensure t)
(leaf doom-themes :ensure t)
(leaf ef-themes :ensure t)

;; load the actual theme
(load-theme 'ef-bio t)

;; Solaire
(leaf solaire-mode
  :ensure t
  :config
  (solaire-global-mode))

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

(provide 'k-themes)
