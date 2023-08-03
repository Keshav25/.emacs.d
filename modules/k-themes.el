;; font
;; (set-frame-parameter (selected-frame) 'font "JetBrains Mono Bold-8")
  
;; (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)

(global-hl-line-mode 1)

;; Show changes in the left margins of the buffer
(global-diff-hl-mode 1)

;; theme packages
(leaf kaolin-themes :ensure t)
(leaf doom-themes :ensure t)
(leaf ef-themes :ensure t)
(leaf yabaki-theme :ensure t)
(leaf haki-theme :ensure t)

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

;; ;; load the actual theme
(load-theme 'kaolin-dark t)

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

(leaf all-the-icons-nerd-fonts
			 :quelpa
			 (all-the-icons-nerd-fonts :fetcher github :repo "mohkale/all-the-icons-nerd-fonts"))

(provide 'k-themes)
