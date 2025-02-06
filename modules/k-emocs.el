(leaf vertico
  :ensure t
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  :bind (:vertico-map
		 ("C-'" . vertico-quick-exit)
		 ("C-i" . vertico-insert)
		 ("C-w" . vertico-directory-delete-char)
		 ("C-j" . vertico-exit-input)
		 ("C-c C-c" . vertico-exit-input)))

(leaf orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(leaf marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(leaf consult
  :ensure t
  :bind (("C-s" . consult-line)
		 ([remap switch-to-buffer] . consult-buffer)
		 ("C-y" . consult-yank-pop)
		 ("C-M-y" . yank)
		 ("C-S-s" . consult-ripgrep)
		 ("M-s M-s" . consult-outline))
  :custom
  ;; (completion-in-region-function . #'consult-completion-in-region)
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)
  (consult-project-root-function . #'deadgrep--project-root))

(leaf consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
		 (:minibuffer-local-completion-map
		  ("C-x C-d" . consult-dir)
		  ("C-x C-j" . consult-dir-jump-file))))

(leaf embark-consult
  :after (embark consult)
  :ensure t
  :hook ((embark-collect-mode . consult-preview-at-point-mode)))

(leaf consult-notes
  :ensure t)

(use-package consult-omni
  :straight (consult-omni :type git :host github :repo "armindarvish/consult-omni" :branch "main" :files (:defaults "sources/*.el"))
  :after consult
  :config
  ;; Load Sources Core code
  (require 'consult-omni-sources)
  ;; Load Embark Actions
  (require 'consult-omni-embark)
  (setq consult-omni-sources-modules-to-load '(consult-omni-wikipedia))
  (consult-omni-sources-load-modules))

(leaf consult-org-roam
  :after (org-roam)
  :ensure t
  :config
  (consult-org-roam-mode))

(leaf avy
  :ensure t
  :bind
  (("M-N" . avy-goto-char)))

(leaf avy-embark-collect
  :after (embark avy)
  :ensure t)

;; Which-Key
(leaf which-key
  :ensure t
  :custom
  (which-key-allow-multiple-replacements . t)
  (which-key-min-display-lines . 1)
  (which-key-idle-delay . 0.125)
  :init
  (which-key-mode 1))

(leaf corfu)
(leaf cape)

(leaf embark
  :ensure t
  :hook ((embark-collect-mode . display-line-numbers-mode))
  :bind (("C-c h a" . embark-act)
		 ("C-c h d" . embark-dwim)
		 ("C-c h b" . embark-bindings)
		 ("C-;" . embark-collect)
		 ;; (:evil-normal-state-map
		 ;; ("zd" . embark-act)
		 )
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :hook ((embark-collect-mode . display-line-numbers-mode))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

(leaf consult-gh
  :quelpa (consult-gh :fetcher github :repo "armindarvish/consult-gh"))

(leaf placeholder
  ;; TODO Remember to bind EXT key + top row for these bindings on the ZSA Voyager
  :quelpa (placeholder :fetcher github :repo "oantolin/placeholder")
  :bind (("C-c p n" . placeholder-forward)
		 ("C-c p p" . placeholder-backward)
		 ("C-c p i" . placeholder-insert)))

;; (leaf hyperbole
;;   :ensure t)

(leaf eev
  :disabled t
  :ensure t)

(leaf rg
  :ensure t
  :config
  (setq rg-executable (executable-find "rga")))

(leaf posframe
  :config
  (leaf vertico-posframe
	:ensure t
	:require t
	:custom
	(vertico-posframe-border-width . 1)
	:config
	(setq vertico-posframe-parameters
		  '((alpha . 100)
			(alpha-background . 90)))
	(vertico-posframe-mode 1))
  (leaf which-key-posframe
	:after (which-key)
	:ensure t
	:config
	(which-key-posframe-mode 1))
  (leaf transient-posframe
	:after (magit)
	:ensure t
	:config
	(transient-posframe-mode 1))
  (leaf flycheck-posframe
	:after (flycheck)
	:ensure t
	:hook
	(flycheck-mode . flycheck-posframe-mode)))

(leaf bufler
  :ensure t
  :bind (([remap list-buffers] . bufler)))

(leaf completion-preview
  :ensure t
  :config
  (global-completion-preview-mode)
  (push 'org-self-insert-command completion-preview-mode)
  (setq completion-auto-select t
		completion-auto-help 'visible
		completions-format 'one-column
		completions-sort 'historical
		completions-max-height 20
		completion-ignore-case t))


(provide 'k-emocs)

;;; k-emocs.el ends here
