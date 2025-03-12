(leaf vertico
  :elpaca t
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
  :elpaca t
  :custom
  (completion-styles . '(orderless))
  (completion-category-overrides . '((file (styles basic partial-completion)))))

(leaf marginalia
  :elpaca t
  :config
  (marginalia-mode 1))

(leaf consult
  :elpaca t
  :bind (("C-s" . consult-line)
		 ([remap switch-to-buffer] . consult-buffer)
		 ("C-y" . consult-yank-pop)
		 ("C-M-y" . yank)
		 ("C-S-s" . consult-ripgrep)
		 ("C-x r b" . consult-bookmark)
		 ("C-x p b" . consult-project-buffer) 
		 ("M-s M-s" . consult-outline)
		 ("M-g e" . consult-compile-error) 
         ("M-g f" . consult-flymake) 
         ("M-g g" . consult-goto-line) 
         ("M-g M-g" . consult-goto-line) 
         ("M-g o" . consult-outline) 
         ("M-g m" . consult-mark) 
         ("M-g k" . consult-global-mark) 
         ("M-g i" . consult-imenu) 
         ("M-g I" . consult-imenu-multi) 
		 ("M-g k" . consult-global-mark)
		 ("M-s d" . consult-find) 
         ("M-s D" . consult-locate) 
         ("M-s g" . consult-grep) 
         ("M-s G" . consult-git-grep) 
         ("M-s r" . consult-ripgrep) 
         ("M-s l" . consult-line) 
         ("M-s L" . consult-line-multi) 
         ("M-s k" . consult-keep-lines) 
         ("M-s u" . consult-focus-lines) 
         ("M-s e" . consult-isearch-history)
		 ("C-c n t" . consult-org-heading)
		 ([f6] . consult-recent-file))
  :config
  (advice-add #'register-preview 
			  :override #'consult-register-window)
  (consult-customize consult-theme 
                     :preview-key '(:debounce 0.2 any) 
                     consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register consult--source-recent-file consult--source-project-recent-file 
                     :preview-key '(:debounce 0.4 any))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  ;; (completion-in-region-function . #'consult-completion-in-region)
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)
  (consult-project-root-function . #'deadgrep--project-root)
  (consult-ripgrep-args . "~/.cargo/bin/rga --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --search-zip")
  (register-preview-delay . 0.5)
  (register-preview-function . #'consult-register-format)
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)
  (consult-narrow-key . "<"))

(leaf consult-dir
  :elpaca t
  :bind (("C-x C-d" . consult-dir)
		 (:minibuffer-local-completion-map
		  ("C-x C-d" . consult-dir)
		  ("C-x C-j" . consult-dir-jump-file))))

(leaf embark-consult
  :after (embark consult)
  :elpaca t
  :hook ((embark-collect-mode . consult-preview-at-point-mode)))

(leaf consult-notes
  :elpaca t)

(leaf consult-omni
  :elpaca (consult-omni :host github :repo "armindarvish/consult-omni" :branch "main" :files (:defaults "sources/*.el"))
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
  :elpaca t
  :config
  (consult-org-roam-mode))

(leaf avy
  :elpaca t
  :bind
  (("M-N" . avy-goto-char)))

(leaf avy-embark-collect
  :after (embark avy)
  :elpaca t)

;; Which-Key
(leaf which-key
  :elpaca t
  :custom
  (which-key-allow-multiple-replacements . t)
  (which-key-min-display-lines . 1)
  (which-key-idle-delay . 0.125)
  :init
  (which-key-mode 1))

(leaf corfu)
(leaf cape)

(leaf embark
  :elpaca t
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
  :elpaca t)

(leaf placeholder
  ;; TODO Remember to bind EXT key + top row for these bindings on the ZSA Voyager
  :elpaca (placeholder :host github :repo "oantolin/placeholder")
  :bind (("C-c p n" . placeholder-forward)
		 ("C-c p p" . placeholder-backward)
		 ("C-c p i" . placeholder-insert)))

;; (leaf hyperbole
;;   :elpaca t)

(leaf eev
  :disabled t
  :elpaca t)

(leaf rg
  :elpaca t
  :config
  (setq rg-executable (executable-find "rga")))

(leaf posframe
  :config
  (leaf vertico-posframe
	:elpaca t
	:require t
	:custom
	(vertico-posframe-border-width . 0)
	:config
	(setq vertico-posframe-parameters
		  '((alpha . 100)
			(alpha-background . 90)))
	(vertico-posframe-mode 1))
  (leaf which-key-posframe
	:after (which-key)
	:elpaca t
	:config
	(which-key-posframe-mode 1))
  (leaf transient-posframe
	:after (magit)
	:elpaca t
	:config
	(transient-posframe-mode 1))
  (leaf flycheck-posframe
	:after (flycheck)
	:elpaca t
	:hook
	(flycheck-mode . flycheck-posframe-mode)))

(leaf bufler
  :elpaca t
  :bind (([remap list-buffers] . bufler)))

(leaf completion-preview
  :config
  (global-completion-preview-mode)
  (push 'org-self-insert-command completion-preview-mode)
  (setq completion-auto-select t
		completion-auto-help 'visible
		completions-format 'one-column
		completions-sort 'historical
		completions-max-height 20
		completion-ignore-case t))

(leaf file-previews
  :config
  (defun k/find-file-preview ()
    (interactive)
    (let ((consult-ripgrep-command "rga --multiline --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep)))
  :bind ("C-x p C-s" . k/find-file-preview))

(leaf ripgrep
  :elpaca t)

(leaf p-search
  :elpaca (p-search :host github :repo "zkry/p-search")
  :require t)


(provide 'k-emocs)

;;; k-emocs.el ends here
