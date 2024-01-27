(leaf vertico
  :ensure t
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  :bind (:vertico-map
		 ("C-'" . vertico-quick-exit)
		 ("C-i" . vertico-insert)
		 ("C-w" . vertico-directory-delete-char)))

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
		 ("C-y" . consult-yank-pop))
  :custom
  ;; (completion-in-region-function . #'consult-completion-in-region)
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)
  (consult-project-root-function . #'deadgrep--project-root)) ;; ensure ripgrep works)

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

(leaf avy-embark-collect
  :after (embark avy)
  :ensure t
  :bind
  ;;jump to any character in any window
  (("C-j" . 'avy-goto-char)))

;; Which-Key
(leaf which-key
  :ensure t
  :init
  (which-key-mode))

(leaf corfu)
(leaf cape)

(leaf embark
  :ensure t
  :hook ((embark-collect-mode . display-line-numbers-mode))
  :bind (("C-c h a" . embark-act)
		 ("C-c h d" . embark-dwim)
		 ("C-c h b" . embark-bindings)
		 ("C-;" . embark-collect)
		 (:evil-normal-state-map
		  ("zd" . embark-act)))
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

(leaf yasnippet
  :ensure t
  :config
  (yas-global-mode)
  (yas-reload-all)
  :hook
  (prog-mode-hook . yas-minor-mode)
  (text-mode-hook . yas-minor-mode)
  :custom
  (yas-prompt-functions . '(yas-completing-prompt)))

(leaf placeholder
  :quelpa (placeholder :fetcher github :repo "oantolin/placeholder")
  :bind (("C-S-n" . placeholder-forward)
		 ("C-S-p" . placeholder-backward)
		 ("C-S-x" . placeholder-insert)))

;; (leaf hyperbole
;;   :ensure t)

(leaf eev
  :ensure t)

(provide 'k-emocs)

;;; k-emocs.el ends here
