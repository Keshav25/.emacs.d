(leaf vertico
  :ensure t
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  :bind (:vertico-map
		 ("C-'" . vertico-quick-exit)
		 ("C-i" . vertico-insert)
		 ("C-w" . vertico-directory-delete-char)
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

;; Remember to replace the api keys with a function, load it from a secrets file
(leaf consult-web
  :disabled t
  :after consult
  :quelpa (consult-web :fetcher github :repo "armindarvish/consult-web")
  :require t
  :custom
  (consult-web-show-preview . t) ;;; show previews
  (consult-web-preview-key . "C-o") ;;; set the preview key to C-o
  (consult-web-highlight-matches . t) ;;; highlight matches in minibuffer
  (consult-web-default-count . 5) ;;; set default count
  (consult-web-default-page . 0) ;;; set the default page (default is 0 for the first page)
  (consult-web-google-customsearch-key . "AIzaSyAvBsS4XZEQbhfutYoKjXl4zw_29nK1K7E")
  (consult-web-google-customsearch-cx . "e068f129c16314581")
  :config
  ;;; set multiple sources for consult-web-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-web-sources-alist'.
  (setq consult-web-multi-sources '("Brave" "Wikipedia" "chatGPT" "Google")) ;; consult-web-multi
  (setq consult-web-dynamic-sources '("gptel" "Brave" "StackOverFlow" )) ;; consult-web-dynamic
  (setq consult-web-scholar-sources '("PubMed")) ;; consult-web-scholar
  (setq consult-web-omni-sources (list "elfeed" "Brave" "Wikipedia" "gptel" "YouTube" 'consult-buffer-sources 'consult-notes-all-sources)) ;;consult-web-omni
  (setq consult-web-dynamic-omni-sources (list "Known Project" "File" "Bookmark" "Buffer" "Reference Roam Nodes" "Zettel Roam Nodes" "Line Multi" "elfeed" "Brave" "Wikipedia" "gptel" "Youtube")) ;;consult-web-dynamic-omni
  (require 'consult-web-google)

  (add-to-list 'consult-web-dynamic-sources "Google") ;; or (add-to-list 'consult-web-multi-sources...)
  (require 'consult-web-notes)
  ;; Per source customization
  ;;; Pick you favorite autosuggest command.
  (require 'consult-web-google-autosuggest)
  (setq consult-web-default-autosuggest-command #'consult-web-dynamic-google-autosuggest)

  ;;; Set API KEYs. It is recommended to use a function that returns the string for better security.
  )

(leaf consult-org-roam
  :ensure t
  :config
  (consult-org-roam-mode))

(leaf avy-embark-collect
  :after (embark avy)
  :ensure t
  :bind
  ;;jump to any character in any window
  (("C-j" . 'avy-goto-char)))

;; Which-Key
(leaf which-key
  :ensure t
  :custom
  (which-key-allow-multiple-replacements . t)
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
