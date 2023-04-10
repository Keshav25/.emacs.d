(leaf vertico
  :ensure t
  :config
  (vertico-mode 1))

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
		 ([remap switch-to-buffer] . consult-buffer)))

(leaf embark-consult
  :after (embark consult)
  :ensure t
  :hook ((embark-collect-mode . consult-preview-at-point-mode)))

(leaf avy-embark-collect
  :after (embark avy)
  :ensure t)

;; Which-Key
(leaf which-key
  :ensure t
  :init
  (which-key-mode))

(leaf corfu)
(leaf cape)

(leaf embark
  :ensure t
  :bind (("C-c h a" . embark-act)
		 ("C-c h d" . embark-dwim)
		 ("C-c h b" . embark-bindings)
		 (:evil-normal-state-map
		  ("zd" . embark-act)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))


(provide 'k-emocs)
