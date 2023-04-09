(leaf vertico
  :ensure t
  :config
  (vertico-mode 1))

leaf orderless
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
  :ensure t)

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
  :bind (("C-c h" . embark-act)
		 (:evil-normal-state-map
		  ("zd" . embark-act))))

(provide 'k-emocs)
