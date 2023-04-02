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
  
;; Avy
(leaf avy :ensure t)

(leaf corfu :ensure t)
(leaf cape :ensure t)
(leaf embark :ensure t)

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

(provide 'k-emocs)
