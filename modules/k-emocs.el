(leaf vertico
  :config
  (vertico-mode 1))

(leaf orderless
      :config
      (setq completion-styles '(orderless)))

(leaf marginalia
  :config
  (marginalia-mode 1))

(leaf consult
  :bind (("C-s" . consult-line)
		 ([remap switch-to-buffer] . consult-buffer)))
  
(leaf corfu)
(leaf cape)
(leaf embark)

(provide 'k-emocs)
