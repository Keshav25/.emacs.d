;; User Information
(setq user-full-name "Keshav Italia"
	  user-mail-address "keshavitalia0@gmail.com")

										; Async
(leaf async
  :ensure t
  :init
  (dired-async-mode 1))

;; keep emacs clean
(leaf no-littering :ensure t)

;; Gcmh
(leaf gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(leaf helpful
  :ensure t
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-h x" . 'helpful-command)
  ("C-h o" . 'helpful-symbol)
  ("C-h RET" . 'helpful-at-point)
  ("C-h F" . 'helpful-function)
  ;; not helpful but might aswell
  ("C-h K" . 'describe-keymap)
  (:helpful-mode-map
   ([remap revert-buffer] . 'helpful-update)))

(leaf elisp-demos
  :ensure t
  :after (helpful)
  :require t
  :advice
  (:after helpful-update elisp-demos-advice-helpful-update))

(provide 'k-misc)
