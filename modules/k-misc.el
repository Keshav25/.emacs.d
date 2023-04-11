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
  ("C-h RET" . 'helpful-at-point)
  ("C-h F" . 'helpful-function))

(provide 'k-misc)
