;; dap-mode
;; https://develop.spacemacs.org/layers/+lang/rust/README.html
;; https://docs.doomemacs.org/v21.12/modules/lang/rust/
;; rustfmt

(leaf rustic
  :ensure t
  :custom
  (rust-format-on-save . t)
  :bind (:rustic-mode-map
		 ("M-j" . lsp-ui-imenu)
		 ("M-?" . lsp-find-references)
		 ("C-c C-c l" . flycheck-list-errors)
		 ("C-c C-c a" . lsp-execute-code-action)
		 ("C-c C-c r" . lsp-rename)
		 ("C-c C-c q" . lsp-workspace-restart)
		 ("C-c C-c Q" . lsp-workspace-shutdown)
		 ("C-c C-c s" . lsp-rust-analyzer-status)
		 ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
		 ("C-c C-c d" . dap-hydra)
		 ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

  (defun rk/rustic-mode-hook ()
	;; so that run C-c C-c C-r works without having to confirm, but don't try to
	;; save rust buffers that are not file visiting. Once
	;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
	;; no longer be necessary.
	(when buffer-file-name
	  (setq-local buffer-save-without-query t))
	(add-hook 'before-save-hook 'lsp-format-buffer nil t))
  )


(leaf rust-playground
  :ensure t)

(leaf toml-mode
  :ensure t)

(leaf lsp-rust
  :after (lsp)
  :custom
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable . "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints . t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names . nil)
  (lsp-rust-analyzer-display-closure-return-type-hints . t)
  (lsp-rust-analyzer-display-parameter-hints . nil)
  (lsp-rust-analyzer-display-reborrow-hints . nil))

(leaf dap-rust
  :after (dap)
  :config
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
		 :gdbpath "rust-lldb")))

(provide 'k-rust)
