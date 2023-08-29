(leaf eglot
  :disabled t
  :hook (
		 (rustic-mode . eglot-ensure))
  :bind (:eglot-mode-map
		 ("C-c e r" . #'eglot-rename)
		 ("C-<down-mouse-1>" . #'xref-find-definitions)
		 ("C-S-<down-mouse-1>" . #'xref-find-references)
		 ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown . t)
  )

(leaf consult-eglot
  :disabled t
  :after (eglot)
  :ensure t
  :bind (:eglot-mode-map ("s-t" . #'consult-eglot-symbols))) 

(leaf corfu
  :ensure t
  :require t
  :setq
  (completion-cycle-threshold . 3)
  (tab-always-indent . 'complete)
  (corfu-quit-no-match . 'separator)
  (corfu-auto . t)
  :init
  (global-corfu-mode 1))

(leaf corfu-terminal
  :ensure t
  :config
  (unless (display-graphic-p)
	(corfu-terminal-mode +1)))

(leaf kind-icon
  :ensure t
  :require t
  :after (corfu)
  :custom
  (kind-icon-default-face . 'corfu-default) ; to compute blended backgrounds correctly
  :hook ('my-completion-ui-mode-hook .
   	    (lambda ()
   	      (setq completion-in-region-function
				(kind-icon-enhance-completion completion-in-region-function))))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf cape
  :ensure t
  :require t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c C-p p" . completion-at-point) ;; capf
         ("C-c C-p t" . complete-tag)        ;; etags
         ("C-c C-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c C-p h" . cape-history)
         ("C-c C-p f" . cape-file)
         ("C-c C-p k" . cape-keyword)
         ("C-c C-p s" . cape-symbol)
         ("C-c C-p a" . cape-abbrev)
         ("C-c C-p l" . cape-line)
         ("C-c C-p w" . cape-dict)
         ("C-c C-p \\" . cape-tex)
         ("C-c C-p _" . cape-tex)
         ("C-c C-p ^" . cape-tex)
         ("C-c C-p &" . cape-sgml)
         ("C-c C-p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(leaf treesit-auto
  :ensure t
  :require t
  :setq
  (treesit-auto-install . 'prompt)
  :config
  (global-treesit-auto-mode))

(leaf envrc
  :ensure t)

(leaf flycheck
  :ensure t
  :init
  (global-flycheck-mode 1)
  :config

  (leaf avy-flycheck
	:after (flycheck avy)
	:ensure t)

  (leaf flycheck-eglot
	:after (flycheck eglot)
	:ensure t)

  (leaf flycheck-guile
	:after (flycheck)
	:ensure t)

  (leaf flycheck-ledger
	:after (flycheck)
	:ensure t)

  (leaf consult-flycheck
	:after (flycheck consult)
	:ensure t)

  (leaf flycheck-inline
	:after (flycheck)
	:ensure t))

(leaf treemacs
  :ensure t
  :setq
  (treemacs-width . 25)
  :hook (treemacs-mode-hook . '(text-scale-adjust -1)))

(leaf aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

;; doesn't work, slows down emacs, and is proprietary
;; (leaf codeium
;;   :quelpa (codeium :fetcher github :repo "Exafunction/codeium.el")
;;   :require t
;;     :init
;;     ;; use globally
;;     (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;     ;; or on a hook
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;     ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions
;;     ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;     ;; an async company-backend is coming soon!

;;     ;; codeium-completion-at-point is autoloaded, but you can
;;     ;; optionally set a timer, which might speed up things as the
;;     ;; codeium local language server takes ~0.2s to start up
;;     ;; (add-hook 'emacs-startup-hook
;;     ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;     ;; :defer t ;; lazy loading, if you want
;;     :config
;;     (setq use-dialog-box nil) ;; do not use popup boxes

;;     ;; if you don't want to use customize to save the api-key
;;     ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;     ;; get codeium status in the modeline
;;     (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;     (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;     ;; alternatively for a more extensive mode-line
;;     ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;     ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;     (setq codeium-api-enabled
;;         (lambda (api)
;;             (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;     ;; you can also set a config for a single buffer like this:
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local codeium/editor_options/tab_size 4)))

;;     ;; You can overwrite all the codeium configs!
;;     ;; for example, we recommend limiting the string sent to codeium for better performance
;;     (defun my-codeium/document/text ()
;;         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;     ;; if you change the text, you should also change the cursor_offset
;;     ;; warning: this is measured by UTF-8 encoded bytes
;;     (defun my-codeium/document/cursor_offset ()
;;         (codeium-utf8-byte-length
;;             (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;     (setq codeium/document/text 'my-codeium/document/text)
;;     (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(leaf lsp-mode
  :ensure t
  :custom
  (lsp-eldoc-render-all . t)
  (lsp-idle-delay . 0.6)
  (lsp-inlay-hint-enable . t)
  :hook
  (lsp-mode-hook . lsp-ui-mode))

(leaf lsp-ui
  :ensure t
  :custom
  (lsp-ui-peek-always-show . t)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-doc-enable . nil))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :hook
  (prog-mode-hook . yas-minor-mode)
  (text-mode-hook . yas-minor-mode))

  (use-package dap-mode
    :ensure t
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup))

(provide 'k-programming)
