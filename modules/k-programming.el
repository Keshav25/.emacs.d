(leaf prog-mode
  :hook ((prog-mode . subword-mode)
		 (prog-mode . which-func-mode)
		 (prog-mode . (lambda () (setq-local fill-column 120)))))

(leaf eglot
  :disabled t
  :hook ((rustic-mode . eglot-ensure))
  :bind (:eglot-mode-map
		 ("C-c e r" . #'eglot-rename)
		 ("C-<down-mouse-1>" . #'xref-find-definitions)
		 ("C-S-<down-mouse-1>" . #'xref-find-references)
		 ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown . t))

(leaf consult-eglot
  :after (eglot)
  :disabled t
  :ensure t
  :bind (:eglot-mode-map ("s-t" . #'consult-eglot-symbols)))

(leaf corfu
  :ensure t
  :require t
  :setq
  (completion-cycle-threshold . 3)
  (tab-always-indent . t)
  (corfu-quit-no-match . 'separator)
  (corfu-auto . nil)
  :init
  (global-corfu-mode 1)
  :bind ((:corfu-map
		  ("RET" . newline))))

(leaf corfu-terminal
  :after corfu
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
  :config
  (global-flycheck-mode 1)

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
  ;; (global-aggressive-indent-mode 1)
  )

(leaf rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode t))

(leaf prism
  :ensure t
  :after ef-themes
  :config
  (prism-mode 1)
  (prism-set-colors
	:desaturations '(0) ; do not change---may lower the contrast ratio
	:lightens '(0)      ; same
	:colors (ef-themes-with-colors
              (list fg-main
					magenta
					cyan-cooler
					magenta-cooler
					blue
					magenta-warmer
					cyan-warmer
					red-cooler
					green
					fg-main
					cyan
					yellow
					blue-warmer
					red-warmer
					green-cooler
					yellow-faint))))

(leaf paredit
  :ensure t
  :hook (emacs-lisp-mode-hook . enable-paredit-mode))

(leaf paredit-everywhere
  ;; Original is by Steve Purcell
  :after (paredit)
  :config
  (require 'paredit)

  (defvar paredit-everywhere-mode-map
	(let ((m (make-sparse-keymap)))
      (define-key m (kbd "C-)") 'paredit-forward-slurp-sexp)
      (define-key m (kbd "C-}") 'paredit-forward-barf-sexp)
      (define-key m (kbd "M-(") 'paredit-wrap-round)
      (define-key m (kbd "M-)") 'paredit-close-round-and-newline)
      (define-key m (kbd "M-]") 'paredit-close-square-and-newline)
      (define-key m (kbd "M-\"") 'paredit-meta-doublequote)
      (define-key m (kbd "M-S") 'paredit-split-sexp)
      (define-key m (kbd "M-J") 'paredit-join-sexps)
      (define-key m (kbd "M-s") 'paredit-splice-sexp)
      (define-key m (kbd "M-r") 'paredit-raise-sexp)
      (define-key m (kbd "M-DEL") 'paredit-backward-kill-word)
      (define-key m (kbd "M-d") 'paredit-forward-kill-word)
	  (define-key m (kbd "C-k") 'paredit-kill)
	  (global-set-key "\M-[" #'paredit-wrap-square)
	  (global-set-key "\M-{" #'paredit-wrap-curly)
	  (global-set-key (kbd "C-M-u") #'paredit-backward-up)
	  (global-set-key (kbd "C-M-n") #'paredit-forward-up)
	  ;; This one's surpisingly useful for writing prose.
	  (global-set-key "\M-S"
					  #'paredit-splice-sexp-killing-backward)
	  m)
	"Keymap for `paredit-everywhere-mode'.")

;;;###autoload
  (define-minor-mode paredit-everywhere-mode
	"A cut-down version of paredit which can be used in non-lisp buffers."
	:lighter " Par-"
	:keymap paredit-everywhere-mode-map)

  (defun turn-off-paredit-everywhere-mode ()
	"Disable `paredit-everywhere-mode'."
	(paredit-everywhere-mode 0))

  ;; Disable paredit-everywhere when full paredit is enabled
  (add-hook 'paredit-mode-hook 'turn-off-paredit-everywhere-mode)
  (paredit-everywhere-mode 1)
  )

(leaf paredit-menu
  :ensure t)

(leaf moldable-emacs
  :disabled t
  :init
  (require 'f)
  (if (f-directory-p "~/.emacs.d/site-lisp/moldable-emacs")
      (shell-command "cd ~/.emacs.d/site-lisp/moldable-emacs; git pull;")
    (shell-command "cd ~/.emacs.d/site-lisp/; git clone git@github.com:ag91/moldable-emacs.git"))
  :load-path "~/.emacs.d/site-lisp/moldable-emacs/"
  :bind (("C-c v m" . me-mold)
         ("C-c v f" . me-go-forward)
         ("C-c v b" . me-go-back)
         ("C-c v o" . me-open-at-point)
         ("C-c v d" . me-mold-docs)
         ("C-c v g" . me-goto-mold-source)
         ("C-c v e a" . me-mold-add-last-example))
  :require t
  :config
  (add-to-list 'me-files-with-molds (concat (file-name-directory (symbol-file 'me-mold)) "molds/experiments.el")) ;; TODO this is relevant only if you have private molds
  (me-setup-molds))

(leaf lsp-mode
  :ensure t
  :require t
  :init
  (setq lsp-keymap-prefix "C-c C-y")
  :custom
  (lsp-eldoc-render-all . t)
  (lsp-idle-delay . 0.6)
  (lsp-inlay-hint-enable . t)
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  (lsp-mode. lsp-enable-which-key-integration)
  (python-ts-mode . lsp))

(leaf lsp-ui
  :ensure t
  :custom
  (lsp-ui-peek-always-show . t)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-doc-enable . nil))

(leaf dap-mode
  :ensure t
  :bind (:dap-mode-map
		 ("C-c d b" . dap-breakpoint-toggle)
		 ("C-c d r" . dap-debug-restart)
		 ("C-c d l" . dap-debug-last)
		 ("C-c d d" . dap-debug))
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup))

(leaf direnv
  :ensure t
  :config (direnv-mode))

(leaf tramp
  :require t
  :setq
  (tramp-default-method . "ssh")
  (tramp-verbose . 1)
  (tramp-default-remote-shell . "/bin/bash")
  (tramp-connection-local-default-shell-variables .
												  '((shell-file-name . "/bin/bash")
													(shell-command-switch . "-c")))
  :config
  (connection-local-set-profile-variables 'tramp-connection-local-default-shell-profile
										  '((shell-file-name . "/bin/bash")
											(shell-command-switch . "-c")))

  ;; add gh codespaces ssh method support for tramp editing
  ;; e.g. C-x C-f /ghcs:codespace-name:/path/to/file
  ;; thanks to my coworker Bas for this one
  (let ((ghcs (assoc "ghcs" tramp-methods))
		(ghcs-methods '((tramp-login-program "gh")
						(tramp-login-args (("codespace") ("ssh") ("-c") ("%h")))
						(tramp-remote-shell "/bin/sh")
						(tramp-remote-shell-login ("-l"))
						(tramp-remote-shell-args ("-c")))))
	;; just for debugging the methods
	(if ghcs (setcdr ghcs ghcs-methods)
      (push (cons "ghcs" ghcs-methods) tramp-methods))))

(leaf tramp-lsp
  :after (tramp lsp)
  :disabled t
  :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
  ;; 					:major-modes '(go-mode go-dot-mod-mode)
  ;; 					:language-id "go"
  ;; 					:remote? t
  ;; 					:priority 0
  ;; 					:server-id 'gopls-remote
  ;; 					:completion-in-comments? t
  ;; 					:library-folders-fn #'lsp-go--library-default-directories
  ;; 					:after-open-fn (lambda ()
  ;; 									 ;; https://github.com/golang/tools/commit/b2d8b0336
  ;; 									 (setq-local lsp-completion-filter-on-incomplete nil))))
  )

(leaf restclient
  :ensure t
  :mode  ("\\.restclient$" . restclient-mode))

(leaf yaml-mode
  :ensure t)

(leaf dockerfile-mode
  :ensure t)

(leaf toml-mode
  :ensure t)

(leaf dape
  :ensure t
  :url "https://github.com/svaante/dape")

(leaf apheleia
  :ensure t
  :doc "reformats code on file save"
  :config
  (apheleia-global-mode 1))

(leaf turbo-log
  :quelpa (turbo-log :fetcher github :repo "artawower/turbo-log.el"))

(leaf macrursors
  :disabled t  
  :quelpa (macrusors :fetcher github :repo "corytertel/macrursors")
  :require t
  :config
  (dolist (mode '(corfu-mode goggles-mode beacon-mode))
	(add-hook 'macrursors-pre-finish-hook mode)
	(add-hook 'macrursors-post-finish-hook mode))
  (define-prefix-command 'macrursors-mark-map)
  (global-set-key (kbd "C-c SPC") #'macrursors-select)
  (global-set-key (kbd "C->") #'macrursors-mark-next-instance-of)
  (global-set-key (kbd "C-<") #'macrursors-mark-previous-instance-of)
  (global-set-key (kbd "C-;") 'macrursors-mark-map)
  (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-lines-or-instances)
  (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-lines-or-instances)
  (define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
  (define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
  (define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
  (define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
  (define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
  (define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
  (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines))

(leaf devdocs
  :ensure t
  :bind ("C-h D" . 'devdocs-lookup))

(leaf code-cells
  :ensure t)

(leaf buffer-env
  :ensure t)

(leaf obvious
  :quelpa (obvious :fetcher github :repo "alphapapa/obvious.el"))

(leaf repl-driven-development
  :ensure t
  :require t
  :config
  (repl-driven-development [C-x C-j] java)       
  (repl-driven-development [C-x C-n] javascript) 
  (repl-driven-development [C-x C-p] python)
  (repl-driven-development [C-x C-t] terminal))

(leaf cider
  :ensure t
  :require t
  :config
  (autoload 'cider--make-result-overlay "cider-overlays")
  
  (defun k/eval-overlay (value point)
	"Evaluate Elisp with a Cider Overlay"
	(cider--make-result-overlay (format "%S" value)
	  :where point
	  :duration 'command)
	value)

  (advice-add 'eval-region :around
			  (lambda (func beg end &rest region)
				(k/eval-overlay
				 (apply func beg end region)
				 end)))
  
  (advice-add 'eval-last-sexp :filter-return
			  (lambda (region)
				(k/eval-overlay region (point))))

  (advice-add 'eval-defun :filter-return
			  (lambda (region)
				(k/eval/overlay region
								(save-excursion
								  (end-of-defun)
								  (point))))))

(leaf rainbow-mode
  :ensure t
  :config
  (rainbow-mode 1))

(leaf compile
  :setq
  (byte-compile-ignore-files . t))

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

(leaf elisp-slime-nav
  :ensure t
  :hook ((emacs-lisp-mode . elisp-slime-nav-mode)))

(provide 'k-programming)
