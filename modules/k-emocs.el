;; -*- lexical-binding: t -*-

(leaf vertico
  :elpaca t
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  :bind (:vertico-map
		 ("C-'" . vertico-quick-exit)
		 ("C-i" . vertico-insert)
		 ("C-w" . vertico-directory-delete-char)
		 ("C-j" . vertico-exit-input)
		 ("C-c C-c" . vertico-exit-input)
		 ("M-S" . vertico-toggle-sort))
  :config
  (defun hanno/vertico-sort-by-mtime (files)
	"Sort FILES by modification time (newest first)."
	(let ((dir nil))
	  (when (< (minibuffer-prompt-end) (point))
		(setq dir (buffer-substring (minibuffer-prompt-end) (point-max))))
	  (sort files
			(lambda (a b)
			  (let* (
					 (fa (expand-file-name a dir))
					 (fb (expand-file-name b dir))
					 (ta (file-attribute-modification-time (file-attributes fa)))
					 (tb (file-attribute-modification-time (file-attributes fb))))
				(time-less-p tb ta))))))

  (defun vertico-toggle-sort ()
	(interactive)
	(setq-local vertico-sort-override-function
				(and (not vertico-sort-override-function)
					 (lambda (files)
					   (if (and (eq minibuffer-history-variable 'file-name-history)
								(not (eq (car-safe minibuffer-completion-table) 'boundaries)))
						   (hanno/vertico-sort-by-mtime files)
						 (vertico-sort-history-length-alpha files))))
				vertico--input t)))

(leaf orderless
  :elpaca t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard . t))

(leaf marginalia
  :elpaca t
  :config
  (marginalia-mode 1))

(leaf consult
  :require t
  :elpaca t
  :bind (("C-s" . consult-line)
		 ([remap switch-to-buffer] . consult-buffer)
		 ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
		 ("C-y" . consult-yank-pop)
		 ("C-M-y" . yank)
		 ("C-S-s" . consult-ripgrep)
		 ("C-x r b" . consult-bookmark)
		 ("C-x C-r" . consult-recent-file)
		 ("C-c C-h" . consult-org-agenda)
		 ("M-s M-s" . consult-outline)
		 ("M-g e" . consult-compile-error)
		 ("M-g f" . consult-flymake)
		 ("M-g g" . consult-goto-line)
		 ("M-g M-g" . consult-goto-line)
		 ("M-g o" . consult-outline)
		 ("M-g m" . consult-mark)
		 ("M-g k" . consult-global-mark)
		 ("M-g i" . consult-imenu)
		 ("M-g I" . consult-imenu-multi)
		 ("M-g k" . consult-global-mark)
		 ("M-g b" . consult-bookmark)
		 ("M-g M-f" . consult-find)
		 ("M-s d" . consult-find)
		 ("M-s D" . consult-locate)
		 ("M-s g" . consult-grep)
		 ("M-s G" . consult-git-grep)
		 ("M-s r" . consult-ripgrep)
		 ("M-s l" . consult-line)
		 ("M-s L" . consult-line-multi)
		 ("M-s k" . consult-keep-lines)
		 ("M-s u" . consult-focus-lines)
		 ("M-s e" . consult-isearch-history)
		 ("C-c n t" . consult-org-heading)
		 ([f6] . consult-recent-file))
  :config
  (advice-add #'register-preview
			  :override #'consult-register-window)
  (consult-customize consult-theme
					 :preview-key '(:debounce 0.2 any)
					 consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register consult--source-recent-file consult--source-project-recent-file
					 :preview-key '(:debounce 0.4 any)
					 consult-line)

  (consult-customize consult-line
					 :add-history (seq-some #'thing-at-point '(region symbol)))

  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize consult-line-thing-at-point
					 :initial (thing-at-point 'symbol))
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :custom
  ;; (completion-in-region-function . #'consult-completion-in-region)
  (consult-preview-excluded-buffers . '(major-mode . exwm-mode))
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)
  (consult-project-root-function . #'deadgrep--project-root)
  (consult-ripgrep-args . "~/.cargo/bin/rga --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --search-zip")
  (register-preview-delay . 0.5)
  (register-preview-function . #'consult-register-format)
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)
  (consult-narrow-key . "<"))

(leaf consult-dir
  :elpaca t
  :bind (("C-x C-d" . consult-dir)
		 (:minibuffer-local-completion-map
		  ("C-x C-d" . consult-dir)
		  ("C-x C-j" . consult-dir-jump-file))))

(leaf embark
  :elpaca t
  :setq
  (prefix-help-command . 'embark-prefix-help-command)
  :custom
  (embark-quit-after-action . nil))

(leaf embark-consult
  :after (embark consult)
  :elpaca t
  :hook ((embark-collect-mode-hook . consult-preview-at-point-mode)))

(leaf consult-notes
  :elpaca t
  :bind
  ("C-c n f" . consult-notes)
  ("C-c n s" . consult-notes-search-in-all-notes)
  :config
  (consult-notes-denote-mode t))

(leaf consult-omni
  :elpaca (consult-omni :host github :repo "armindarvish/consult-omni" :branch "main" :files (:defaults "sources/*.el"))
  :after consult
  :config
  ;; Load Sources Core code
  (require 'consult-omni-sources)
  ;; Load Embark Actions
  (require 'consult-omni-embark)
  (setq consult-omni-sources-modules-to-load '(consult-omni-wikipedia))
  (consult-omni-sources-load-modules))

(leaf consult-org-roam
  :disabled t
  :after (org-roam)
  :elpaca t
  :config
  (consult-org-roam-mode))

(leaf avy
  :elpaca t
  :custom
  (avy-keys . '(?i ?s ?r ?t ?g ?p ?n ?e ?o))
  (avy-dispatch-alist . `((120 . avy-action-kill-move)
						  (88 . avy-action-kill-stay)
						  (116 . avy-action-teleport)
						  (109 . avy-action-mark)
						  (?c . avy-action-copy)
						  (121 . avy-action-yank)
						  (89 . avy-action-yank-line)
						  (105 . avy-action-ispell)
						  (122 . avy-action-zap-to-char)))
  :bind
  (("M-n" . avy-goto-char)
   ("M-/" . avy-goto-line)))

(leaf avy-embark-collect
  :after (embark avy)
  :elpaca t)

;; TODO: first I need to turn this into a delay then I can bind C-s to isearch
(leaf ace-isearch
  :after (avy consult)
  :elpaca t
  :require t
  :custom
  (ace-isearch-input-length . 6)
  (ace-isearch-function-from-isearch .'ace-isearch-consult-line-from-isearch)
  (ace-isearch-fallback-function . 'ace-isearch-consult-line-from-isearch)
  (ace-isearch-jump-based-on-one-char . nil))

;; TODO: Maybe integrate with ibuffer, consult-buffer, project, or perspective
;; TODO: search the buffer list
(leaf ace-jump-buffer
  :elpaca t
  :bind
  ("C-x C-b" . ace-jump-buffer))

(leaf ace-mc
  :elpaca t)

(leaf ace-popup-menu
  :elpaca t)

(leaf avy-act
  :elpaca t)

;; Which-Key
(leaf which-key
  :elpaca t
  :custom
  (which-key-allow-multiple-replacements . t)
  (which-key-min-display-lines . 1)
  (which-key-idle-delay . 0.125)
  :init
  (which-key-mode 1))

;; Extremely annoying
(leaf corfu
  :disabled t
  :elpaca t
  :require t
  :custom
  (corfu-popupinfo-delay . '(0.5 . 0.5))
  (corfu-min-width . 80)
  (corfu-max-width . corfu-min-width)
  (corfu-count . 14)
  (corfu-scroll-margin . 4)
  (corfu-cycle . t)
  (completion-cycle-threshold . 3)
  (tab-always-indent . t)
  (corfu-quit-no-match . 'separator)
  (corfu-auto . t)
  :init
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  :bind ((:corfu-map
		  ("RET" . #'corfu-insert)
		  ("C-n" . #'corfu-next)
		  ("C-p" . #'corfu-previous)
		  ("<escape>" . #'corfu-quit)
		  ("M-d" . #'corfu-popupinfo-documentation)
		  ("M-l" . #'corfu-popupinfo-location)
		  ;; saw someone match avy-goto-line binding with corfu-quick-complete
		  ("M-/" . corfu-quick-complete))))

(leaf corfu-terminal
  :after corfu
  :elpaca t
  :config
  (unless (display-graphic-p)
	(corfu-terminal-mode +1)))

(leaf kind-icon
  :elpaca t
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
  :elpaca t
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
  (setq-default completion-at-point-functions
				(append (default-value 'completion-at-point-functions)
						(list #'cape-dabbrev #'cape-file #'cape-abbrev #'cape-elisp-block)))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)
  )

(leaf cape-hydra
  :doc "https://jtamagnan.com/posts/à-la-mode-corfu-cape-and-completion-preview%2f"
  :after (cape hydra)
  :config
  (defhydra k/cape
	(:color blue :hint nil)
	"
^Complete^
^--------^
_i_ Completion at Point
_d_abbrev
_f_ile
_h_istory
_p_complete
_e_moji
"
	("i" completion-at-point)
	("p" (lambda () (interactive) (let ((completion-at-point-functions '(pcomplete-completions-at-point t))) (completion-at-point))))
	("d" cape-dabbrev)
	("f" cape-file)
	("h" cape-history)
	("e" cape-emoji))
  )

(leaf embark
  :elpaca t
  :hook ((embark-collect-mode-hook . display-line-numbers-mode))
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
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

(leaf em-cmpl
  :elpaca nil
  :bind (:eshell-cmpl-mode-map
		 ("C-M-i" . nil))
  :hook (eshell-cmpl-mode-hook . my/em-cmpl-mode-hook)
  :config
  (defun my/em-cmpl-mode-hook ()
	(setq-local completion-at-point-functions
				(list #'cape-history #'cape-file #'cape-dabbrev))))


(leaf consult-gh
  :elpaca t)

(leaf placeholder
  ;; TODO Remember to bind EXT key + top row for these bindings on the ZSA Voyager
  :elpaca (placeholder :host github :repo "oantolin/placeholder")
  :bind (("C-c p n" . placeholder-forward)
		 ("C-c p p" . placeholder-backward)
		 ("C-c p i" . placeholder-insert)))

;; (leaf hyperbole
;;   :elpaca t)

(leaf eev
  :disabled t
  :elpaca t)

(leaf rg
  :elpaca t
  :config
  (setq rg-executable (executable-find "rga")))

(leaf posframe
  :elpaca t)

(leaf which-key-posframe
  :after (which-key posframe)
  :elpaca (which-key-posframe :repo "Keshav25/which-key-posframe")
  :config
  (which-key-posframe-mode 1))

(leaf transient-posframe
  :after (magit transient posframe)
  :elpaca (transient-posframe :repo "Keshav25/transient-posframe")
  :custom
  (transient-posframe-parameters . '((alpha . 100) (alpha-background . 90)))
  :config
  (transient-posframe-mode 1))

(leaf flycheck-posframe
  :after (flycheck posframe)
  :elpaca t
  :hook
  (flycheck-mode-hook . flycheck-posframe-mode))

(leaf vertico-posframe
  :after (vertico posframe)
  :require t
  :elpaca t
  :custom
  (vertico-posframe-border-width . 0)
  :config
  (vertico-posframe-mode 1))

(leaf perfect-margin
  :require t
  :elpaca t
  :config
  (perfect-margin-mode 1))

;; (leaf bufler
;; :elpaca t
;; :bind (([remap list-buffers] . bufler)))

(leaf completion-preview
  :config
  (global-completion-preview-mode)
  (push 'org-self-insert-command completion-preview-mode)
  (setq completion-auto-select t
		completion-auto-help 'visible
		completions-format 'one-column
		completions-sort 'historical
		completions-max-height 20
		completion-ignore-case t)
  (setopt completion-preview-sort-function #'corfu-sort-function
		  completion-preview-minimum-symbol-length 2)
  (add-variable-watcher 'corfu-sort-function
						(lambda (_symbol newval operation where)
                          "Match the value of `completion-preview-sort-function' to `corfu-sort-function'.
If `corfu-sort-function' is set buffer-locally, also set
`completion-preview-sort-function' buffer-locally.  Otherwise, change
the default value of `completion-preview-sort-function' accordingly.

This action only applies when the value of `corfu-sort-function' is
set (i.e., OPERATION is \\='set).  This excludes, e.g., let bindings."
                          (when (equal operation 'set)
							(if where
								(with-current-buffer where
                                  (setq-local completion-preview-sort-function newval))
                              (setopt completion-preview-sort-function newval)))))
  :bind (:completion-preview-active-mode-map
		 ("M-f" . #'completion-preview-insert-mode)
		 ("C-M-f" . #'completion-preview-insert-sexp)))

(leaf file-previews
  :config
  (defun k/find-file-preview ()
    (interactive)
    (let ((consult-ripgrep-command "rga --multiline --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep)))
  :bind ("C-x p C-s" . k/find-file-preview))

(leaf ripgrep
  :elpaca t)

(leaf deadgrep
  :elpaca t)

(leaf wgrep
  :elpaca t
  :require t)

(leaf p-search
  :elpaca (p-search :host github :repo "zkry/p-search"))

(leaf affe
  :elpaca t
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
	(setq input (cdr (orderless-compile input)))
	(cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))


;; Prescient disabled, was slowing down emacs a lot for some reason.
;; (leaf prescient
;;   :disabled t
;;   :elpaca t
;;   :require t
;;   :config
;;   (prescient-persist-mode 1))

;; (leaf vertico-prescient
;;   :disabled t
;;   :elpaca t
;;   :after (vertico prescient)
;;   :custom
;;   (vertico-prescient-enable-sorting . t)
;;   (vertico-prescient-override-sorting . nil) ; Don't override `display-sort-function')
;;   ;; Filtering
;;   (vertico-prescient-enable-filtering . nil) ; We want orderless to do the filtering
;;   :config
;;   (vertico-prescient-mode 1))

;; (leaf corfu-prescient
;;   :disabled t
;;   :elpaca t
;;   :after (corfu prescient)
;;   :custom
;;   ;; Sorting
;;   (corfu-prescient-enable-sorting . t)
;;   (corfu-prescient-override-sorting . nil)
;;   ;; Keep the filtering done by orderless
;;   (corfu-prescient-enable-filtering . nil)
;;   :config
;;   (corfu-prescient-mode 1))

(provide 'k-emocs)
;;; k-emocs.el ends here
