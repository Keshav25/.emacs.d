(leaf evil
  :ensure t
  :after (key-chord)
  :leaf-defer nil
  :setq
  (evil-want-integration . t)
  (evil-want-keybinding . nil)
  (evil-want-fine-undo . t)
  (evil-respect-visual-line-mode . nil)
  (evil-want-C-u-scroll . t)
  (evil-want-vsplit-window-right . t)
  (evil-want-split-window-below . t)
  :config
  (evil-mode 1)
  (defun k-consult-line-search-forward ()
	(interactive)
	(search-forward (car consult--line-history)))
  (defun k-consult-line-search-backward ()
	(interactive)
	(search-backward (car consult--line-history)))
  (add-to-list 'evil-insert-state-modes 'text-mode)
  ;; https://github.com/minad/consult/issues/318
  :bind (:evil-normal-state-map
		 ("n" . 'k-consult-line-search-forward)
		 ("N" . 'k-consult-line-search-backward)
		 ("F" . 'evil-find-char)
		 ("ge" . 'evil-end-of-line)
		 ("ga" . 'evil-first-non-blank)
		 ("-" . 'dired)
		 ;;unbind open and close folds
		 ("zo" . 'nil)
		 ("zc" . 'nil)
		 ("C-S-u" . 'scroll-other-window-down)
		 ("C-S-d" . 'scroll-other-window)
		 ;;unbind q
		 ("q" . 'nil))
  (:evil-insert-state-map
   ("C-o" . evil-force-normal-state))
  (:evil-motion-state-map
   ("ge" . 'evil-end-of-line)
   ;;unbind a few emacs keys
   ("C-y" . 'nil))
  :chord
  (:evil-insert-state-map
   ("jj" . evil-normal-state)))

;; Evil Collection
(leaf evil-collection
  :ensure t
  :after evil
  :setq
  (evil-collection-mode-list . '(dashboard dired buffer))
  :config
  (evil-collection-init))

;; Honestly might be better to make my own package
(leaf evil-extra-operator
  :ensure t
  :bind (:evil-motion-state-map
		 ("gs" . evil-operator-google-translate))
  :config
  (require 'evil-extra-operator)
  (evil-define-operator evil-operator-google-search (beg end type)
	"Evil operator for google search."
	:move-point nil
	(interactive "<R>")
	(browse-url
	 (concat "http://www.startpage.com/search?q="
			 (url-hexify-string
			  (.eeo/make-url-args beg end type)))))
  (global-evil-extra-operator-mode 1))

(leaf evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

;; Evil Escape
(leaf evil-escape
  :ensure t
  :config
  (evil-escape-mode)
  :setq-default
  (evil-escape-key-sequence . "jk")
  (evil-escape-delay . 0.05)
  (evil-escape-undordered-key-sequence . t))


(leaf evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  :setq
  (evil-goggles-pulse . t)
  (evil-goggles-duration . 0.001))

;; Evil Surround
(leaf evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(leaf evil-nerd-commenter
  :ensure t
  :init
  (evilnc-default-hotkeys t) ;;only enables for emacs state
  )

;; ;; Evil TextObj Treesitter
;; ;; Seems to not work with the Emacs 29 treesitter feature yet
;; (leaf evil-textobj-tree-sitter :ensure t
;;   :bind (:evil-outer-text-objects-map
;; 		 ("f" . (evil-textobj-tree-sitter-get-textobj "function.outer"))))
;; ;; `M-x combobulate' (or `C-c o o') to start using Combobulate
;; (leaf treesit
;;   :ensure t
;;   :preface
;;   (defun mp-setup-install-grammars ()
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              '((css "https://github.com/tree-sitter/tree-sitter-css")
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;                (python "https://github.com/tree-sitter/tree-sitter-python")
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;                (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       ;; Only install `grammar' if we don't already have it
;;       ;; installed. However, if you want to *update* a grammar then
;;       ;; this obviously prevents that from happening.
;;       (unless (treesit-language-available-p (car grammar))
;;         (treesit-install-language-grammar (car grammar)))))

;;   ;; Optional, but recommended. Tree-sitter enabled major modes are
;;   ;; distinct from their ordinary counterparts.
;;   ;;
;;   ;; You can remap major modes with `major-mode-remap-alist'. Note
;;   ;; that this does *not* extend to hooks! Make sure you migrate them
;;   ;; also
;;   (dolist (mapping '((python-mode . python-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (typescript-mode . tsx-ts-mode)
;;                      (js-mode . js-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (yaml-mode . yaml-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))

;;   :config
;;   (mp-setup-install-grammars)
;;   ;; Do not forget to customize Combobulate to your liking:
;;   ;;
;;   ;;  M-x customize-group RET combobulate RET
;;   ;;
;;   (leaf combobulate
;; 	:ensure t
;;     ;; Optional, but recommended.
;;     ;;
;;     ;; You can manually enable Combobulate with `M-x
;;     ;; combobulate-mode'.
;;     :hook ((python-ts-mode . combobulate-mode)
;;            (js-ts-mode . combobulate-mode)
;;            (css-ts-mode . combobulate-mode)
;;            (yaml-ts-mode . combobulate-mode)
;;            (typescript-ts-mode . combobulate-mode)
;;            (tsx-ts-mode . combobulate-mode))))
;; Evil God State

(leaf evil-god-state
  :ensure t
  :bind (:evil-normal-state-map
		 (",". evil-execute-in-god-state)
		 ((kbd "ĵ") . god-local-mode))
  (:evil-god-state-map
   ([escape] . evil-god-state-bail)))

;; Needs better evil integration and try undo-in-region
(leaf vundo :emacs>= 28.1 :ensure t
  :bind
  (:evil-normal-state-map
   ("C-M-u" . 'vundo)))

;; don't really like multicursors, maybe in the future
;; (leaf evil-mc
;; :ensure t
;; :init
;; (global-evil-mc-mode 1))

;; (leaf undo-tree
;;   :ensure t
;;   :custom
;;   (evil-undo-system . 'undo-tree)
;;   :config
;;   (global-undo-tree-mode 1))

(leaf evil-embrace
  :ensure t)

(leaf evil-textobj-tree-sitter
  :require t
  :ensure t
  :custom
  (tree-sitter-debug-jump-buttons . t)
  (tree-sitter-debug-highlight-jump-region . t)
  :config
  (defun +tree-sitter-goto-textobj (group &optional previous end query)
	"Thin wrapper that returns the symbol of a named function, used in keybindings."
	(let ((sym (intern (format "+goto%s%s-%s" (if previous "-previous" "") (if end "-end" "") group))))
      (fset sym (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj group previous end query)))
      sym))
  (defun +tree-sitter-get-textobj (group &optional query)
	"A wrapper around `evil-textobj-tree-sitter-get-textobj' to
prevent eager expansion."
	(eval `(evil-textobj-tree-sitter-get-textobj ,group ,query))))

(leaf evil-textobj-tree-sitter-bindings
  :after (evil-textobj-tree-sitter)
  :bind
  (:+tree-sitter-inner-text-objects-map
   ("A" . (+tree-sitter-get-textobj '("parameter.inner" "call.inner")))
   ("f" . (+tree-sitter-get-textobj "function.inner"))
   ("F" . (+tree-sitter-get-textobj "call.inner"))
   ("C" . (+tree-sitter-get-textobj "class.inner"))
   ("v" . (+tree-sitter-get-textobj "conditional.inner"))
   ("l" . (+tree-sitter-get-textobj "loop.inner")))
  (:+tree-sitter-outer-text-objects-map
   ("A" . (+tree-sitter-get-textobj '("parameter.outer" "call.outer")))
   ("f" . (+tree-sitter-get-textobj "function.outer"))
   ("F" . (+tree-sitter-get-textobj "call.outer"))
   ("C" . (+tree-sitter-get-textobj "class.outer"))
   ("v" . (+tree-sitter-get-textobj "conditional.outer"))
   ("l" . (+tree-sitter-get-textobj "loop.outer")))
  (:+tree-sitter-goto-previous-map
   ("a" . (+tree-sitter-goto-textobj "parameter.outer" t))
   ("f" . (+tree-sitter-goto-textobj "function.outer" t))
   ("F" . (+tree-sitter-goto-textobj "call.outer" t))
   ("C" . (+tree-sitter-goto-textobj "class.outer" t))
   ("c" . (+tree-sitter-goto-textobj "comment.outer" t))
   ("v" . (+tree-sitter-goto-textobj "conditional.outer" t))
   ("l" . (+tree-sitter-goto-textobj "loop.outer" t)))
  (:+tree-sitter-goto-next-map
   ("a" . (+tree-sitter-goto-textobj "parameter.outer"))
   ("f" . (+tree-sitter-goto-textobj "function.outer"))
   ("F" . (+tree-sitter-goto-textobj "call.outer"))
   ("C" . (+tree-sitter-goto-textobj "class.outer"))
   ("c" . (+tree-sitter-goto-textobj "comment.outer"))
   ("v" . (+tree-sitter-goto-textobj "conditional.outer"))
   ("l" . (+tree-sitter-goto-textobj "loop.outer")))
  :config
  (evil-define-key '(visual operator) 'tree-sitter-mode
	"i" +tree-sitter-inner-text-objects-map
	"a" +tree-sitter-outer-text-objects-map)
  (evil-define-key ('normal 'tree-sitter-mode)
	"[g" +tree-sitter-goto-previous-map
	"]g" +tree-sitter-goto-next-map))

(provide 'k-evil)
