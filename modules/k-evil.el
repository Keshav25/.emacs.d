;; Evil
;; for some reason can't get pre-setq to work
(setq evil-want-keybinding nil)

(leaf evil
  :ensure t
  :leaf-defer nil
  :setq
  (evil-want-integration . t)
  (evil-want-keybindings . nil)
  (evil-want-C-u-scroll . t)
  (evil-want-vsplit-window-right . t)
  (evil-want-split-window-below . t)
  :config
  (evil-mode 1)
  ;; https://github.com/minad/consult/issues/318
  :bind (:evil-normal-state-map
		 ("n" . (lambda () (interactive) (search-forward (car consult--line-history))))
		 ("N" . (lambda () (interactive) (search-backward (car consult--line-history))))
		 ("F" . 'evil-find-char)))

;; Evil Collection
(leaf evil-collection
  :ensure t
  :after evil
  :setq
  (evil-collection-mode-list . '(dashboard dired buffer))
  (evil-want-keybinding . nil)
  :config
  (evil-collection-init))

;; Evil Escape
(leaf evil-escape
  :ensure t
	   :config
	   (evil-escape-mode)
	   :setq-default
	   (evil-escape-key-sequence . "jk")
	   (evil-escape-delay . 0.05)
	   (evil-escape-undordered-key-sequence . t))

;; Evil Goggles
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

;; Evil TextObj Treesitter
;; Seems to not work with the Emacs 29 treesitter feature yet
;; (leaf evil-textobj-tree-sitter :ensure t
;;   :bind (:evil-outer-text-objects-map
;; 		 ("f" . (evil-textobj-tree-sitter-get-textobj "function.outer"))))
;; `M-x combobulate' (or `C-c o o') to start using Combobulate
;; (leaf treesit
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
		 (",". 'evil-execute-in-god-state)
		 ((kbd "ĵ") . #'god-local-mode))
        (:evil-god-state-map
         ([escape] . 'evil-god-state-bail)))

(leaf vundo :emacs>= 28.1 :ensure t)

(provide 'k-evil)