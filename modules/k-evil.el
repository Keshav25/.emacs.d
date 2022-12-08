;; Evil
(leaf evil
  :leaf-defer nil
  :pre-setq (evil-want-keybinding . nil)
  :setq
  (evil-want-integration . t)
  (evil-want-keybindings . nil)
  (evil-want-C-u-scroll . t)
  (evil-want-vsplit-window-right . t)
  (evil-want-split-window-below . t)
  :config
  (evil-mode 1))

;; Evil Collection
(leaf evil-collection
  :after evil
  :setq
  (evil-collection-mode-list . '(dashboard dired buffer))
  (evil-want-keybinding . nil)
  :config
  (evil-collection-init))

;; Evil Escape
(leaf evil-escape
	   :config
	   (evil-escape-mode)
	   :setq-default
	   (evil-escape-key-sequence . "jk")
	   (evil-escape-delay . 0.05)
	   (evil-escape-undordered-key-sequence . t))

;; Evil Goggles
(leaf evil-goggles
	   :config
	   (evil-goggles-mode)
	   :setq
	   (evil-goggles-pulse . t)
	   (evil-goggles-duration . 0.001))

;; Evil Surround
(leaf evil-surround
       :config
       (global-evil-surround-mode 1))

;; Evil TextObj Treesitter
(leaf evil-textobj-tree-sitter)

;; Evil God State
(leaf evil-god-state)
;; (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
;; (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
(global-set-key (kbd "ĵ") #'god-local-mode)

(provide 'k-evil)
