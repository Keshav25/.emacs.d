(leaf isrt
  :init
  (keyboard-translate ?\C-t ?\C-x)
  (keyboard-translate ?\C-x ?\C-t)
  :bind
  (("M-x" . transpose-words)
   ("M-t" . execute-extended-command)
   ("M-T" . execute-extended-command-for-buffer)))

(leaf dup-lines
  :bind
  (("C-," . duplicate-dwim)))

(leaf expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(leaf embrace
  :ensure t)

(leaf multifiles
  :ensure t
  :require t
  :bind
  (("C-!" . mf/mirror-region-in-multifile)))

(leaf meow
  :ensure t
  :require t
  :config
  (defun meow-setup ()
	(setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)

	(meow-motion-overwrite-define-key
	 ;; Use e to move up, n to move down.
	 ;; Since special modes usually use n to move down, we only overwrite e here.
	 '("e" . meow-prev)
	 '("<escape>" . ignore))

	(meow-leader-define-key
	 '("?" . meow-cheatsheet)
	 ;; To execute the originally e in MOTION state, use SPC e.
	 ;; '("e" . "H-e")
	 '("1" . meow-digit-argument)
	 '("2" . meow-digit-argument)
	 '("3" . meow-digit-argument)
	 '("4" . meow-digit-argument)
	 '("5" . meow-digit-argument)
	 '("6" . meow-digit-argument)
	 '("7" . meow-digit-argument)
	 '("8" . meow-digit-argument)
	 '("9" . meow-digit-argument)
	 '("0" . meow-digit-argument))
	
	(meow-normal-define-key
	 '("0" . meow-expand-0)
	 '("1" . meow-expand-1)
	 '("2" . meow-expand-2)
	 '("3" . meow-expand-3)
	 '("4" . meow-expand-4)
	 '("5" . meow-expand-5)
	 '("6" . meow-expand-6)
	 '("7" . meow-expand-7)
	 '("8" . meow-expand-8)
	 '("9" . meow-expand-9)
	 '("-" . negative-argument)
	 '(";" . meow-reverse)
	 '("," . meow-inner-of-thing)
	 '("." . meow-bounds-of-thing)
	 '("[" . meow-beginning-of-thing)
	 '("]" . meow-end-of-thing)
	 '("/" . meow-visit)
	 '("a" . meow-append)
	 '("A" . meow-open-below)
	 '("b" . meow-back-word)
	 '("B" . meow-back-symbol)
	 '("c" . meow-change)
	 '("d" . meow-delete)
	 '("e" . meow-prev)
	 '("E" . meow-prev-expand)
	 '("f" . meow-find)
	 '("g" . meow-cancel-selection)
	 '("G" . meow-grab)
	 '("h" . meow-left)
	 '("H" . meow-left-expand)
	 '("i" . meow-right)
	 '("I" . meow-right-expand)
	 '("j" . meow-join)
	 '("k" . meow-kill)
	 '("l" . meow-line)
	 '("L" . meow-goto-line)
	 '("m" . meow-mark-word)
	 '("M" . meow-mark-symbol)
	 '("n" . meow-next)
	 '("N" . meow-next-expand)
	 '("o" . meow-block)
	 '("O" . meow-to-block)
	 '("p" . meow-yank)
	 '("q" . meow-quit)
	 '("r" . meow-replace)
	 '("s" . meow-insert)
	 '("S" . meow-open-above)
	 '("t" . meow-till)
	 '("u" . meow-undo)
	 '("U" . meow-undo-in-selection)
	 '("v" . meow-search)
	 '("w" . meow-next-word)
	 '("W" . meow-next-symbol)
	 '("x" . meow-delete)
	 '("X" . meow-backward-delete)
	 '("y" . meow-save)
	 '("z" . meow-pop-selection)
	 '("'" . repeat)
	 '("<backspace>" . meow-left)
	 '("<escape>" . ignore)))
  (meow-setup))

(leaf meow-for-commands
  :after (hydra)
  :init
  (leaf meow :ensure t :require t)
  (leaf evil :ensure t :require t)
  (pretty-hydra-define text-objects
	(:quit-key "C-g" :title "motions")
	("direction"
	 (("-"  negative-argument)
	  (";"  meow-reverse)
	  ("i"  meow-inner-of-thing)
	  ("."  meow-bounds-of-thing)
	  ("["  meow-beginning-of-thing)
	  ("]"  meow-end-of-thing)
	  ("'"  repeat))
	 "cmd"
	 (("l"  meow-line)
	  ("o"  meow-block)
	  ("O"  meow-to-block)
	  ("z"  meow-pop-selection)
	  ("," duplicate-dwim)
	  ("d" evil-delete))))
  :bind ("C-," . #'text-objects/body))

(leaf meow-tree-sitter
  :after (meow)
  :ensure t
  :require t
  :config
  (meow-tree-sitter-register-defaults))

(leaf fill-or-unfill
  :config
  (defun k/fill-or-unfill ()
	"Like `fill-paragraph', but unfill if used twice."
	(interactive)
	(let ((fill-column
           (if (eq last-command 'k/fill-or-unfill)
               (progn (setq this-command nil)
                      (point-max))
			 fill-column)))
      (call-interactively #'fill-paragraph)))

  (global-set-key [remap fill-paragraph]
                  #'k/fill-or-unfill))

(leaf toggle-hydra
  :after (hydra)
  :init
  (pretty-hydra-define toggle
	(:quit-key "C-g")
	("toggle"
	 (("v" visual-line-mode))))
  :bind ("C-c t" . #'toggle/body))

(leaf back-button
  :ensure t
  :require t
  :bind (("C-<" . back-button-local-backward)
		 ("C->" . back-button-local-forward)
		 ("C-M-<" . back-button-global-backward)
		 ("C-M->" . back-button-global-forward))
  :config
  (back-button-mode 1))

(leaf binky
  :ensure t
  :config
  (binky-mode t)
  (binky-margin-mode t))

(leaf grugru
  :ensure t)

(provide 'k-bindings)
