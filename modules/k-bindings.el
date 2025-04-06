;; (leaf isrt
;;   :init
;;   (defvar isrt-mode-map
;; 	(let ((m (make-sparse-keymap)))
;; 	  (define-key m (kbd "M-x") 'transpose-words)
;; 	  (define-key m (kbd "M-t") 'execute-extended-command)
;; 	  (define-key m (kbd "M-T") 'execute-extended-comd-for-buffer)
;; 	  m)
;; 	"Keymap for `isrt-mode`")

;;   (define-minor-mode isrt-mode
;; 	"Swaps C-t and C-x for use with the ISRT keyboard layout"
;; 	:keymap isrt-mode-map)

;;   (define-global-minor-mode isrt-global-mode isrt-mode
;; 	(lambda ()
;;       (unless (minibufferp)
;; 		(isrt-mode 1)))
;; 	:group 'meow
;; 	(if isrt-mode
;; 		(isrt--global-enable)
;;       (isrt--global-disable)))


;;   (defun isrt-mode-swap-C-t-C-x ()
;; 	(interactive)
;; 	(keyboard-translate ?\C-t ?\C-x)
;; 	(keyboard-translate ?\C-x ?\C-t))

;;   (add-hook 'isrt-mode-hook #'isrt-mode-swap-C-t-C-x))

(leaf isrt
  :init
  (key-translate "C-t" "C-x")
  (key-translate "C-x" "C-t")
  :bind
  (("M-x" . transpose-words)
   ("M-t" . execute-extended-command)
   ("M-T" . execute-extended-command-for-buffer)))

(leaf dup-lines
  :bind
  (("C-," . duplicate-dwim)))

(leaf expand-region
  :elpaca t
  :bind (("C-=" . er/expand-region)))

(leaf embrace
  :elpaca t)

(leaf multifiles
  :elpaca t
  :require t
  :bind
  (("C-!" . mf/mirror-region-in-multifile))
  :config
  (defun mf/dired-mirror-marked-files ()
	"Multifile-mirror the files that are marked in a `dired' buffer."
	(interactive)
	(mapcar
	 (lambda (buffer)
       (with-current-buffer buffer
		 (mf/mirror-region-in-multifile (point-min) (point-max))))
	 (mapcar 'find-file-noselect (dired-get-marked-files)))))

(leaf meow
  :elpaca t
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
  (leaf meow :elpaca t :require t)
  (leaf evil :elpaca t :require t)
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
  :elpaca t
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
	 (("v" visual-line-mode)
	  ("d" debug-on-error))))
  :bind ("C-c t" . #'toggle/body))

(leaf back-button
  :elpaca t
  :require t
  :bind (("C-<" . back-button-local-backward)
		 ("C->" . back-button-local-forward)
		 ("C-M-<" . back-button-global-backward)
		 ("C-M->" . back-button-global-forward))
  :config
  ;; bring back rectangle-mark-mode
  (keymap-unset back-button-mode-map "C-x SPC")
  (back-button-mode 1))

(leaf binky
  :elpaca t
  :config
  (binky-mode t)
  (binky-margin-mode t))

(leaf grugru
  :elpaca t)

(leaf shannon-max
  :config
  (add-to-list 'load-path "~/.emacs.d/site-lisp/")
  (require 'shannon-max)
  (setq shannon-max-jar-file
		(expand-file-name "~/.emacs.d/site-lisp/target/emacskeys-0.1.0-SNAPSHOT-standalone.jar"))
  (shannon-max-start-logger))

(leaf undo-tree
  :elpaca t
  :require t
  :config
  (global-undo-tree-mode t)
  :custom
  (undo-tree-auto-save-history . nil))

(leaf recursive-narrow
  :elpaca t
  :require t
  :bind
  ("C-x n w" . recursive-widen)
  ("C-x n n" . recursive-narrow-or-widen-dwim)
  :config
  (defun recursive-narrow-or-widen-dwim ()
	"If the region is active, narrow to that region.
Otherwise, narrow to the current function. If this has no effect,
widen the buffer. You can add more functions to
`recursive-narrow-dwim-functions'."
	(interactive)
	(recursive-narrow-save-position
	 (cond ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
           ((run-hook-with-args-until-success 'recursive-narrow-dwim-functions))
           ((derived-mode-p 'prog-mode) (narrow-to-defun))
           ((derived-mode-p 'org-mode) (org-narrow-to-subtree)))
	 ;; If we don't narrow
	 (progn
       (message "Recursive settings: %d" (length recursive-narrow-settings))
       (recursive-widen)))))

(leaf better-C-a
  :config
  ;; smart beginning-of-line (BOL)
  (defadvice move-beginning-of-line (around smarter-bol activate)
	;; Move to requested line if needed.
	(let ((arg (or (ad-get-arg 0) 1)))
      (when (/= arg 1)
		(forward-line (1- arg))))
	;; Move to indentation on first call, then to actual BOL on second.
	(let ((pos (point)))
	  (back-to-indentation)
	  (when (= pos (point))
		ad-do-it))))

(provide 'k-bindings)
