(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
      (add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(use-package auto-package-update
  :defer nil
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
 (use-package async
	:ensure t
	:init
	(dired-async-mode 1))
(use-package gcmh
   :config
   (gcmh-mode 1))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(load-theme 'poet-dark)
(when (member "Iosevka" (font-family-list))
  (set-frame-font "Iosevka" t t)
  (add-to-list 'default-frame-alist '(font . "Iosevka"))
  (set-face-attribute 'default nil :font "Iosevka")
  (set-fontset-font t 'symbol "Iosevka"))

(use-package diminish
	:ensure t)
  ;; (use-package spaceline
  ;;   :ensure t)
  ;;  (use-package powerline
  ;; 	:ensure t
  ;; 	:init
  ;; 	(spaceline-spacemacs-theme)
  ;; 	:hook
  ;; 	('after-init-hook) . 'powerline-reset)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode t)
(setq backward-delete-char-untabify-method 'nil)

(global-prettify-symbols-mode t)
(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

(use-package all-the-icons)
(use-package poet-theme)

(defun split-and-follow-horizontally ()
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))
 (global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

 (defun split-and-follow-vertically ()
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))
 (global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package swiper
       :ensure t
       :bind ("C-s" . 'swiper))

(use-package evil
  :ensure t
  :defer nil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-vsplit-window-right t)
  (setq evil-want-split-window-below t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-mode-list '(dashboard dired buffer))
  (evil-collection-init))

(use-package avy
       :ensure t
       :bind
       ("M-s" . avy-goto-char))

(use-package switch-window
       :ensure t
       :config
       (setq switch-window-input-style 'minibuffer)
       (setq switch-window-increase 4)
       (setq switch-window-threshold 2)
       (setq switch-window-shortcut-style 'qwerty)
       (setq switch-window-qwerty-shortcuts
		 '("a" "s" "d" "f" "j" "k" "l"))
       :bind
       ([remap other-window] . switch-window))

(use-package ido
  :init
  (ido-mode 1)
  :config
  (setq ido-enable-flex-matching nil)
  (setq ido-create-new-buffer 'always)
  (setq ido-everywhere t))

(use-package general
:config
(general-evil-setup t))

(require 'general)

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
"b b" '(ibuffer :which-key "Ibuffer")
"b c" '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
"b k" '(kill-current-buffer :which-key "Kill Current Buffer")
"b n" '(next-buffer :which-key "Next buffer")
"b p" '(previous-buffer :which-key "Previous buffer")
"b B" '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
"b K" '(kill-buffer :which-key "Kill buffer"))

(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
"x f" '(eval-buffer :which-key "Eval emacs-lisp in buffer")
"x d" '(evale-deun :which-key "Eval defun")
"x e"   '(eval-expression :which-key "Eval emacs-lisp expression")
"x l"   '(eval-last-sexp :which-key "Eval last sexression")
"x r"   '(eval-region :which-key "Eval region"))

(use-package all-the-icons-dired)
(use-package dired-open)
(use-package peep-dired)

(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
"f d" '(dired :which-key "Open dired")
"f j" '(dired :which-key "Dired jump to current")
"f p" '(dired :which-key "Peed-dired")) 

(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "."     '(find-file :which-key "Find file")
       "f f"   '(find-file :which-key "Find file")
       "f r"   '(counsel-recentf :which-key "Recent files")
       "f s"   '(save-buffer :which-key "Save file")
       "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
       "f y"   '(dt/show-and-copy-buffer-path :which-key "Yank file path")
       "f C"   '(copy-file :which-key "Copy file")
       "f D"   '(delete-file :which-key "Delete file")
       "f R"   '(rename-file :which-key "Rename file")
       "f S"   '(write-file :which-key "Save file as...")
       "f U"   '(sudo-edit :which-key "Sudo edit file"))

(with-eval-after-load 'dired
  ;;(define-key dired-mode-map (kbd "M-p") 'peep-dired)
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
; This enables arrow keys to select while in ido mode. If you want to
; instead use the default Emacs keybindings, change it to
; "'C-n-and-C-p-only"
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(setq org-src-fontify-natively t)
(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

(use-package org
  :config
  (add-hook 'org-mode-hook
            '(lambda ()
               (visual-line-mode 1))))

(use-package htmlize
  :ensure t)
