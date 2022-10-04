;; Garbage Collection
(setq gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.6)
      (add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Native Comp Errors
(setq comp-deferred-compilation nil
      native-comp-deferred-compilation nil
      comp-async-report-warnings-errors nil)

(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil)

;; User Information
(setq user-full-name "Keshav Italia"
      user-mail-address "keshavitalia0@gmail.com")

;; Language Environment
(set-language-environment "UTF-8")

;; global modes
(global-so-long-mode 1)
(global-hl-line-mode 0)

;; Tab Width
(setq-default tab-width 4)

;; Advice Warnings
(setq ad-redefinition-action 'accept)

;; Ignore Startup Echo Message
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Determine OS
(require 'subr-x)
(defconst islinux (eq system-type 'gnu/linux))
(defconst iswindows (eq system-type '(cygwin windows-nt ms-dos)))
(defconst istermux (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))
(defconst isguix nil)
(setq enabled-eaf nil)

;; Get Home Directory if Windows
(when (and iswindows (null (getenv-internal "HOME")))
  (setenv "HOME" (getenv "USERPROFILE"))
  (setq abbreviated-home-dir nil))

;; Determine if Native Comp
(defconst isnativecomp (if (fboundp 'native-comp-available-p)
                         (native-comp-available-p)))

;; Determine if EXWM should be enabled
(setq isexwm (and (not istermux)
		  (eq window-system 'x)
		  (seq-contains command-line-args "--use-exwm")))

;; Bidirectional Text
(setq-default bidi-display-reordering 'left-to-right
               bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Repositories


;; Leaf
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")
		       ("elpa" . "https://elpa.gnu.org/packages/")))

  (when istermux
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
  
  (unless (or (package-installed-p 'leaf) isguix)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :init
    (leaf hydra)
    (leaf el-get)
    (leaf blackout)

    :config
    (leaf-keywords-init)))

(unless isguix
  (setq leaf-defaults (leaf-append-defaults '(:ensure t))))

;; leaf-convert
(leaf leaf-convert)

(leaf kaolin-themes :config
  (load-theme 'kaolin-ocean t))


;; Auto Package Update
(leaf auto-package-update
  :setq
  (auto-package-update-delete-old-versions . t)
  (auto-package-update-hide-results . t)
  :config (auto-package-update-maybe))

;; Async
(leaf async
  :init
  (dired-async-mode 1))

;; Keep .emacs.d clean
(setq user-emacs-directory (expand-file-name "~/.cache/emacs")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(leaf no-littering)

(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-$s.el" (user-uid)) temporary-file-directory)))

;; Gcmh
(leaf gcmh
  :config
  (gcmh-mode 1))

;; Visual Settings
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Disable Graphical Menus
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Completion Framework
(leaf vertico)
(leaf orderless
      :config
      (setq completion-styles '(orderless)))

;; Evil God State
(leaf evil-god-state
      :config
      (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
      (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

;; Window Divider Mode
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode)

;; Split Thresholds
(setq split-width-threshold 160
      split-height-threshold nil)

;; Recursive Minibuffers
(setq enable-recursive-minibuffers t)

;; Doom Themes
(leaf doom-themes)

;; All the icons
(leaf all-the-icons)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha `(90,90))
(add-to-list 'default-frame-alist `(alpha . (90, 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Line Numbers
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-width 3)

;; General Text Editing Preferences
(show-paren-mode)
(global-prettify-symbols-mode t)
(setq electric-pair-pairs '(
							  (?\{ . ?\})
							  (?\( . ?\))
							  (?\[ . ?\])
							  (?\" . ?\")
							  ))
(electric-pair-mode t)

;; Display Time
(setq display-time-mode 1)
(setq display-time-day-and-date 1)

;; Cursor Settings
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizon 2)
(setq x-stretch-cursor nil)

;; Doom Modeline
(leaf diminish)


(unless istermux
  (leaf solaire-mode)
  (mmap
     ";" 'evil-ex
     ":" 'avy-next)

;; Split and Follow Functions
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

;; Eshell mycat command
;; To know if I need to print as image or I need to default to the default eshell/cat
(defun my-is-imagep (filename)
  (let ((extension (file-name-extension filename))
        (image-extensions '("png" "jpg" "bmp")))
    (member extension image-extensions)))

;; Creates a space with display properties. Feel free to change `eshell/println` to `insert` and use it in a normal emacs buffer, it will inline the path given in `file`.
(defun my-print-image-eshell (file)
  (eshell/printnl (propertize " " 'display (create-image file))))


;; If image, use `my-print-image-eshell`. Otherwise, just use `eshell/cat`.
(defun eshell/mycat (&rest args)
  (interactive)
  (mapc (lambda (arg)
          (if (my-is-imagep arg)
              (my-print-image-eshell arg)
            (eshell/cat arg)
            )
          ) (-flatten args))
  nil)

;; Which-Key
(leaf which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

;; Swiper
(leaf swiper
	:bind ("C-s" . 'swiper))

;; Avy
(leaf avy)

;; Switch Window
(leaf switch-window
  :setq
  (switch-window-input-style . 'minibuffer)
  (switch-window-increase . 4)
  (switch-window-threshold . 2)
  (switch-window-shortcut-style . 'qwerty)
  (switch-window-qwerty-shortcuts .
				  '("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

;; Ido
(leaf ido
       :init
       (ido-mode 1)
       :setq
       (ido-enable-flex-matching . nil)
       (ido-create-new-buffer . 'always)
       (ido-everywhere . t))

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

;; Evil Org
(leaf evil-org
	 :after org
	 :hook (org-mode . (lambda () (evil-org-mode)))
	 :config
	 (require 'evil-org-agenda)
	 (evil-org-agenda-set-keys)
	 :setq (evil-want-C-i-jump . nil))

;; Dired
(leaf all-the-icons-dired)
(leaf dired-open)
(leaf peep-dired)

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

;; General
(leaf general
  :config
  (general-evil-setup t))

(require 'general)

;; Enhance Evil Mode
(mmap
  "/" 'swiper
  "f" 'avy-goto-char)

;; SPC h for help
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC h"
  "t" '(load-theme :which-key "load-theme"))

;; SPC x for executing emacs lisp
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC x"
       "f" '(eval-buffer :which-key "Eval emacs-lisp in buffer")
       "d" '(evale-deun :which-key "Eval defun")
       "e"   '(eval-expression :which-key "Eval emacs-lisp expression")
       "l"   '(eval-last-sexp :which-key "Eval last sexression")
       "r"   '(eval-region :which-key "Eval region")
       "p" '(pp-eval-last-sexp :which-key "Pretty Eval"))

;; SPC b for buffer navigation
;; More will be done here once I figure out which completion packages to use
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC b"
  "b" '(switch-to-buffer :which-key "switch buffer")
  ;; S for sudo edit
  ;; k previous buffer
  ;; j next buffer
  ;; k kill buffer
  ;; n empty buffer
  ;; o for kill all buffers
  ;; s save buffer
  ;; x pop scratch buffer
  ;; z burry buffer
  )

;; Org-Mode navigation
(general-define-key
     :keymaps 'org-mode-map
     "M-n" 'org-next-visible-heading
     "M-p" 'org-previous-visible-heading)

;; Org-Mode Hook
(add-hook 'org-mode-hook 'toggle-truncate-lines)

;; Window Navigation (Mainly For EXWM)
(general-define-key
 "s-h" 'windmove-left
 "s-l" 'windmove-right
 "s-j" 'windmove-down
 "s-k" 'windmove-up)

;; SPC w for Window Navigation
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "w h" '(windmove-left :which-key "Move to Left Window")
  "w j" '(windmove-down :which-key "Move Down a Window")
  "w k" '(windmove-up :which-key "Move Up a Window")
  "w l" '(windmove-right :which-key "Move to Right Window"))

;; SPC f for Files
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
"f d" '(dired :which-key "Open dired")
"f j" '(dired :which-key "Dired jump to current")
"f p" '(dired :which-key "Peed-dired")
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

;; SPC n for Org-Roam
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC n"
  "l" '(org-roam :which-key "org-roam")
  "i" '(org-roam-node-insert :which-key "org-roam-node-insert")
  "b" '(org-roam-switch-to-buffer :which-key "org-roam-switch-to-buffer")
  "f" '(org-roam-find-file :which-key "org-roam-find-file")
  "g" '(org-roam-show-graph :which-key "org-roam-show-graph")
  "c" '(org-roam-capture :which-key "org-roam-capture")
  "j" '(org-roam-dailies-capture-today :which-key "org-roam-dailies-capture-today"))

;; SPC o for Opening various application
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC o"
  "e" '(eshell :which-key "eshell")
  "b" '(eaf-open-browser-other-window :which-key "open url")
  "s" '(eaf-search-it :which-key "search browser"))

(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC q"
  "q" '(save-buffers-kill-terminal :which-key "save-buffers-kill-terminal"))

;; some other keybindings that I want but have yet to implement
;;  c code
;;         d jump to def
;;         D jump to ref
;;         e evaluate buffer
;;         E evaluate and replace
;;         b build
;;         r repl
;;         x list errors
;;     f file
;;         . find file
;;         / find file in project
;;         > sudo find file
;;         ? find file from here
;;         E Browse emacs.d
;;         P browse private config
;;         R recent project files
;;         a find other file
;;         c open project editor config
;;         d find dir
;;         e find file in emacs.d
;;         p find file in private config
;;         r recent files
;;         y yank filename
;;     g git
;;         c magit commit
;;         C magit clone
;;         G list gists
;;         L list reps
;;         P magic pull popup
;;         R git revert
;;         S git stage
;;         U git unstange hunk
;;         [ previous
;;         ] next
;;         b magic blame
;;         d magic dispatch
;;         f magic find
;;         g magit status
;;         i init repo
;;         l magit buffer log
;;         p push popup
;;         r git revert hunk
;;         s git status
;;         t git time matchine
;;     o open
;;         M mail
;;         N neotree
;;         O reveal proj finder
;;         b browser
;;         d debugger
;;         n neotree
;;         o reveal in finder
;;         r repl
;;         t terminal
;;     p project
;;         ! run cmd in project root
;;         . browse
;;         / find in project
;;         c compile project
;;         o find other file
;;         p switch project
;;         r recent project files
;;         t list project tasks
;;         x invalidate cache
;;     q quit
;;         q save and quit
;;         Q quit
;;     r remote
;;         . browse remote files
;;         > detect remote changes
;;         D diff local and remote
;;         U upload local
;;         d download remote
;;         u upload local
;;     s snippets
;;         S find snippet
;;         i insert snippet
;;         n new snippet
;;         s find snippet for mode
;;     t toggle
;;         F frame fullscreen
;;         I indente
;;         b big mode
;;         f flycheck
;;         g evil goggles
;;         h impatient modei indet guides
;;         l line numbers
;;         p org-tree-slide-mode
;;         s flyspell
;;     w window
;;         + increase height
;;         - descr height
;;         < dec width
;;         = balance windows
;;         > incr width
;;         H move left
;;         J move down
;;         K move up
;;         L move right
;;         R rotate up
;;         S split
;;         W prev
;;         _ set height
;;         b bottom right
;;         c close window
;;         h left
;;         j down
;;         k up
;;         l right
;;         n new
;;         o enlargen
;;         p mru
;;         q quit
;;         r rotate down
;;         s split
;;         t top left
;;         u winner undo
;;         v vsplit
;;         w next
;;         | set width

;; Org-Mode
(setq org-directory "~/org/")

(leaf org
  :config
  (setq org-ellipsis " ▾"))

;; To fix in the future
;; (use-package org-krita
;;   :ensure t
;;   :quelpa (org-krita :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources"))
;;   :config
;;   (add-hook 'org-mode-hook 'org-krita-mode))

;; Org-Ehtml
(leaf org-ehtml
  :setq
  (org-ehtml-docroot . '(expand-file-name "~/org/roam"))
  (org-ehtml-everything-editable . t))

(require 'org-ehtml)

(defun start-ehtml ()
  (interactive)
  (ws-start org-ethml-handler 8888))

;; Org-Roam
(leaf org-roam
  :config
  (let
      ((custom--inhibit-theme-enable nil))
    (unless
	(memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes
	    (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
				'(org-roam-directory
				  (file-truename "~/org/roam")
				  nil nil "Customized with use-package org-roam")
				'(org-roam-completon-everywhere t nil nil "Customized with use-package org-roam")
				'(org-roam-completion-system 'default nil nil "Customized with use-package org-roam")
				'(org-roam-dailies-directory "~/org/roam/daily" nil nil "Customized with use-package org-roam")))
  (with-eval-after-load 'org-roam
    (org-roam-db-autosync-mode)
    (require 'org-roam-protocol)))

;; Disable Warning About Org-Roamv2
(setq org-roam-v2-ack t)

;; Org Roam UI
;; Commented due to the fact that I have not yet installed straigt.el
;; (use-package org-roam-ui
;;   :straight
;;     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;     :after org-roam
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;; Org-Bullets
(leaf org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Elfeed
(leaf elfeed :bind
  (("C-x w" . elfeed))
  :config
  (with-eval-after-load 'elfeed
    (let
	((leaf--load-file-name "/home/haresh/.emacs.d/init.el"))
      (setq elfeed-db-directory "~/.emacs.d/elfeed")
      (setq elfeed-show-entry-switch 'display-buffer))))

;; Elfeed-Web
(leaf elfeed-web)

;; Elfeed-Org
(leaf elfeed-org
  :config
  (with-eval-after-load 'elfeed-org
    (setq elfeed-show-entry-switch 'display-buffer)
    (setq rmh-elfeed-org-files
	  (list "~/org/elfeed.org"))))

;; Vterm
(leaf vterm)

;; Counsel
(leaf counsel)

(when isexwm
(leaf exwm)
(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "eDP1"))
 (add-hook 'exwm-randr-screen-change-hook
           (lambda ()
             (start-process-shell-command
              "xrandr" nil "xrandr --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal")))
  (exwm-randr-enable)
 (setq exwm-input-global-keys
       `(
         ([?\s-r] . exwm-reset)
         ([?\s-h] . windmove-left)
         ([?\s-l] . windmove-right)
         ([?\s-k] . windmove-up)
         ([?\s-j] . windmove-down)
        ([?\s-o] . counsel-linux-app)
        ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
        ([?\s-w] . exwm-workspace-switch)
        ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

(defun efs/set-wallpaper ()
   (interactive)
   (start-process-shell-command
       "feh" nil  "feh --bg-scale ~/Pictures/wallpaper.jpg"))

(defun efs/exwm-init-hook ()
  (exwm-workspace-switch-create 1))

   (display-battery-mode 1)
   (setq display-time-day-and-date t)
   (display-time-mode 1)

   (defun efs/run-in-background (command)
   (let ((command-parts (split-string command "[ ]+")))
     (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

   (defun efs/exwm-update-class ()
     (exwm-workspace-rename-buffer exwm-class-name))

   (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)
   (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
   (efs/set-wallpaper)
   (require 'exwm-systemtray)
   (setq exwm-systemtray-height 17)
   (exwm-systemtray-enable)

;; Desktop-Environment
(leaf desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment . "2%+")
  (desktop-environment-brightness-small-decrement . "2%-")
  (desktop-environment-brightness-normal-increment . "5%+")
  (desktop-environment-brightness-normal-decrement . "5%-"))
)

;; EAF
(if (enabled-eaf)
    (require 'module-eaf))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "c0f4b66aa26aa3fded1cbefe50184a08f5132756523b640f68f3e54fd5f584bd" default))
 '(evil-escape-mode t)
 '(org-bullets-bullet-list nil t)
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(## eaf leaf org-roam elfeed-org elfeed-web elfeed org-bullets which-key use-package switch-window swiper solaire-mode peep-dired org-ehtml general gcmh evil-surround evil-org evil-goggles evil-collection doom-themes doom-modeline dired-open diminish avy auto-package-update async all-the-icons-dired))
 '(safe-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
