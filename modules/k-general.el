
;; General
(leaf general
  :after evil
  :ensure t
  :config
  (general-evil-setup t))

(require 'general)

;; Enhance Evil Mode
(unless istermux
(mmap
  ";" 'evil-ex
  ":" 'avy-next))

(mmap
  "/" 'consult-line
  "f" 'avy-goto-char)

;; SPC h for help
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC t"
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
  "k" '(kill-this-buffer :which-key "kill buffer")
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

;; Window Navigation (Mainly For EXWM)
(general-define-key
 "s-h" 'windmove-left
 "s-l" 'windmove-right
 "s-j" 'windmove-down
 "s-k" 'windmove-up)

;; SPC w for Window Navigation
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC w"
  "h" '(windmove-left :which-key "Move to Left Window")
  "j" '(windmove-down :which-key "Move Down a Window")
  "k" '(windmove-up :which-key "Move Up a Window")
  "l" '(windmove-right :which-key "Move to Right Window")
  "s" '(split-and-follow-horizontally :which-key "Split Window Horizontally")
  "v" '(split-and-follow-verticaly :which-key "Split Window Verticaly")
  "t" '(fwb-toggle-window-split :which-key "Toggle Window Split")
  "o" '(switch-window :which-key "Jump to Other Window")
  "f" '(k-toggle-fullscreen :which-key "Un/Maximize a Window")
  "u" '(winner-undo :which-key "Undo Window Manipulation")
  "U" '(winner-redo :which-key "Redo Window Manipulation"))

;; SPC f for Files
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
"f d" '(dired :which-key "Open dired")
"f j" '(dired :which-key "Dired jump to current")
"f p" '(dired :which-key "Peed-dired")
       "."     '(find-file :which-key "Find file")
       "f f"   '(find-file :which-key "Find file")
       "f r"   '(consult-recent-file :which-key "Recent files")
       "f s"   '(save-buffer :which-key "Save file")
       "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
       "f y"   '(dt/show-and-copy-buffer-path :which-key "Yank file path")
       "f C"   '(copy-file :which-key "Copy file")
       "f D"   '(delete-file :which-key "Delete file")
       "f R"   '(rename-file :which-key "Rename file")
       "f S"   '(write-file :which-key "Save file as...")
       "f U"   '(sudo-edit :which-key "Sudo edit file")
	   "f l"   '(leaf-find :which-key "find leaf"))

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


;; SPC v for version control
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC v"
  "g" '(magit-status :which-key "run magit"))

;; SPC s for system
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC s"
  "g" '(guix :which-key "guix")
  ;;"a" '(alert)
  )

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

(provide 'k-general)
