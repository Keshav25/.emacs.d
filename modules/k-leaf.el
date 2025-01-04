;; k-leaf
;; Leaf and Repositories
(require 'package)

(when istermux
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")
					   ("nongnu" . "https://elpa.nongnu.org/nongnu/")) t)
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (defvar bootstrap-version)
  (let ((bootstrap-file
		 (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
		(bootstrap-version 7))
	(unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
		(goto-char (point-max))
		(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage)))

(straight-use-package 'use-package)

;; Auto Package Update
;; (leaf auto-package-update
;; :setq
;; (auto-package-update-delete-old-versions . t)
;; (auto-package-update-hide-results . t)
;; :config (auto-package-update-maybe))

;; (leaf my/font
;; :config
;; 
;; )

(leaf leaf
  :bind (("C-c f l" . leaf-find)))

(leaf quelpa :ensure t)

(leaf quelpa-leaf
  :after leaf
  :ensure t
  :require t
  :config
  (quelpa-leaf-init))

;; key-chords
(leaf key-chord
  :require t
  :ensure t
  :config
  (key-chord-mode 1))

;; leaf-keywords
(leaf leaf-keywords
  :after leaf
  :ensure t
  :config
  (leaf-keywords-init))

;; leaf-convert
(leaf leaf-convert
  :after leaf
  :ensure t)

;; leaf-manager
(leaf leaf-manager
  :after leaf
  :ensure t)

;; macrostep
(leaf macrostep
  :ensure t
  :bind (("C-c m e" . macrostep-expand)
		 ("C-c m c" . macrostep-collapse)))

(leaf system-packages
  :ensure t)

(provide 'k-leaf)
