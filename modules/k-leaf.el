;; k-leaf
;; Leaf and Repositories
(require 'package)

(when istermux
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")) t)
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))

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

;; leaf-keywords
(leaf leaf-keywords :ensure t)

;; leaf-convert
(leaf leaf-convert :ensure t)

;; leaf-manager
(leaf leaf-manager :ensure t)

;; macrostep
(leaf macrostep
  :ensure t
  :bind (("C-c m e" . macrostep-expand)
		 ("C-c m c" . macrostep-collapse)))

(provide 'k-leaf)