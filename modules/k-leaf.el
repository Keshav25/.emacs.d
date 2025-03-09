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

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

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

;; (defvar elpaca-installer-version 0.8)
;; (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;; (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
;; (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
;; (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
;;                               :ref nil :depth 1
;;                               :files (:defaults "elpaca-test.el" (:exclude "extensions"))
;;                               :build (:not elpaca--activate-package)))
;; (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
;;        (build (expand-file-name "elpaca/" elpaca-builds-directory))
;;        (order (cdr elpaca-order))
;;        (default-directory repo))
;;   (add-to-list 'load-path (if (file-exists-p build) build repo))
;;   (unless (file-exists-p repo)
;;     (make-directory repo t)
;;     (when (< emacs-major-version 28) (require 'subr-x))
;;     (condition-case-unless-debug err
;;         (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
;;                   ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
;;                                                   ,@(when-let* ((depth (plist-get order :depth)))
;;                                                       (list (format "--depth=%d" depth) "--no-single-branch"))
;;                                                   ,(plist-get order :repo) ,repo))))
;;                   ((zerop (call-process "git" nil buffer t "checkout"
;;                                         (or (plist-get order :ref) "--"))))
;;                   (emacs (concat invocation-directory invocation-name))
;;                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
;;                                         "--eval" "(byte-recompile-directory \".\" 0 'force)")))
;;                   ((require 'elpaca))
;;                   ((elpaca-generate-autoloads "elpaca" repo)))
;;             (progn (message "%s" (buffer-string)) (kill-buffer buffer))
;;           (error "%s" (with-current-buffer buffer (buffer-string))))
;;       ((error) (warn "%s" err) (delete-directory repo 'recursive))))
;;   (unless (require 'elpaca-autoloads nil t)
;;     (require 'elpaca)
;;     (elpaca-generate-autoloads "elpaca" repo)
;;     (load "./elpaca-autoloads")))
;; (add-hook 'after-init-hook #'elpaca-process-queues)
;; (elpaca `(,@elpaca-order))

;; ;; (elpaca elpaca-use-package
;;   ;; Enable use-package :ensure support for Elpaca.
;;   ;; (elpaca-use-package-mode))

;; ;; (use-package p-search :elpaca (:host github :repo "https://github.com/zkry/p-search.git"))



(leaf leaf
  :bind (("C-c f l" . leaf-find)))

;; (leaf leaf-elpaca
;;   :config
;;   (defmacro leaf-handler-package (name pkg _pin)
;; 	"Handler for ensuring the installation of PKG with package.el
;; via PIN in the leaf block NAME."
;; 	`(progn
;;        (leaf-safe-push ',pkg package-selected-packages 'no-dup)
;;        (unless (package-installed-p ',pkg)
;; 		 (unless (assoc ',pkg package-archive-contents)
;;            (package-refresh-contents))
;; 		 (condition-case _err
;; 			 (elpaca ',pkg)
;;            (error
;; 			(package-refresh-contents)
;; 			(condition-case err
;; 				(elpaca ',pkg)
;;               (error
;;                (display-warning 'leaf
;; 								(format
;; 								 ,(concat
;;                                    (format "In `%s' block" name)
;;                                    (when load-file-name
;; 									 (format " at `%s'" load-file-name))
;;                                    (format ", failed to :package of `%s'." pkg)
;;                                    "  Error msg: %s")
;; 								 (error-message-string err)))))))))))

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
