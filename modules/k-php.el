;; https://github.com/strikerlulu/projectile-laravel
(use-package projectile-laravel
  :straight (projectile-laravel :type git :host github :repo "strikerlulu/projectile-laravel"))

(leaf php
  :config
  (defun k/insert-backslash ()
	(interactive)
	(insert "\\"))

  (defun k/insert-forwardslash ()
	(interactive)
	(insert "/"))

  (defun k/insert-arrow ()
	(interactive)
	(insert "->"))
  
  (define-key php-ts-mode-map "\\" 'k/insert-forwardslash)
  (define-key php-ts-mode-map "/" 'k/insert-backslash)
  ;; need a way better binding than .
  (define-key php-ts-mode-map "." 'k/insert-arrow))

(provide 'k-php)
;;; k-php.el ends here
