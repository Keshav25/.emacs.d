(leaf zig-mode
  :elpaca t
  :config
  (flycheck-define-checker zig
	"zig"
	:command ("zig" "ast-check" (eval (buffer-file-name)))
	:error-patterns
	((error line-start (file-name) ":" line ":" column ": error:" (message) line-end))
	:modes zig-mode t)
  (add-to-list 'flycheck-checkers 'zig))

(provide 'k-zig)
