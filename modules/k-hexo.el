;; -*- lexical-binding: t -*-

(leaf hexo
  :elpaca t
  :config
  (defun hexo-my-blog ()
	(interactive)
	;; change to your blog directory
	(hexo "~/org/blog/")))

(provide 'k-hexo)
