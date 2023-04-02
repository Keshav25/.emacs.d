(leaf hexo
  :ensure t)

(defun hexo-my-blog ()
    (interactive)
    ;; change to your blog directory
    (hexo "~/org/hexo/"))

(provide 'k-hexo)
