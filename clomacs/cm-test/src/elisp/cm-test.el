(require 'clomacs)
(clomacs-defun cm-test-md-to-html-wrapper
               cm-test.core/my-md-to-html-string
               :lib-name "cm-test"
               :namespace cm-test.core
               :doc "Convert markdown to html via Clojure lib.")

(defun cm-test-mdarkdown-to-html (beg end)
  "Add to the selected markdown text it's html representation."
  (interactive "r")
  (save-excursion
    (if (< (point) (mark))
        (exchange-point-and-mark))
    (insert
     (concat "\n" (cm-test-md-to-html-wrapper
                   (buffer-substring beg end))))))

(provide 'cm-test)
