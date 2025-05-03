(require 'clomacs)
(clomacs-defun cm-test-output-kesh
               cm-test.core/output-kesh
               :lib-name "cm-test"
               :namespace cm-test.core
               :doc "Output Kesh's astrological information")

(defun cm-test-output-kesh-in-region (beg end)
  "Add to the selected markdown text it's html representation."
  (interactive "r")
  (save-excursion
    (if (< (point) (mark))
        (exchange-point-and-mark))
    (insert
     (concat "\n" (cm-test-output-kesh)
             (buffer-substring beg end)))))

(provide 'cm-test)
