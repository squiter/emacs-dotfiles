"Stolen from plambert"
(defun custom/insert-new-line ()
  "Insert new line without breaking the current one"
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(provide 'init-custom-functions)
