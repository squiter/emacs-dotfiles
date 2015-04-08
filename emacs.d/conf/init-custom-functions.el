"Stolen from plambert"
(defun custom/insert-new-line ()
  "Insert new line without breaking the current one"
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

"Stackoverflow functions :)"
(defun switch-fullscreen nil
  (interactive)
  (let* ((modes '(nil fullboth fullwidth fullheight))
         (cm (cdr (assoc 'fullscreen (frame-parameters) ) ) )
         (next (cadr (member cm modes) ) ) )
    (modify-frame-parameters
     (selected-frame)
     (list (cons 'fullscreen next)))))

(provide 'init-custom-functions)
