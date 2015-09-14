;;; init-smerge.el --- My configurations to git merge
;;; commentary:
;;; code:

(setq smerge-command-prefix "\C-cm")

;; TODO: Try to make this config works
;; (defun my-enable-smerge-maybe ()
;;   (when (and buffer-file-name (vc-backend buffer-file-name))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (when (re-search-forward "^<<<<<<< " nil t)
;;         (smerge-mode +1)))))

;; (add-hook 'buffer-list-update-hook 'my-enable-smerge-maybe)

(provide 'init-smerge)
;;; init-smerge.el ends here
