;;; init-anzu.el --- My Anzu package configuration
;;; Commentary:
;;; Code:

(global-anzu-mode +1)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "s-%") 'anzu-query-replace-at-cursor)

(provide 'init-anzu)
;;; init-anzu.el ends here
