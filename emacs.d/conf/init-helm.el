;;; init-helm.el --- My Helm configurations
;;; Commentary:
;;; Code:

(require 'helm-config)

(helm-mode t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-quick-update t
     helm-buffers-fuzzy-matching t)

(provide 'init-helm)
;;; init-helm.el ends here
