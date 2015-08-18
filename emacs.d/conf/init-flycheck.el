;;; init-flycheck --- Configs for Flycheck plugin
;;; Commentary:
;;; Code:
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers '(html-tidy))

;; disable flycheck in restclient responses
(add-hook 'restclient-mode-hook (flycheck-mode 0))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
