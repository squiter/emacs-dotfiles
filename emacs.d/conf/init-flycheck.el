;;; init-flycheck --- Configs for Flycheck plugin
;;; Commentary:
;;; Code:
(add-hook 'prog-mode-hook 'flycheck-mode)

(setq-default flycheck-disabled-checkers '(html-tidy))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
