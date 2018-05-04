;;; init-flycheck --- Configs for Flycheck plugin
;;; Commentary:
;;; Code:
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init (setq-default flycheck-disabled-checkers '(html-tidy)))

(use-package flycheck-pos-tip
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
