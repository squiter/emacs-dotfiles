;;; init-flycheck --- Configs for Flycheck plugin
;;; Commentary:
;;; Code:
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init (setq-default flycheck-disabled-checkers '(html-tidy)))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package flycheck-projectile
  :after flycheck projectile
  :bind ("C-c ! L" . flycheck-projectile-list-errors))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
