;;; init-flycheck --- Configs for Flycheck plugin
;;; Commentary:
;;; Code:
(add-hook 'prog-mode-hook 'flycheck-mode)

(setq-default flycheck-disabled-checkers '(html-tidy))

(eval-after-load 'flycheck '(flycheck-clojure-setup))

;; flycheck-pos-tip configuration
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
