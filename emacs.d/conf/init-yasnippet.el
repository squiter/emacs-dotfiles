;;; init-yasnippet.el --- Configurations for Yasnippet Package
;;; Commentary:
;;; Code:
(require 'yasnippet)

(setq yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

(yas-global-mode 1)

;; disable yasnipet for any terminal mode
(add-hook 'term-mode-hook (lambda()
                            (yas-minor-mode -1)))

;; always load fundamental-mode as a global snippet mode
(add-hook 'yas-minor-mode-hook
          (lambda ()
            (yas-activate-extra-mode 'fundamental-mode)))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
