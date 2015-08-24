;;; init-yasnippet.el --- Configurations for Yasnippet Package
;;; Commentary:
;;; Code:
(require 'yasnippet)

(setq yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

(yas-global-mode 1)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
