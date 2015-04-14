(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; To work well with line numbers
(git-gutter:linum-setup)

;; hook to update gutter when using magit
(add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)

(provide 'init-gitgutter)
