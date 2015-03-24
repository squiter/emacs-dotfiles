;; theme options
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-eighties t)

;; other options
(tool-bar-mode 0)
(set-face-attribute 'default nil :height 140)

;; line numbers
(add-hook 'prog-mode-hook 'linum-mode)

(provide 'init-ui)
