;; theme options
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-eighties t)

;; other options
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 140)

;; line numbers
(add-hook 'prog-mode-hook 'linum-mode)

(provide 'init-ui)
