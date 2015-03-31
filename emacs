(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))

(require 'init-general)
(require 'init-sml)
(require 'init-ui)
(require 'init-packages)

;; init my packages configurations
(require 'init-helm)
(require 'init-magit)
(require 'init-projectile)
(require 'init-web-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-eighties)))
 '(custom-safe-themes (quote ("e3c90203acbde2cf8016c6ba3f9c5300c97ddc63fcb78d84ca0a144d402eedc6" "18804b0ef053c87cd79a3df7d33b7e9876cdeb1e2d9f928ca5b8cedc4e1b678c" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )