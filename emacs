(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))

(require 'init-general)
(require 'init-custom-functions)
(require 'init-sml)
(require 'init-ui)
(require 'init-packages)
(require 'init-keybindings)

;; init my packages configurations
(require 'init-helm)
(require 'init-magit)
(require 'init-projectile)
(require 'init-ruby)
(require 'init-web-mode)
(require 'init-company)
(require 'init-gitgutter)