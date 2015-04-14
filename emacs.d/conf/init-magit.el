(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "M-g c") 'magit-checkout)

(provide 'init-magit)
