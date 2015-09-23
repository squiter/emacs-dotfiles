;;; init-twittering-mode.el --- My Twitter configuration for Emacs
;;
;; URL: https://github.com/hayamiz/twittering-mode
;;
;;; Commentary:

;;; Code:

(setq twittering-icon-mode t)
(global-set-key (kbd "C-, t") 'twittering-update-status-from-pop-up-buffer)

(provide 'init-twittering-mode)
;;; init-twittering-mode.el ends here
