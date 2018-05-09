;;; init-twittering-mode.el --- My Twitter configuration for Emacs
;;
;; URL: https://github.com/hayamiz/twittering-mode
;;
;;; Commentary:

;;; Code:

(use-package twittering-mode
  :init
  (setq twittering-icon-mode t)
  (setq twittering-convert-fix-size 24)
  :bind
  ("C-x t" . twittering-update-status-from-pop-up-buffer))

(provide 'init-twittering-mode)
;;; init-twittering-mode.el ends here
