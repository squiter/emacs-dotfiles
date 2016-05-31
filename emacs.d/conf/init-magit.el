;;; init-magit.el --- My Magit configurations
;;; Commentary:
;;; Code:

(require 'magit)

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "M-g c") 'magit-checkout)

;; Magit, dont fuck with me!
(setq magit-push-always-verify nil)

(defadvice magit-status (around magit-fullscreen activate)
  "Run magit in fullscreen mode."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; stolen from https://github.com/p-lambert/emacs-dotfiles
(defun plambert/branch-changelog ()
  (interactive)
  (let ((default-directory (magit-toplevel))
        (cmd "git log --format='* %s' origin/master..HEAD"))
    (kill-new (shell-command-to-string cmd))
    (message "Changelog copied to kill-ring.")))

(global-set-key (kbd "M-n b c") 'plambert/branch-changelog)

(provide 'init-magit)
;;; init-magit.el ends here
