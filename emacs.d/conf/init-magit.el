;;; init-magit.el --- My Magit configurations
;;; Commentary:
;;; Code:

(require 'magit)

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

;; stolen from https://github.com/p-lambert/emacs-dotfiles
(defun plambert/branch-changelog ()
  (interactive)
  (let ((default-directory (magit-toplevel))
        (cmd "git log --format='* %s' origin/master..HEAD"))
    (kill-new (shell-command-to-string cmd))
    (message "Changelog copied to kill-ring.")))

(provide 'init-magit)
;;; init-magit.el ends here
