;;; init-magit.el --- My Magit configurations
;;; Commentary:
;;; Code:

(use-package magit
  :custom
  (magit-push-always-verify nil)

  :bind
  ("C-c g"   . magit-status)
  ("M-g c"   . magit-checkout)
  ("M-g l"   . magit-log-buffer-file)
  ("M-n b c" . plambert/branch-changelog)
  (:map magit-branch-section-map
        ("RET"        . magit-checkout)
        ("S-<return>" . magit-branch-and-checkout))
  (:map magit-status-mode-map
        ("q"   . magit-quit-session)
        ("M-1" . delete-other-windows))
  (:map git-commit-mode-map
        ("C-c C-a" . git-commit-co-authored))

  :config
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
      (message "Changelog copied to kill-ring."))))

(use-package magit-todos :after magit :config (magit-todos-mode))
(use-package forge :after magit)
(use-package github-review :after magit)

(provide 'init-magit)
;;; init-magit.el ends here
