(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; To work well with line numbers
(git-gutter:linum-setup)

;; hook to update gutter when using magit
(add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)

;; `M-x git-gutter-reset-to-default` compare you file under working directory with the latet version under VCS.
;; `M-x git-gutter:next-hunk` and `M-x git-gutter:previous-hunk` will jump to the deleted/modified/added line.
(defun git-gutter-reset-to-head-parent()
  (interactive)
  (let (parent (filename (buffer-file-name)))
    (if (eq git-gutter:vcs-type 'svn)
        (setq parent "PREV")
      (setq parent (if filename (concat (shell-command-to-string (concat "git --no-pager log --oneline -n1 --pretty='format:%H' " filename)) "^") "HEAD^")))
    (git-gutter:set-start-revision parent)
    (message "git-gutter:set-start-revision HEAD^")))

(defun git-gutter-reset-to-default ()
  (interactive)
  (git-gutter:set-start-revision nil)
  (message "git-gutter reset"))

(global-set-key (kbd "C-, g h p") 'git-gutter-reset-to-head-parent)
(global-set-key (kbd "C-, g h d") 'git-gutter-reset-to-default)
(global-set-key (kbd "C-, g n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-, g p") 'git-gutter:previous-hunk)

(provide 'init-gitgutter)
