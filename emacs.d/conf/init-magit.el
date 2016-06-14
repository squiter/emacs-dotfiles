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

(defun squiter/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (let ((repo (magit-get "remote" (magit-get-remote) "url")))
    (cond ((string-match "github\\.com" repo)
           (visit-gh-pull-request repo))
          ((string-match "bitbucket\\.org" repo)
           (visit-bb-pull-request repo))
          ((string-match "locaweb\\.com\\.br" repo)
           (visit-lw-pull-request repo)))))

;; TODO: check why this functions is not working
(defun visit-gh-pull-request (repo)
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            repo)
           (magit-get-current-branch))))


(defun visit-bb-pull-request (repo)
  (browse-url
   (format "https://bitbucket.org/%s/pull-request/new"
           (replace-regexp-in-string
            "\\`.+bitbucket\\.org:\\(.+\\)\\.git\\'" "\\1"
            repo))))

;; TODO: make the target_branch be interactive
(defun visit-lw-pull-request (repo)
  (browse-url
   (format "https://code.locaweb.com.br/%s/merge_requests/new%s"
           (replace-regexp-in-string
            "\\`.+code\\.locaweb\\.com\\.br:\\(.+\\)\\.git\\'" "\\1"
            repo)
           (format "?merge_request[source_branch]=%s&merge_request[target_branch]=master"
                   (magit-get-current-branch)))))

;; visit PR for github or bitbucket repositories with "v"
(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'squiter/visit-pull-request-url))

(provide 'init-magit)
;;; init-magit.el ends here
