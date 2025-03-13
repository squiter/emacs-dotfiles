;;; init-magit.el --- Magit configuration
;;
;; Copyright (C) 2015 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/dotfiles
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.
;;
;;; Commentary:
;;
;; Thanks to http://github.com/rranelli/emacs-dotfile

;;; Code:

;; Magit requires a newer transient version to work
(use-package transient)

(use-package magit
  :after transient
  :custom
  (magit-push-always-verify nil)

  :bind
  ("C-c g"   . magit-status)
  ("C-c C-w" . squiter/get-url-for-this-line-number)

  (:map magit-branch-section-map
	("RET"        . magit-checkout)
	("S-<return>" . magit-branch-and-checkout))
  (:map magit-status-mode-map
	("q"   . magit-quit-session)
	("M-1" . delete-other-windows))

  :init
  (setopt magit-format-file-function #'magit-format-file-all-the-icons)

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Functions to get current url of remote repo with file and line number ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun squiter/get-relative-file-with-line ()
    (let ((relative-name (file-relative-name (buffer-file-name) (projectile-project-root)))
          (line-number (number-to-string (line-number-at-pos))))
      (concat relative-name "#L" line-number)))

  (defun squiter/get-current-remote-url ()
    (let ((bare-remote-url (magit-get "remote" (magit-get-remote) "url"))
          (current-branch (magit-get-current-branch)))
      (cond ((string-match "github\\.com" bare-remote-url)
             (format "https://github.com/%s/blob/%s/"
                     (replace-regexp-in-string
                      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                      bare-remote-url)
                     current-branch))

            ((string-match "gitlab\\.com" bare-remote-url)
             (format "https://gitlab.com/%s/-/blob/%s/"
                     (replace-regexp-in-string
                      "\\`.+gitlab\\.com:\\(.+\\)\\.git\\'" "\\1"
                      bare-remote-url)
                     current-branch))

            ((string-match "bitbucket\\.org" bare-remote-url)
             (format "https://bitbucket.org/%s/src/%s/"
                     (replace-regexp-in-string
                      "\\`.+bitbucket\\.org:\\(.+\\)\\.git\\'" "\\1"
                      bare-remote-url)
                     current-branch))

            (t
             (format "https://code.locaweb.com.br/%s/blob/%s/"
                     (replace-regexp-in-string
                      "\\`.+locaweb\\.com\\.br:\\(.+\\)\\.git\\'" "\\1"
                      bare-remote-url)
                     current-branch)))))

  (defun squiter/get-url-for-this-line-number ()
    (interactive)
    (let ((repo-url (squiter/get-current-remote-url))
          (file-with-line-number (squiter/get-relative-file-with-line)))
      (kill-new (concat repo-url file-with-line-number))
      (message "Repo + File + Line number succefully copied!"))))

(use-package magit-todos :after magit :config (magit-todos-mode))

(use-package forge
  :after magit
  :bind
  ("C-c M-a" . git-commit-co-authored))

(provide 'init-magit)
;; init-magit.el ends here
