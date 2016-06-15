;;; init-git-pr.el --- Functions to improve pull requests
;;
;; Copyright (C) 2015 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
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
;; This file contains functions that should be trigger to open pull
;; request/merge requests in gihub, bitbucket or gitlab.
;;
;; You can go to pull request page by typing `v' inside a magit
;; buffer.  This script will use your current branch as source and
;; will ask for destiny in your minibuffer.

;;; Code:

(defcustom gitlab-url "https://gitlab.com"
  "URL to your gitlab project."
  :type 'string
  :group 'init-git-pr)

(defcustom gitlab-remote-pattern "\\`.gitlab\\.com:\\(.+\\)\\.git\\'"
  "Regex pattern to extract project name."
  :type 'string
  :group 'init-git-pr)

(defun squiter/visit-pull-request-url ()
  "Visit the current branch's PR on Github/Bitbucker/Gitlab."
  (interactive)
  (let ((repo (magit-get "remote" (magit-get-remote) "url")))
    (cond ((string-match "github\\.com" repo)
           (squiter/visit-gh-pull-request repo))
          ((string-match "bitbucket\\.org" repo)
           (squiter/visit-bb-pull-request repo))
          (t
           (squiter/visit-gl-pull-request repo)))))

;; TODO: check why this functions is not working
(defun squiter/visit-gh-pull-request (repo)
  "Visit the current branch's PR on Github."
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            repo)
           (magit-get-current-branch))))


(defun squiter/visit-bb-pull-request (repo)
  "Visit the current branch's PR on Bitbucket."
  (browse-url
   (format "https://bitbucket.org/%s/pull-request/new"
           (replace-regexp-in-string
            "\\`.+bitbucket\\.org:\\(.+\\)\\.git\\'" "\\1"
            repo))))

(defun squiter/visit-gl-pull-request (repo)
  "Visit the current branch's MR on Gitlab."
  (browse-url
   (format "%s/%s/merge_requests/new%s"
           (format gitlab-url)
           (replace-regexp-in-string
            gitlab-remote-pattern "\\1"
            repo)
           (format "?merge_request[source_branch]=%s&merge_request[target_branch]=%s"
                   (magit-get-current-branch)
                   (magit-read-branch "Choose your destiny branch: ")))))

;; visit PR for github or bitbucket repositories with "v"
(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'squiter/visit-pull-request-url))

(provide 'init-git-pr)
;;; init-git-pr.el ends here
