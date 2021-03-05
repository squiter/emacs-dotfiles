;;; init-org-gcal.el --- Google Calendar configurations
;;
;; Copyright (C) 2017 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created:  9 janeiro 2017
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

;;; Code:

(use-package persist :defer t)

;; TODO: Fix this package installation
(use-package org-gcal
  :hook ((org-agenda-mode-hook . squiter/gcal-fetch-with-messages)
         (org-after-refile-insert-hook . squiter/gcal-fetch-with-messages))
  :init
  (setq org-gcal-client-id *google-calendar-client-id*
        org-gcal-client-secret *google-calendar-secret-id*
        org-gcal-file-alist `(("squiter85@gmail.com" . ,(path-join *user-org-cache-directory* "gcal.org"))))

  (defun squiter/gcal-fetch-with-messages ()
    (message "Syncing Google Calendar...")
    (org-gcal-fetch)
    (message "Google Calendar Synced!"))

  ;; (add-hook 'org-agenda-mode-hook
  ;;           (lambda ()
  ;;             (message "Syncing Google Calendar...")
  ;;             (org-gcal-fetch)
  ;;             (message "Google Calendar Synced!")))

  ;; (add-hook 'org-after-refile-insert-hook
  ;;           (lambda ()
  ;;             (message "Syncing Google Calendar...")
  ;;             (org-gcal-fetch)
  ;;             (message "Google Calendar Synced!")))
  (message "org-cal loaded!!"))

(provide 'init-org-gcal)
;;; init-org-gcal.el ends here
