;;; init-org-roam.el --- Configuration of org-roam
;;
;; Copyright (C) 2020 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created:  5 August 2020
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

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (path-join *user-dropbox-directory* "org-roam"))
  (org-roam-db-location (path-join *user-dropbox-directory* "org-roam" "org-roam.db"))
  (org-roam-capture-templates
   `(("d" "default" plain #'org-roam-capture--get-point
      "%?\n\n* Links\n- %a\n"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n#+roam_tags: %(oc/prmt \"Roam Tags\" 'tags)\n#+roam_key: %(oc/prmt \"Roam Key\" 'key)\n#+created_at: %U\n#+STARTUP: showall\n\n"
      :unnarrowed t)))
  :bind (:map org-roam-mode-map
              (("C-c z l" . org-roam)
               ("C-c z f" . org-roam-find-file)
               ("C-c z g" . org-roam-graph-show)
               ("C-c z s" . org-roam-server-mode))
              :map org-mode-map
              (("C-c z i" . org-roam-insert))
              (("C-c z I" . org-roam-insert-immediate)))
  :config
  (require 'org-roam-protocol))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)

  (defun org-roam-server-open ()
    "Opens the url for the org-roam-server"
    (interactive)
    (browse-url (concat "http://" org-roam-server-host ":" (number-to-string org-roam-server-port))))

  :bind (:map org-roam-mode-map
              (("C-c z o" . org-roam-server-open))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
