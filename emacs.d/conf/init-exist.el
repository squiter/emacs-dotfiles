;;; init-exist.el --- Exist.io integration
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
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
;; This configuration file add integration with orgmode and exist.io

;;; Code:

(require 'request)

(defvar exist/access-token *my-exist-access-token*)

(defvar exist/root-url "https://exist.io/api/1")
(defvar exist/authorization (concat "Bearer " exist/access-token))
(defvar exist/headers `(("Content-Type" . "application/json")
                        ("Authorization" . ,exist/authorization)))

(defun exist/build-uri (uri)
  "Create a complete and valid API url with URI."
  (concat exist/root-url uri))

(defun exist/closed-tasks-at (date)
  "Get a count of todos done at passed DATE for all agenda files."
  (let ((closed-drawer (format "CLOSED={%s}" date)))
       (length (org-map-entries t closed-drawer 'agenda))))

(defun exist/send-tasks (quantity)
  "Send the QUANTITY value to exist.io."
  (request
   (exist/build-uri "/attributes/update/")
   :type "POST"
   :headers exist/headers
   :data (json-encode `((("name" . "tasks_completed") ("date" . ,(format-time-string "%Y-%m-%d")) ("value" . ,quantity))))
   :status-code '((400 . (lambda (&rest _) (message "Error to send tasks to Exist.io. Got 400.")))
                  (418 . (lambda (&rest _) (message "Error to send tasks to Exist.io. Got 418.")))
                  (200 . (lambda (&rest _) (message "Tasks sent to Exist.io succefully."))))))

(defun exist/setup ()
  "Get permission to edit tasks_completed in Exist.io."
  (request
   (exist/build-uri "/attributes/acquire/")
   :type "POST"
   :headers exist/headers
   :data (json-encode `((("name" . "tasks_completed") ("active" . true))))
   :status-code '((400 . (lambda (&rest _) (message "Error to send tasks to Exist.io. Got 400.")))
                  (418 . (lambda (&rest _) (message "Error to send tasks to Exist.io. Got 418.")))
                  (200 . (lambda (&rest _) (message "Success! You can use Exist.io integration now."))))))

(defun exist/send-today-tasks ()
  "Send to total tasks done today to Exist.io."
  (exist/send-tasks
   (exist/closed-tasks-at
    (format-time-string "%Y-%m-%d"))))

(advice-add 'org-agenda-quit :before 'exist/send-today-tasks)
(advice-add 'org-save-all-org-buffers :after 'exist/send-today-tasks)

(provide 'init-exist)
;;; init-exist.el ends here
