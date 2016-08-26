;;; init-org-customs.el --- Custom configurations and functions
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
;; Some of this functrions was copied from p-lambert/emacs-dotfiles

;;; Code:
(defun custom/org-file-path (name)
  (let ((org-file (format "%s.org" name)))
    (f-join *user-org-cache-directory* org-file)))

(defun custom/org-open-project-file (&optional name)
  (interactive)
  (let ((filename (or name (projectile-project-name))))
    (find-file (custom/org-file-path filename))))

(provide 'init-org-customs)
;;; init-org-customs.el ends here
