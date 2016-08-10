;;; init-emacs-livedown.el --- Live previews for Markdown files
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

;;; Code:
(add-to-list 'load-path (expand-file-name "vendor/emacs-livedown" user-emacs-directory))
(require 'livedown)

(custom-set-variables
 '(livedown:autostart nil) ; automatically open preview when opening markdown files
 '(livedown:open t)        ; automatically open the browser window
 '(livedown:port 1337))    ; port for livedown server

(provide 'init-emacs-livedown)
;;; init-emacs-livedown.el ends here
