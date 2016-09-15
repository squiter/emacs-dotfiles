;;; init-org-gamify.el --- Gamification over orgmode
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 15 setembro 2016
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
;; This file configures org-gamify (https://bitbucket.org/eeeickythump/org-gamify)

;;; Code:
(add-to-list 'load-path (expand-file-name "vendor/org-gamify/" user-emacs-directory))

(require 'org-gamify)

(define-gamify-currency gold
  :name "gold"
  :category "Money"
  :min 0 :enforce-min block)

(define-gamify-currency xp
  :name "XP"
  :category "Experience"
  :min 0 :enforce-min truncate)

(provide 'init-org-gamify)
;;; init-org-gamify.el ends here
