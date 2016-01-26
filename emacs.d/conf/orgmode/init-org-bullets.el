;;; init-org-bullets.el --- Configurations for Org Bullets Package
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

;;; Code:
(require 'org-bullets)

(setq org-bullets-bullet-list
      '("▸" "▹" "◔" "◕" "◰" "◱" "◲"))

(add-hook 'org-mode-hook 'org-bullets-mode 1)

(provide 'init-org-bullets)
;; init-org-bullets.el ends here
