;;; init-calendars.el --- Calendar configurations
;;
;; Copyright (C) 2018 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 26 June 2018
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
(use-package calfw)
(use-package calfw-org :after (calfw))

(use-package calfw-ical
  :after (calfw)
  :config

  (defun clw:open-locaweb-calendar ()
    (interactive)
    (cfw:open-ical-calendar *locaweb-ical-url*))

  (defun clw:open-google-calendar ()
    (interactive)
    (cfw:open-ical-calendar *google-principal-calendar-url*)))


(provide 'init-calendars)
;;; init-calendars.el ends here
