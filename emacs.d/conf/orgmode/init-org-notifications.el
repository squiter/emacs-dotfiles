;;; init-org-notifications.el --- Setup notifications
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
;; This code was copied from
;; http://emacs.stackexchange.com/questions/3844/good-methods-for-setting-up-alarms-audio-visual-triggered-by-org-mode-events

;;; Code:
(require 'appt)
(appt-activate t)

(setq appt-message-warning-time 5) ; Show notification 5 minutes before event
(setq appt-display-interval appt-message-warning-time) ; Disable multiple reminders
(setq appt-display-mode-line nil)

(defun my-org-agenda-to-appt ()
  "Use appointment data from orgmode."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Update alarms when...
;; (1) ... Starting Emacs
(my-org-agenda-to-appt)

;; (2) ... Everyday at 12:05am (useful in case you keep Emacs always on)
(run-at-time "10:05am" (* 24 3600) 'my-org-agenda-to-appt)

;; (3) ... When save all org buffers
(advice-add 'org-save-all-org-buffers :after 'my-org-agenda-to-appt)

;; Display appointments as a window manager notification
(setq appt-disp-window-function 'my-appt-display)
(setq appt-delete-window-function (lambda () t))

(defvar emacs-icon "/usr/share/icons/hicolor/512x512/apps/emacs25.png")

(setq my-appt-notification-app (concat (getenv "HOME") "/bin/appt-notification"))

(defun my-appt-display (min-to-app new-time msg)
  (if (atom min-to-app)
      (call-process my-appt-notification-app nil nil nil min-to-app msg emacs-icon)
    (dolist (i (number-sequence 0 (1- (length min-to-app))))
      (call-process my-appt-notification-app nil nil nil (nth i min-to-app) (nth i msg) emacs-icon))))

(provide 'init-org-notifications)
;;; init-org-notifications.el ends here
