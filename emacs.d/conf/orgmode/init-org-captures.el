;;; init-org-captures.el --- My org capture configurations
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
;;; Code:

;; Org Capture
(setq org-directory *user-org-cache-directory*)
(setq org-capture-directory (path-join org-directory "captures"))
(setq org-default-notes-file (path-join org-directory "refile.org"))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

(defun squiter/oc-template (file)
  "Get org template using a FILE."
  (get-string-from-file (path-join org-capture-directory file)))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      `(("t" "todo" entry (file ,(path-join *user-org-cache-directory* "refile.org"))
         ,(squiter/oc-template "todo.org")
         :clock-in t
         :clock-resume t)
        ("n" "note" entry (file ,(path-join *user-org-cache-directory* "refile.org"))
         ,(squiter/oc-template "note.org")
         :clock-in t
         :clock-resume t)
        ("J" "Jira task" entry
         (file ,(path-join *user-org-cache-directory* "refile.org"))
         "* TODO %(oc/prmt \"Jira Ticket No.\" 'jr-no) %?\n:PROPERTIES:\n:CURRENCY_DELTAS: ((gold +10) (xp +10))\n:END:\n%U\n[[https://locaweb.atlassian.net/browse/%(progn jr-no)][See more in Jira.]]\n"
         :clock-in t
         :clock-resume t)
        ("j" "Journal" entry (file+olp+datetree ,(path-join *user-org-cache-directory* "diary.org"))
         "* %?\n%(oc/inc \"Things that I learned\" \"** Three things that I learn today\n\")"
         :clock-in t
         :clock-resume t)
        ("m" "Morning Journal" entry (file+olp+datetree ,(path-join *user-org-cache-directory* "diary.org"))
         ,(squiter/oc-template "morning-journal.org")
         :clock-in t
         :clock-resume t)
        ("e" "Evening Journal" entry (file+olp+datetree ,(path-join *user-org-cache-directory* "diary.org"))
         ,(squiter/oc-template "evening-journal.org")
         :clock-in t
         :clock-resume t)
        ("w" "Weekly Review" entry (file+olp+datetree ,(path-join *user-org-cache-directory* "diary.org"))
         ,(squiter/oc-template "weekly-review.org")
         :clock-in t
         :clock-resume t)
        ("s" "Code Snippet" entry
         (file ,(path-join *user-org-cache-directory* "snippets.org"))
         ;; Prompt for tag and language
         "* %? :NOTE:\t\n%U\n#+BEGIN_SRC %(eval custom/org-mode-memory)\n%c\n#+END_SRC")
        ("h" "Habit" entry (file ,(path-join *user-org-cache-directory* "refile.org"))
         ,(squiter/oc-template "habit.org")
         :clock-in t
         :clock-resume t)
        ("1" "1:1 Meetings" entry (file+headline ,(path-join *user-org-cache-directory* "nubank.org") "1:1 Meetings")
         ,(squiter/oc-template "one-o-one-meeting.org")
         :clock-in t
         :clock-resume t)))

(defvar custom/org-mode-memory nil)
(defadvice org-capture (before custom/org-mode-memory activate)
  (setq custom/org-mode-memory (substring (symbol-name major-mode) 0 -5)))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; http://storax.github.io/blog/2016/05/02/org-capture-tricks/
;; Use user input in severall locations
(defvar oc-capture-prmt-history nil
  "History of prompt answers for org capture.")
(defun oc/prmt (prompt variable)
  "PROMPT for string, save it to VARIABLE and insert it."
  (make-local-variable variable)
  (set variable (read-string (concat prompt ": ") nil oc-capture-prmt-history)))

;; Conditionally insert text
(defun oc/inc (what text &rest fmtvars)
  "Ask user to include WHAT.  If user agrees return TEXT."
  (when (y-or-n-p (concat "Include " what "?"))
    (apply 'format text fmtvars)))

;; Thanks for @pashini : https://github.com/pashinin/emacsd/blob/master/elisp/init-org-capture.el
;; Org - Capture
(require 'remember)
;; make the frame contain a single window. by default org-remember
;; splits the window.
(add-hook 'remember-mode-hook 'delete-other-windows)

;;---------------------
;; http://www.windley.com/archives/2010/12/capture_mode_and_emacs.shtml
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))

;; make the frame contain a single window. by default org-capture
;; splits the window.
;; (add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun my-capture-hook ()
  (when (string= "1" (plist-get org-capture-plist :key))
    (progn
      (org-tags-view nil "ONEONONE")
      (vsplit-last-buffer))))

(add-hook 'org-capture-mode-hook #'my-capture-hook)

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)
    ))

(defun make-remember-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  ;; if there are no emacs frames - make frame
  (if (= (length '(frame-list)) 0)
      (progn
        (make-frame '((name . "capture")
                      (width . 90)
                      (height . 20)
                      ;;(auto-raise . t)
                      )))
    (progn
      (rename-frame (selected-frame) "capture")  ; install frame-cmds from melpa for this
      ))
  (select-frame-by-name "capture")
  (set-frame-width  (selected-frame) 90)
  (set-frame-height (selected-frame) 20)
  (setq word-wrap      1)
  (setq truncate-lines nil)
  (org-capture))

(provide 'init-org-captures)
;;; init-org-captures.el ends here
