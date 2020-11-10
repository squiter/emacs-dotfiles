;;; init-org.el --- My custom configuration for orgmode
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
;;

;;; Code:
(require 'org)
(require 'org-habit)
(require 'org-id)
(require 'org-crypt)

(add-to-list 'load-path (expand-file-name "conf/orgmode" user-emacs-directory))

(defconst *user-org-cache-directory*
  (path-join *user-dropbox-directory* "org")
  "Path to user's org cache store.")

(setq org-ellipsis " ➥")
(setq org-startup-indented t)

;; Set images with 1/3 of my display size
(setq org-image-actual-width (/ (display-pixel-width) 3))
;; Load images on startup
(setq org-startup-with-inline-images t)

(setq org-log-reschedule 'time)

(require 'init-org-keybinds)
(require 'init-org-bullets)
(require 'init-org-agenda)
(require 'init-org-captures)
(require 'init-org-speed-commands)
(require 'init-org-refile)
(require 'init-org-agenda-view)
(require 'init-org-agenda-tunned)
(require 'init-org-clock)
(require 'init-org-projects)
(require 'init-org-notifications)
(require 'init-org-babel)
(require 'init-org-alfred)
(require 'init-org-customs)
(require 'init-org-gcal)
(require 'init-org-insert-image)
(require 'init-org-roam)

(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("[ ]" . "☐") prettify-symbols-alist)
   (push '("[X]" . "☑") prettify-symbols-alist)
   (push '("[-]" . "❍") prettify-symbols-alist)
   (prettify-symbols-mode)))

;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup) ;; location group
                            ("@office" . ?O)
                            ("@home" . ?H)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            (:endgroup)
                            (:startgroup) ;; note type
                            ("IDEA" . ?i)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("NOTE" . ?n)
                            ("crypt" . ?k)
                            ("LENT" . ?l)
                            ("BILLS" . ?B)
                            ("SKIPPED" . ?s)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(setq org-log-into-drawer t)

;; Org Crypt configuration
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(setq org-crypt-key nil)
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

;; Set the defaul warnings for deadline
(setq org-deadline-warning-days 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Org mode 8.3    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-repair-property-drawers ()
  "Fix properties drawers in current buffer.  Ignore non Org buffers."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t)
           (inline-re (and (featurep 'org-inlinetask)
                           (concat (org-inlinetask-outline-regexp)
                                   "END[ \t]*$"))))
       (org-map-entries
        (lambda ()
          (unless (and inline-re (org-looking-at-p inline-re))
            (save-excursion
              (let ((end (save-excursion (outline-next-heading) (point))))
                (forward-line)
                (when (org-looking-at-p org-planning-line-re) (forward-line))
                (when (and (< (point) end)
                           (not (org-looking-at-p org-property-drawer-re))
                           (save-excursion
                             (and (re-search-forward org-property-drawer-re end t)
                                  (eq (org-element-type
                                       (save-match-data (org-element-at-point)))
                                      'drawer))))
                  (insert (delete-and-extract-region
                           (match-beginning 0)
                           (min (1+ (match-end 0)) end)))
                  (unless (bolp) (insert "\n"))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show habit graphs everywhere ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my/org-habit-show-graphs-everywhere nil
  "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil variable to show
consistency graphs in all Org mode agendas.")

(defun my/org-agenda-mark-habits ()
  "Mark all habits in current agenda for graph display.

This function enforces `my/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `my/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
  (when (and my/org-habit-show-graphs-everywhere
         (not (get-text-property (point) 'org-series)))
    (let ((cursor (point))
          item data) 
      (while (setq cursor (next-single-property-change cursor 'org-marker))
        (setq item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item)) 
          (with-current-buffer (marker-buffer item)
            (setq data (org-habit-parse-todo item))) 
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p data))))))

(setq my/org-habit-show-graphs-everywhere t)
(advice-add #'org-agenda-finalize :before #'my/org-agenda-mark-habits)

(defun my-init-hook ()
  (split-window-right)
  (let ((org-agenda-window-setup 'other-window))
    (org-agenda nil " ")))

(add-hook 'window-setup-hook #'my-init-hook)

(provide 'init-org)
;;; init-org.el ends here
