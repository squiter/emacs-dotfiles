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

(defun zin/since-state (since todo-state &optional done all)
  "List Agenda items that are older than SINCE.

TODO-STATE is a regexp for matching to TODO states.  It is provided to
`zin/find-state' to match inactive timestamps.
SINCE is compared to the result of `zin/org-date-diff'.  If
`zin/org-date-diff' is greater than SINCE, the entry is shown in the
Agenda.
Optional argument DONE allows for done and not-done headlines to be
evaluated.  If DONE is non-nil, match completed tasks.
Optional argument ALL is passed to `zin/find-state' to specify whether
to search for any possible match of STATE, or only in the most recent
log entry."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    ;; If DONE is non-nil, look for done keywords, if nil look for not-done
    (if (member (org-get-todo-state)
                (if done
                    org-done-keywords
                  org-not-done-keywords))
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (subtree-valid (save-excursion
                                (forward-line 1)
                                (if (and (< (point) subtree-end)
                                         ;; Find the timestamp to test
                                         (zin/find-state todo-state subtree-end all))
                                    (let ((startpoint (point)))
                                      (forward-word 3)
                                      ;; Convert timestamp into days difference from today
                                      (zin/org-date-diff startpoint (point)))))))
          (if (or (not subtree-valid)
                  (<= subtree-valid since))
              next-headline
            nil))
      (or next-headline (point-max)))))

(defun zin/find-state (state &optional end all)
  "Used to search through the logbook of subtrees.

Tests to see if the first line of the logbook is a change of todo
status to status STATE
- Status \"STATE\" from ...
The search brings the point to the start of YYYY-MM-DD in inactive timestamps.

Optional argument END defines the point at which to stop searching.
Optional argument ALL when non-nil specifies to look for any occurence
of STATE in the subtree, not just in the most recent entry."
  (let ((drawer (if all "" ":.*:\\W" "CLOSED:")))
    (or (re-search-forward (concat drawer ".*State \\\"" state "\\\"\\W+from.*\\[") end t)
        (re-search-forward (concat drawer ".*\\[") end t))))

(defun zin/org-date-diff (start end &optional compare)
  "Calculate difference between  selected timestamp to current date.

The difference between the dates is calculated in days.
START and END define the region within which the timestamp is found.
Optional argument COMPARE allows for comparison to a specific date rather than to current date."
  (let* ((start-date (if compare compare (calendar-current-date))))
    (- (calendar-absolute-from-gregorian start-date) (org-time-string-to-absolute (buffer-substring-no-properties start end)))
    ))

;; Adapted from https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location
(defun my/refile (file headline &optional arg)
  "Refile to a specific location.
With a 'C-u' ARG argument, we jump to that location (see
`org-refile').
Use `org-agenda-refile' in `org-agenda' mode."
  (let* ((pos (with-current-buffer (or (get-buffer file)	;Is the file open in a buffer already?
				       (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
		(or (org-find-exact-headline-in-buffer headline)
		    (error "Can't find headline `%s'" headline))))
	 (filepath (buffer-file-name (marker-buffer pos)));If we're given a relative name, find absolute path
	 (rfloc (list headline filepath nil pos)))
    (if (and (eq major-mode 'org-agenda-mode) (not (and arg (listp arg)))) ;Don't use org-agenda-refile if we're just jumping
	(org-agenda-refile nil rfloc)
      (org-refile arg nil rfloc))))

(defun josh/refile (file headline &optional arg)
  "Refile to HEADLINE in FILE. Clean up org-capture if it's activated.
With a `C-u` ARG, just jump to the headline."
  (interactive "P")
  (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
    (cond
     ((and arg (listp arg))	    ;Are we jumping?
      (my/refile file headline arg))
     ;; Are we in org-capture-mode?
     (is-capturing      	;Minor mode variable that's defined when capturing
      (josh/org-capture-refile-but-with-args file headline arg))
     (t
      (my/refile file headline arg)))
    (when (or arg is-capturing)
      (setq hydra-deactivate t))))

(defun my-org-export-each-headline-to-markdown (&optional scope)
  "Export each headline to a markdown file with the title as filename.
If SCOPE is nil headlines in the current buffer are exported.
For other valid values for SCOPE see `org-map-entries'.
Already existing files are overwritten."
  (interactive)
  ;; Widen buffer temporarily as narrowing would affect the exporting.
  (org-with-wide-buffer
   (save-mark-and-excursion
     ;; Loop through each headline.
     (org-map-entries
      (lambda ()
        ;; Get the plain headline text without statistics and make filename.
        (let* ((title (car (last (org-get-outline-path t))))
               (dir (file-name-directory buffer-file-name))
               (filename (concat dir title ".md")))
          ;; Set the active region.
          (set-mark (point))
          (outline-next-preface)
          (activate-mark)
          ;; Export the region to a markdown file.
          (with-current-buffer (org-md-export-as-markdown)
            ;; Save the buffer to file and kill it.
            (write-file filename)
            (kill-current-buffer))))
      nil scope))))

(provide 'init-org-customs)
;;; init-org-customs.el ends here
