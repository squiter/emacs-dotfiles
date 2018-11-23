;;; init-dired.el --- Configurations for Dired
;;
;; Copyright (C) 2015 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/dotfiles
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
;; Thanks to Pedro Tavares @
;; https://raw.githubusercontent.com/pgt/emacs-dotfiles for the
;; examples and configurations :)

;;; Code:
(require 'dired)

;; allow editing file permissions
(setq wdired-allow-to-change-permissions t)

;; Reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
	   (revert-buffer))))

(defun dired-get-size ()
  "Show amount size of selected files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
		 (match-string 1))))))

(define-key dired-mode-map (kbd "?") 'dired-get-size)

;; Dired open Wdired
(define-key dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)

;;; Show Kbytes on dired
(setq dired-listing-switches "-alh")

;;; Open dired with `a' key
(put 'dired-find-alternate-file 'disabled nil)

;;; Always recursively delete directory
(setq dired-recursive-deletes 'always)

;;; Always recursively copy directory
(setq dired-recursive-copies 'always)

;;; Delete by moving to trash
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

(defun open-default-mac-program ()
  "Open current file with default OSX app."
  (interactive)
  (save-window-excursion
    (let ((files (dired-get-marked-files nil current-prefix-arg))
	  command)
      ;; the open command
      (setq command "open ")
      (dolist (file files)
	(setq command (concat command (shell-quote-argument file) " ")))
      (message command)
      ;; execute the command
      (async-shell-command command))))
(define-key dired-mode-map (kbd "C-M-o") 'open-default-mac-program)

(defun reveal-in-finder ()
  "Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current directory."
  (interactive)
  (let* ((path (buffer-file-name)) ; The full file path associated with the buffer.
	 (filename-at-point (dired-file-name-at-point)) ; effective in dired only
	 ;; Create a full path if filename-at-point is non-nil
	 (filename-at-point (if filename-at-point
				(expand-file-name filename-at-point) ; full path
			      nil)) ; if nil, return nil
	 dir file)		   ; let* definition part ends here.

    ;; Conditionals: The first one that is non-nil is executed.
    (cond (path
	   ;; If path is non-nil,
	   (setq dir  (file-name-directory    path))
	   (setq file (file-name-nondirectory path)))

	  (filename-at-point
	   ;; If filename-at-point is available from dired,
	   (setq dir  (file-name-directory    filename-at-point))
	   (setq file (file-name-nondirectory filename-at-point)))

	  (t
	   ;; Otherwise,
	   (setq dir  (expand-file-name default-directory))))

    ;; Pass dir and file to the helper function.
    ;; (message (concat "dir:" dir " ; file:" file " ; path:" path " ; fap:" filename-at-point)) ; for debugging
    (reveal-in-finder-as dir file) ; These variables are  passed to the helper function.
    ))
(define-key dired-mode-map (kbd "C-S-o") 'reveal-in-finder)

;; AppleScript helper function. Thanks milkeypostman for suggestions.
;; Use let* to reuse revealpath in defining script.
(defun reveal-in-finder-as (dir file)
  "A helper function for reveal-in-finder that passes DIR and FILE arguments to this apple script."
  (let* ((revealpath (if file		   ; Define revealpath local variable.
			 (concat dir file) ; dir/file if file name available.
		       dir))		   ; dir only if not.
	 (script			   ; Define script variable using revealpath and text.
	  (concat
	   "set thePath to POSIX file \"" revealpath "\"\n"
	   "tell application \"Finder\"\n"
	   " set frontmost to true\n"
	   " reveal thePath \n"
	   "end tell\n")))		   ; let* definition part ends here.
    ;; (message script)			   ; Check the text output.
    (start-process "osascript-getinfo" nil "osascript" "-e" script) ; Run AppleScript.
    ))


(provide 'init-dired)
;;; init-dired.el ends here
