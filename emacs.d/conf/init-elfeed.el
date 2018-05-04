;;; init-elfeed.el --- Elfeed configurations
;;
;; Copyright (C) 2017 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 22 February 2017
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
(use-package elfeed
  :init
  (setq elfeed-db-directory (path-join *user-elfeed-directory* "elfeeddb"))

  :config
  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening."
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun bjm/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer."
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defun squiter/elfeed-save ()
    "The function elfeed-db-save is not an interactive function."
    (interactive)
    (elfeed-db-save)
    (message "Elfeed database saved."))

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))

  (defun copy-elfeed-link (entry)
    "Copy the ENTRY URL to the clipboard."
    (interactive)
    (let* ((link (elfeed-entry-link entry)))
      (kill-new link)
      (x-set-selection 'PRIMARY link)
      (message "Yanked: %s" link)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Creates a orgmode note with entry link ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun copy-elfeed-link-title-to-org (entry)
    "Copy the ENTRY title and URL as org link to the clipboard."
    (interactive)
    (let* ((link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (titlelink (concat "[[" link "][" title "]]")))
      (when titlelink
        (kill-new titlelink)
        (x-set-selection 'PRIMARY titlelink)
        (message "Yanked: %s" titlelink))))

  (defun elfeed-show-quick-url-note ()
    "Fastest way to capture entry link to org agenda from elfeed show mode"
    (interactive)
    (copy-elfeed-link-title-to-org elfeed-show-entry)
    (org-capture nil "n")
    (yank)
    (org-capture-finalize))

  (defun elfeed-search-quick-url-note ()
    "In search mode, capture the title and link for the selected
entry or entries in org aganda."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (copy-elfeed-link-title-to-org entry)
               do (org-capture nil "n")
               do (yank)
               do (org-capture-finalize)
               (mapc #'elfeed-search-update-entry entries))
      (unless (use-region-p) (forward-line)))))

(use-package elfeed-org
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (path-join *user-elfeed-directory* "elfeed.org"))))

(use-package instapaper
  :init
  (setq instapaper-username "squiter85@gmail.com")
  (setq instapaper-password *instapaper-password*)

  :config

  (defun squiter/elfeed-show-add-to-instapaper ()
    "Save current entry in show mode to Instapaper."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (instapaper-add link)
      (message "Added to Instapaper: %s" link)))

  (defun squiter/elfeed-search-add-to-instapaper ()
    "Save current entry in search mode to Instapaper."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (instapaper-add (elfeed-entry-link entry))
               do (message "Added to Instapaper: %s" link)
               (mapc #'elfeed-search-update-entry entries))
      (unless (use-region-p) (forward-line)))))


(provide 'init-elfeed)
;;; init-elfeed.el ends here
