;;; init-org-insert-image.el --- Org capture images configurations
;;
;; Copyright (C) 2017 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created:  7 julho 2017
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
;; This file is based on script created by Ben Maughan and posted in
;; http://pragmaticemacs.com/emacs/a-workflow-to-quickly-add-photos-to-org-mode-notes/

;;; Code:

;; required libraries
(require 'dash)
(require 'swiper)
(require 's)

(defun squiter/org-insert-camera-image ()
  "Select an image inside Camera Uploads directory of dropbox and insert it in org."
  (interactive)
  (let ((dir (expand-file-name "Camera Uploads" *user-dropbox-directory*)))
    (bjm/insert-image-from dir)))

(defun squiter/org-insert-pictures-image ()
  "Select an image inside Pictures directory and insert it in org."
  (interactive)
  (let ((dir (expand-file-name "Pictures" *user-home-directory*)))
    (bjm/insert-image-from dir)))

(defun squiter/image-dir-for (current-dir first-depth-dir)
  (let* ((default-image-dir (expand-file-name "images" current-dir))
        (desirable-dir (expand-file-name first-depth-dir default-image-dir)))
    (if (file-accessible-directory-p desirable-dir)
        desirable-dir
      (progn
        (mkdir desirable-dir)
        desirable-dir))))

(defun bjm/insert-image-from (image-dir)
  "Insert image from conference directory, rename and add link in current file.

The file is taken from a start directory set by
`image-dir' and moved to the current directory,
renamed and embedded at the point as an org-mode link. The user
is presented with a list of files in the start directory, from
which to select the file to move, sorted by most recent first."
  (interactive)
  (let (file-list target-dir file-list-sorted start-file start-file-full file-ext end-file end-file-base end-file-full file-number)
    ;; clean directories from list but keep times
    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes image-dir)))

    ;; get section heading and clean it up
    (setq end-file-base (s-downcase (s-dashed-words (nth 4 (org-heading-components)))))

    ;; get target directory
    (setq current-file-dir (file-name-directory (buffer-file-name)))
    (setq target-dir (squiter/image-dir-for current-file-dir end-file-base))

    ;; sort list by most recent
    ;; http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
    (setq file-list-sorted
          (mapcar #'car
                  (sort file-list
                        #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

    ;; use ivy to select start-file
    (setq start-file (ivy-read
                      (concat "Move selected file to " target-dir ":")
                      file-list-sorted
                      :re-builder #'ivy--regex
                      :sort nil
                      :initial-input nil))

    ;; add full path to start file and end-file
    (setq start-file-full
          (expand-file-name start-file image-dir))
    ;; generate target file name from current org section
    ;; (setq file-ext (file-name-extension start-file t))

    ;; my phone app doesn't add an extension to the image so I do it
    ;; here. If you want to keep the existing extension then use the
    ;; line above
    (setq file-ext ".jpg")
    ;; shorten to first 40 chars to avoid long file names
    (setq end-file-base (s-left 40 end-file-base))
    ;; number to append to ensure unique name
    (setq file-number 1)
    (setq end-file (concat
                    end-file-base
                    (format "-%s" file-number)
                    file-ext))

    ;; increment number at end of name if file exists
    (while (file-exists-p (expand-file-name end-file target-dir))
      ;; increment
      (setq file-number (+ file-number 1))
      (setq end-file (concat
                      end-file-base
                      (format "-%s" file-number)
                      file-ext))
      )

    ;; final file name including path
    (setq end-file-full
          (expand-file-name end-file target-dir))
    ;; copy file
    (copy-file start-file-full end-file-full)
    (message "copied %s to %s" start-file-full end-file-full)
    ;; insert link
    (insert (org-make-link-string (format "file:%s" end-file-full)))
    ;; display image
    (org-display-inline-images t t)))

(provide 'init-org-insert-image)
;;; init-org-insert-image.el ends here
