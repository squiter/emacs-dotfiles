;;; init-scratch.el --- My custom scratch message
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
;; This file will make your scratch buffer more fun!

;;; Code:
(defvar welcome-message '(";;; Welcome Squiter 🐱"))

;; TODO: Try to get this bindings from `describe-personal-keybinds`
(defvar keybindings-file (concat *emacsd-directory* "/conf" "/init-keybindings.el"))
(defvar tips-file (concat *emacsd-directory* "/pragmatic-tips.txt"))

(defun is-comment (st)
  "Check if ST start with ;."
  (equal (substring st 0 1) ";"))

(defun get-random-line (filename)
  "Get a random non commented line of FILENAME."
  (let ((valid-lines (cl-remove-if 'is-comment (read-lines filename))))
    (nth (random (- (length valid-lines) 2)) valid-lines)))

(defun build-message (prefix filename)
  "This function format the message to append in scratch.
It use PREFIX message and a random line of FILENAME."
  (let ((random-binding (get-random-line filename)))
    (concat prefix random-binding)))

(add-to-list 'welcome-message (build-message ";;; Binding of the day: " keybindings-file))
(add-to-list 'welcome-message (build-message ";;; Tip of the day: " tips-file))
(add-to-list 'welcome-message "\n")

(setq initial-scratch-message (mapconcat 'identity (reverse welcome-message) "\n"))

(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 3)
  (dashboard-setup-startup-hook))

(provide 'init-scratch)
;;; init-scratch.el ends here
