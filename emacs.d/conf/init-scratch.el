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
(defvar welcome-message '(";;; Bem vindo Squiter 🐱"))

(defvar keybindings-file (concat *emacsd-directory* "/conf" "/init-keybindings.el"))

(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun is-comment (st)
  "Check if ST start with ;."
  (equal (substring st 0 1) ";"))

(defun get-random-binding (filename)
  "Get a random non commented line of FILENAME."
  (let ((valid-lines (cl-remove-if 'is-comment (read-lines filename))))
    (nth (random (- (length valid-lines) 2)) valid-lines)))

(defun get-binding-message ()
  "This function format the message to append in scratch."
  (let ((random-binding (get-random-binding keybindings-file)))
    (concat ";;; Binding do dia: " random-binding)))

(add-to-list 'welcome-message (get-binding-message))
(add-to-list 'welcome-message "\n")

(setq initial-scratch-message (mapconcat 'identity (reverse welcome-message) "\n"))

(provide 'init-scratch)
;;; init-scratch.el ends here
