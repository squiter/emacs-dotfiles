;;; init-edit-custom-functions.el --- Custon functions to use when editing
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
;; Most of this code was copied from Pedro Lambert's Emacs dotfiles.
;; You can found original source code at:
;; https://github.com/p-lambert/emacs-dotfiles/blob/master/lisp/init-edit-defuns.el

;;; Code:
(defun custom/insert-new-line ()
  "Insert a new line without breaking the current one."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun custom/smart-move-beginning-of-line ()
  "Move to beginning of line or to beginning of indentation depending on POINT."
  (interactive)
  (if (= (point) (line-beginning-position))
      (back-to-indentation)
    (move-beginning-of-line nil)))

(defun custom/duplicate-current-line-or-region (arg)
  "Duplicates the current line or those covered by region ARG times."
  (interactive "p")
  (custom/with-region-info
   (let ((exit-point end)
         (region (buffer-substring-no-properties beg end)))
     (dotimes (_ arg)
       (goto-char end)
       (newline)
       (insert region)
       (setq end (point)))
     (goto-char exit-point)
     (next-line)
     (back-to-indentation))))

(defun custom/copy-line ()
  "Copy current line or those covered by a marked region."
  (interactive)
  (custom/with-region-info (kill-ring-save beg end)))

(defun custom/kill-line ()
  "Kill current line or the ones covered by a marked region."
  (interactive)
  (custom/with-region-info
   (goto-char beg)
   (kill-whole-line num-lines)
   (kill-new (custom/chomp (car kill-ring)))))

(defun custom/join-line ()
  "Join current line with the previous one or all covered by a marked region."
  (interactive)
  (custom/with-region-info
   (goto-char end)
   (dotimes (_ (max 1 (1- num-lines)))
     (join-line))))

(defun custom/toggle-line-comment ()
  "Comment or uncomment current line or the ones covered by a marked region."
  (interactive)
  (custom/with-region-info
   (comment-or-uncomment-region beg end)))

(defmacro custom/with-region-info (&rest body)
  "Evaluate BODY provinding BEG, END and NUM-LINES bindings, which represents
regions's beginning, ending and extension in lines."
  `(save-excursion
     (let (beg end num-lines)
       (if (and mark-active (> (point) (mark)))
           (exchange-point-and-mark))
       (setq beg (line-beginning-position))
       (if mark-active
           (exchange-point-and-mark))
       (setq end (line-end-position))
       (setq num-lines (max 1 (count-lines beg end)))
       ,@body)))

(defun custom/yank-and-indent ()
  "Indent and then indent newly formed region."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun custom/chomp (str)
  "Remove newline character at the the end of STR."
  (if (string-match "\n$" str) (substring str 0 -1) str))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun camelcase-to-snakecase ()
  "un-camelcase the word at point, replacing uppercase chars with
the lowercase version preceded by an underscore.

The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore.
"
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))

;; TODO: Fix this malformed arglist
;; (defun remove-duplicate-lines
;;     (replace-regexp "\\([^\n]+\n\\)\\1+" "\\1"))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(fset 'squiter/ruby-linear-modules
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ([5 14 1 134217828 4 backspace 58 58] 0 "%d")) arg)))

(provide 'init-edit-custom-functions)
;;; init-edit-custom-functions.el ends here
