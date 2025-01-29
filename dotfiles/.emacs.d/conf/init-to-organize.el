;;; init-to-organize.el --- Bootstrap for my emacs configurations
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
;; Thanks to http://github.com/rranelli/emacs-dotfile

;;; Code:

;; Theme
(use-package shades-of-purple-theme
  :config (load-theme 'shades-of-purple t))

(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :after moody
  :config (minions-mode))

(elpaca-wait)
;;; After here, everything will be loaded by demand

(use-package restart-emacs)

(use-package which-key
  :init (which-key-mode))

;; constant variable
(defconst *wakatime-osx-dir*
  (file-name-concat "/" "opt" "homebrew" "bin" "wakatime-cli")
  "Path to the wakatime binary.")

(use-package wakatime-mode
  :init
  (if (file-exists-p *wakatime-osx-dir*)
      (setq wakatime-cli-path *wakatime-osx-dir*)
    (if (file-exists-p *wakatime-dir*)
	(setq wakatime-cli-path *wakatime-dir*)
      (setq wakatime-cli-path *wakatime-nix-dir*)))
  :config
  (global-wakatime-mode))

(use-package expand-region
  :bind ("C-=" . 'er/expand-region))

(use-package ripgrep)

;; Fish Shell Path Loader
(let*
    ((fish-path (shell-command-to-string "/opt/homebrew/bin/fish -i -c \"echo -n \\$PATH[1]; for val in \\$PATH[2..-1];echo -n \\\":\\$val\\\";end\""))
     (full-path (append exec-path (split-string fish-path ":"))))
  (setenv "PATH" fish-path)
  (setq exec-path full-path))

;; Custom Functions
(defun custom/hsplit-last-buffer ()
  "Ex: | Vertically split window showing last buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun custom/vsplit-last-buffer ()
  "Ex: - Horizontally split window showing last buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun custom/swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun custom/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (move-file-to-trash filename)
        (kill-buffer buffer)
        (message "File '%s' successfully trashed" filename)))))

(defun custom/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun custom/sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun custom/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun custom/move-line (n)
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

(defun custom/move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (custom/move-line (if (null n) -1 (- n))))

(defun custom/move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (custom/move-line (if (null n) 1 n)))

;; General configs
(use-package emacs
  :ensure nil
  :config
  ;; replace marked text when type
  (delete-selection-mode 1)

  ;; move cursor by camelCase
  (subword-mode 1)

  ;; enable cameCase support for all programming modes
  (add-hook 'prog-mode-hook 'subword-mode)

  ;; make indentation commands use space only
  (setq-default indent-tabs-mode nil)

  ;; enable y/n answers
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-emacs 'y-or-n-p)

  (global-display-line-numbers-mode)

  ;; Highlight current line
  (global-hl-line-mode 1)

  ;; Remember where the cursor where in a buffer
  (save-place-mode 1)

  ;; Reverting Buffers when underlying file has changed
  (global-auto-revert-mode 1)
  )

;; Keybindings
;; init-edit-custom-functions.el keybinds:
(global-set-key (kbd "C-<return>")              'custom/insert-new-line)
(global-set-key [remap move-beginning-of-line] #'custom/smart-move-beginning-of-line)
(global-set-key (kbd "C-c d")                   'custom/duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-w")                 'custom/copy-line)
(global-set-key [remap kill-whole-line]        #'custom/kill-line)
(global-set-key (kbd "C-c j")                   'custom/join-line)
(global-set-key (kbd "C-c C-/")                 'custom/toggle-line-comment)
(global-set-key (kbd "M-<up>")                  'custom/move-line-up)
(global-set-key (kbd "M-<down>")                'custom/move-line-down)
(global-set-key [remap fill-paragraph]         #'endless/fill-or-unfill)
(global-set-key (kbd "C-c i")                   'custom/indent-buffer)

(global-set-key (kbd "C-x C-S-k")               'custom/delete-current-buffer-file)
(global-set-key (kbd "C-x C-r")                 'custom/rename-current-buffer-file)
(global-set-key (kbd "C-x !")                   'custom/sudo-edit)

;; window and buffer manipulation
(global-set-key (kbd "C-x |")                   'custom/vsplit-last-buffer)
(global-set-key (kbd "C-x -")                   'custom/hsplit-last-buffer)
(global-set-key (kbd "C-x =")                   'custom/swap-buffers-in-windows)
(global-set-key (kbd "M-o")                     'other-window)
(global-set-key (kbd "M-k")                     'kill-buffer)
(global-set-key (kbd "M-1")                     'delete-other-windows)
(global-set-key (kbd "M-5")                     'delete-window)

;; this is the most awkward keybind I set in my setup
(global-set-key (kbd "M-@") (lambda () (interactive)  (insert  ?€ )))
(global-set-key (kbd "M-#") (lambda () (interactive)  (insert  ?£ )))

(provide 'init-to-organize)
;;; init-to-organize.el ends here
