;;; init-custom-functions.el --- My functions to edit files
;;; Commentary:
;;; Code:

(defun custom/get-region-positions ()
  "Returns a dotted-pair (BEG . END) with regions's beginning and ending positions."
  (interactive)
  (save-excursion
    (let (beg end)
      (if (and mark-active (> (point) (mark)))
          (exchange-point-and-mark))
      (setq beg (line-beginning-position))
      (if mark-active
          (exchange-point-and-mark))
      (setq end (line-end-position))
      (cons beg end))))

(defun hsplit-last-buffer ()
  "Ex: | Vertically split window showing last buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun vsplit-last-buffer ()
  "Ex: - Horizontally split window showing last buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun switch-fullscreen nil
  (interactive)
  (let* ((modes '(nil fullboth fullwidth fullheight))
         (cm (cdr (assoc 'fullscreen (frame-parameters) ) ) )
         (next (cadr (member cm modes) ) ) )
    (modify-frame-parameters
     (selected-frame)
     (list (cons 'fullscreen next)))))

(defun delete-current-buffer-file ()
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

(defun rename-current-buffer-file ()
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

(defun toggle-window-split ()
  "Toggle h to v or v to h splits."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first.  Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun insert-today ()
  (interactive)
  (insert-current-date))

(fset 'squiter/org-attach-path-to-img
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ("\344\344\344\344.[[]]" 0 "%d")) arg)))

(defun squiter/say-it ()
  "Use espeak to say the selected text."
  (interactive)
  (call-process "/usr/bin/espeak" nil t nil (buffer-substring (mark) (point))))

(defun squiter/list-ebooks ()
  "list everything recursively"
  (interactive)
  (let* ((cands
          (split-string
           (shell-command-to-string (concat "find " *user-ebook-directory*)) "\n" t)))
    (ivy-read "File: " cands
              :action #'find-file
              :caller 'fhd/counsel-everything)))

(provide 'init-custom-functions)
;;; init-custom-functions.el ends here
