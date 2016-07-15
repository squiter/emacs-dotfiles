(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete)


(defmacro do-not-query-process-kill (function-name process-name)
  "Do not query process kill for FUNCTION-NAME that spawns process
PROCESS-NAME."
  `(defadvice ,function-name (after do-not-query-shell-exit
                                    first (&optional buffer)
                                    activate)
     (interactive)
     "Do not query exit confirmation for shell process buffer."
     (let* ((processes (remove-if-not
                        (lambda (process) (string-match-p ,process-name (process-name process)))
                        (process-list))))
       (dolist (p processes)
         (set-process-query-on-exit-flag p nil)))))

(do-not-query-process-kill shell "shell")
(do-not-query-process-kill term "terminal")


(defun new-term (arg)
  "Create a new terminal giving it a nice name.
If ARG is present, open a new term regardless."
  (interactive "P")
  (let* ((custom-name (if arg
                          (format "[%s]" (read-string "Terminal name: "))
                        ""))
         (term-name (format "ansi-term: %s %s" (rr/shell-project-name) custom-name))
         (shell-exists-p (bufferp (get-buffer term-name))))

    (if (not shell-exists-p)
        (progn (ansi-term "/bin/bash")
               (rename-buffer term-name)
               (term-line-mode)
               (goto-char (point-max))
               (insert (format "cd %s # [Enter] cds to root" (rr/shell-wd)))
               (term-char-mode)
               )
      (switch-to-buffer term-name))))

(defun rr/shell-project-name ()
  (file-name-base (directory-file-name (rr/shell-wd))))

(defun rr/shell-wd ()
  (if (rr/in-project)
      (projectile-project-root)
    default-directory))

(defun rr/in-project ()
  (stringp (projectile-project-root)))

(add-hook 'term-mode-hook
          (lambda ()
            (expose-bindings term-raw-map
                             (->> bindings-to-expose
                                  (remove "C-h")
                                  (remove "M-h")))))

(global-set-key (kbd "C-c t") 'new-term)

(provide 'init-shell)
