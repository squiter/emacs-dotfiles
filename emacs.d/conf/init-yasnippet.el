;;; init-yasnippet.el --- Configurations for Yasnippet Package
;;; Commentary:
;;; Code:
(use-package yasnippet
  :hook (term-mode . (lambda() (yas-minor-mode -1)))

  :init
  (setq yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (defvar warning-suppress-types '(yasnippet backquote-change))
  (defvar custom/yas-guess-mode nil)

  :config
  (yas-global-mode 1)

  (defadvice yas-new-snippet (before custom/yas-guess-mode activate)
    (setq custom/yas-guess-mode (symbol-name major-mode)))

  (defun custom/yas-save-snippet ()
    "Automatically save snippet under the correct directory."
    (interactive)
    (if buffer-file-name
        (save-buffer)
      (let (mode
            snippet-name
            location
            (yas-dir (car yas-snippet-dirs)))
        (save-excursion
          (beginning-of-buffer)
          (search-forward-regexp "name: \\([a-z-_]+\\)")
          (setq snippet-name (match-string 1)))

        (setq mode (read-string "Snippet mode: " custom/yas-guess-mode))
        (setq location (format "%s/%s/%s" yas-dir mode snippet-name))

        (write-file location)))
    (yas-reload-all))

  (defun custom/yas-dired ()
    (interactive)
    (dired yas-snippet-dirs)))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
