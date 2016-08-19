;;; init-yasnippet.el --- Configurations for Yasnippet Package
;;; Commentary:
;;; Code:
(require 'yasnippet)

(setq yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

(yas-global-mode 1)

;; disable yasnipet for any terminal mode
(add-hook 'term-mode-hook (lambda()
                            (yas-minor-mode -1)))

(defvar custom/yas-guess-mode nil)
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

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
