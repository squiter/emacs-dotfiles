;;; init-company.el --- My company configuration
;;; Commentary:
;;; Code:
(use-package company :hook (after-init . global-company-mode))
(use-package company-emoji
  :hook ((after-init . company-emoji-init)
         (after-make-frame-functions . darwin-set-emoji-font))
  :config
  (defun set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs NS/Cocoa can display emoji properly."
    (if (eq system-type 'darwin)
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

  (set-emoji-font nil))

(provide 'init-company)
;;; init-company.el ends here
