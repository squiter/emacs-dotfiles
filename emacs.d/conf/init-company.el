;;; init-company.el --- My company configuration
;;; Commentary:
;;; Code:
(require 'company-emoji)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'company-emoji-init)

(defun darwin-set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs NS/Cocoa can display emoji properly."
  (if (eq system-type 'darwin)
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)))

;; For when emacs is started with Emacs.app
(darwin-set-emoji-font nil)

;; Hook for when a cocoa frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions 'darwin-set-emoji-font)

(provide 'init-company)
;;; init-company.el ends here
