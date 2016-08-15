;;; init-telephone.el --- My telephone package configurations
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "vendor/git-emacs" user-emacs-directory))
(autoload 'git--update-all-state-marks "git-modeline" nil t)
(add-hook 'find-file-hook 'git-status-in-modeline t)
(add-hook 'magit-revert-buffer-hook 'git-status-in-modeline t)

(defun git-status-in-modeline ()
  (if (and vc-mode (string-match "^ Git" (substring-no-properties vc-mode)))
      (git--update-all-state-marks)))

(require 'telephone-line)

(set-face-attribute 'telephone-line-accent-active nil
                    :background "#6272a4"
                    :foreground "black"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :foreground "#6272a4"
                    :background "#2b2f3a")

(set-face-attribute 'mode-line nil
                    :box '(:line-width 1 :style raised)
                    :foreground "#6272a4"
                    :background "#282a36")

(telephone-line-defsegment* squiter/telephone-line-buffer-segment
  `("["
    ,(telephone-line-raw mode-line-modified t)
    ,"] "
    ,(telephone-line-raw mode-line-buffer-identification t)))

(telephone-line-defsegment* squiter/telephone-line-projectile-project-name
  `(""
    ,(telephone-line-raw (ignore-errors (format "📁 %s" (projectile-project-name))))))

(setq telephone-line-lhs
      '((accent . (squiter/telephone-line-buffer-segment
                   telephone-line-process-segment
                   telephone-line-erc-modified-channels-segment))
        (nil . (telephone-line-vc-segment
                squiter/telephone-line-projectile-project-name))))

(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment
                   telephone-line-position-segment))))

(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)

(setq telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)

(telephone-line-mode 1)

(provide 'init-telephone)
;;; init-telephone.el ends here
