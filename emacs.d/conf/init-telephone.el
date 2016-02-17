;;; init-telephone.el --- My telephone package configurations
;;; Commentary:
;;; Code:

(require 'telephone-line)

(set-face-attribute 'telephone-line-accent-active nil
                    :background "#8bc34a"
                    :foreground "black"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :foreground "#8bc34a")

(set-face-attribute 'mode-line nil
                    :box '(:line-width -1 :style raised)
                    :foreground "#8bc34a")

(telephone-line-defsegment* rr/telephone-line-buffer-segment
  `("" mode-line-remote " "
    ,(telephone-line-raw mode-line-buffer-identification t)))

(telephone-line-defsegment* rr/telephone-line-projectile-project-name
  `(""
    ,(telephone-line-raw (ignore-errors (format "📁 %s" (projectile-project-name))))))

(setq telephone-line-lhs
      '((accent . (rr/telephone-line-buffer-segment
                   telephone-line-process-segment
                   telephone-line-erc-modified-channels-segment))
        (nil . (telephone-line-vc-segment
                rr/telephone-line-projectile-project-name))))

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
