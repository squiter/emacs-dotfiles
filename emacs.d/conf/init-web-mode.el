;;; init-web-mode.el --- My web-mode configuration
;;; Commentary:
;;; Code:

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.eex$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))

(defun rr/web-mode-conf ()
  (setq web-mode-extra-auto-pairs
        '(("eex"  . (("do" "end")))
          ))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (smartparens-mode -1))

(add-hook 'web-mode-hook 'rr/web-mode-conf)

(provide 'init-web-mode)
;;; init-web-mode.el ends here
