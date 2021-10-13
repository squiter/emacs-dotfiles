;;; init-web-mode.el --- My web-mode configuration
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-meode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html.eex$" . web-mode)
         ("\\.css$" . web-mode))

  :config

  (defun rr/web-mode-conf ()
    (setq web-mode-extra-auto-pairs
          '(("eex"  . (("do" "end")))
            ))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-current-column-highlight t)
    (smartparens-mode 0))

  (add-hook 'web-mode-hook 'rr/web-mode-conf))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (typescript-mode . emmet-mode))
  :config
  (setq emmet-self-closing-tag-style " /")
  (add-to-list 'emmet-jsx-major-modes 'typescript-mode))

(provide 'init-web-mode)
;;; init-web-mode.el ends here
