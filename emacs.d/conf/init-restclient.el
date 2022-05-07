;;; init-restclient.el --- My restclient configurations
;;; Commentary:
;;; Code:

(use-package restclient
  :mode ("\\.restclient$" . restclient-mode)
  :bind (:map restclient-mode-map
              ("M-n" . restclient-jump-next)
              ("M-p" . restclient-jump-prev)))

(use-package restclient-jq :after restclient)

(provide 'init-restclient)
;;; init-restclient.el ends here
