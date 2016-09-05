;;; init-org-babel.el --- My configurations for Babel
;;; Commentary:
;;  Stolen from: https://github.com/rranelli/emacs-dotfiles
;;; Code:
;; Setting up babel support for languages
(setq org-babel-sh-command "bash"
      org-export-babel-evaluate nil)

(setq org-babel-clojure-backend 'cider)
(require 'cider)

;; racket configuration
(add-to-list 'load-path (expand-file-name "vendor/ob-racket" user-emacs-directory))
(setq org-babel-racket-command "/usr/bin/racket")

(setq org-src-fontify-natively t)
(org-babel-do-load-languages'org-babel-load-languages
 '((emacs-lisp . t)
   (dot . t)
   (python . t)
   (ruby . t)
   (haskell . t)
   (java . t)
   (clojure . t)
   (sh . t)
   (org . t)
   (latex . t)
   (sql . t)
   (sml . t)
   (racket . t)
   (restclient . t)))

(provide 'init-org-babel)
;;; init-org-babel.el ends here
