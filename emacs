;; -*- mode: emacs-lisp -*-

;;; Code:
(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))

(setq rr/initialization-errors nil)

(require 'init-bootstrap)
(rr/safe-load-init-files)

;; Finish!
(message "======================================")
(message (if rr/initialization-errors
             (mapconcat #'identity rr/initialization-errors "\n")
           "All is sane, and init.el got to its end"))
(message "======================================")
;;; emacs file ends here
