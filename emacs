;;; emacs --- My `init.el' file
;;
;; Copyright (C) 2015 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.
;;
;;; Commentary:
;;
;; This files sets how to load my Emacs environment.

;;; Code:

;; Local Variables:
;; mode: emacs-lisp
;; End:

;; Package setup
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; Used to track startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))

;; use-package initialization
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

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
