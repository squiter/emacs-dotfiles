;;; init-google.el --- My Google configurations
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
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
;; This file configure `google-this' and `google-translate'

;;; Code:

(use-package google-this
  :defer t
  :config (google-this-mode 1))

(use-package google-translate
  :defer t
  :init
  (setq google-translate-translation-directions-alist
        '(("en" . "pt") ("pt" . "en")))
  :bind ("C-x / t" . google-translate-smooth-translate))

;; google-this and google-translate
;; (global-set-key (kbd "C-x g") 'google-this-mode-submap)
;; (global-set-key [remap google-this] 'google-translate-smooth-translate)

(provide 'init-google)
;;; init-google.el ends here.
