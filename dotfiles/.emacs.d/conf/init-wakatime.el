;;; init-wakatime.el --- Wakatime configuration
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 21 outubro 2016
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

;;; Code:

(use-package wakatime-mode
  :init
  (if (file-exists-p *wakatime-osx-dir*)
      (setq wakatime-cli-path *wakatime-osx-dir*)
    (if (file-exists-p *wakatime-dir*)
        (setq wakatime-cli-path *wakatime-dir*)
      (setq wakatime-cli-path *wakatime-nix-dir*)))
  :config
  (global-wakatime-mode))

(provide 'init-wakatime)
;;; init-wakatime.el ends here
