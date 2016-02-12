;;; init-artist-mode.el --- My custom configuration for artist-mode
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
;; This file contains some configurations to use artist-mode in Emacs

;;; Code:

(global-set-key (kbd "C-<f1>") (lambda()
                                 (interactive)
                                 (show-all)
                                 (artist-mode)))

(add-hook 'artist-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
            (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
	    (local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
	    (local-set-key (kbd "<f5>") 'artist-select-op-ellipse)  ; f5 = ellipse
	    (local-set-key (kbd "C-z") 'undo)
            ))

(provide 'init-artist-mode)
;;; init-artist-mode.el ends here
