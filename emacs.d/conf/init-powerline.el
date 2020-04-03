;;; init-powerline.el --- Powerline configuration
;;
;; Copyright (C) 2019 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 24 January 2019
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
;; Commentary

;;; Code:

(use-package eyebrowse
  :bind (("C-c C-w k" . eyebrowse-close-window-config)
         ("C-c C-w l" . eyebrowse-last-window-config))
  :config (eyebrowse-mode t))

(use-package powerline
  :ensure t
  :init (use-package eyebrowse)
  :config

  (defun make-rect (color height width)
    "Create an XPM bitmap."
    (when window-system
      (propertize
       " " 'display
       (let ((data nil)
             (i 0))
         (setq data (make-list height (make-list width 1)))
         (pl/make-xpm "percent" color color (reverse data))))))


  (defun powerline-mode-icon ()
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (format " %s"
                (propertize icon
                            'help-echo (format "Major-mode: `%s`" major-mode)
                            'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))


  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (modified (buffer-modified-p))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (bar-color (cond ((and active modified) (face-foreground 'error))
                                           (active (face-background 'cursor))
                                           (t (face-background 'tooltip))))
                          (lhs (list
                                (make-rect bar-color 30 3)
                                (when modified
                                  (concat
                                   " "
                                   (all-the-icons-faicon "floppy-o"
                                                         :face (when active 'error)
                                                         :v-adjust -0.01)))
                                " "
                                (powerline-buffer-id)
                                ))
                          (center (list
                                   " "
                                   (powerline-mode-icon)
                                   " "
                                   (powerline-major-mode)
                                   " "))
                          (rhs (list
                                (format "%s" (eyebrowse--get 'current-slot))
                                " | "
                                (powerline-raw "%l:%c" 'mode-line 'r)
                                " | "
                                (powerline-raw "%6p" 'mode-line 'r)
                                (powerline-hud 'highlight 'region 1)
                                " "
                                ))
                          )
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                      (powerline-render center)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs)))))))

(provide 'init-powerline)
;;; init-powerline.el ends here
