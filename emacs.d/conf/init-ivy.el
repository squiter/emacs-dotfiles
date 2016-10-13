;;; init-ivy.el --- Ivy configuration
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 13 outubro 2016
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
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x C-l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(global-set-key (kbd "C-c C-r") 'ivy-resume)

;;;;;;;;;;;;;;;;;;;;;
;; Squiter Hotspot ;;
;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct hotspot-item name action)
(setq hotspot-list (list (make-hotspot-item
                       :name "Ponto"
                       :action (lambda () (browse-url "https://portalrh.cservices.com.br/PortalLocaweb/")))
                      (make-hotspot-item
                       :name "Calendar"
                       :action (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                      (make-hotspot-item
                       :name ".emacs.d"
                       :action (lambda () (find-file "~/.emacs.d/")))
                      (make-hotspot-item
                       :name "Downloads"
                       :action (lambda () (find-file "~/Downloads/")))))

(defun squiter/search-action-in-hs (name hslist)
  (if (not hslist)
      nil
    (if (string-equal name (hotspot-item-name (car hslist)))
        (hotspot-item-action (car hslist))
      (squiter/search-action-in-hs name (cdr hslist)))))

(defun squiter/hotspots ()
  (interactive)
  (ivy-read "Where do you want to go?"
            (mapcar 'hotspot-item-name hotspot-list)
            :action (lambda (chosen)
                      (funcall (squiter/search-action-in-hs chosen hotspot-list)))))

(provide 'init-ivy)
;;; init-ivy.el ends here
