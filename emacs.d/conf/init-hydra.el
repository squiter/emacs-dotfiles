;;; init-hydra.el --- Hydra configurations
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 19 outubro 2016
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
(defhydra hydra-launcher (:color blue :hint nil)
  "
^SQUITER HOTSPOTS:^

 Locaweb                Pessoal             Paths
------------------------------------------------------------------------
_p_: Ponto             _C_: Calendar       _d_: ~/Downloads
_c_: CI                _g_: Github         _e_: ~/.emacs.d

"
   ("p" (browse-url "https://portalrh.cservices.com.br/PortalLocaweb/"))
   ("c" (browse-url "http://ci.qaservices.locaweb.com.br/job/paas_paas/job/paas_hospedagem/"))
   ("C" (browse-url "https://www.google.com/calendar/render"))
   ("g" (browse-url "https://github.com/squiter"))
   ("d" (find-file "~/Downloads"))
   ("e" (find-file "~/.emacs.d"))
   ("q" nil "cancel" :color blue))

(defhydra hydra-org (:color teal :hint nil)
  "
ORG COMMANDS:

 Clock                  Navigation
-----------------------------------------
_cr_: Resume          _nl_: Last Stored
_co_: Out
_cg_: Go to
_ce_: Estimate

"
  ("cr" org-clock-in-last)
  ("co" org-clock-out)
  ("cg" org-clock-goto)
  ("ce" org-clock-modify-effort-estimate)
  ("nl" org-refile-goto-lacest-stored)
  ("q" nil "cancel" :color blue))

(provide 'init-hydra)
;;; init-hydra.el ends here
