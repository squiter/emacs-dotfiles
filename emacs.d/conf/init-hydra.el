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

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("e" exchange-point-and-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

(defhydra hydra-move (:body-pre (next-line))
   "Move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" bacFkward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom))

(provide 'init-hydra)
;;; init-hydra.el ends here
