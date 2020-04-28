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

;;;;;;;;;;;;;;;;;;;
;; hydra-laucher ;;
;;;;;;;;;;;;;;;;;;;
(use-package hydra
  :init (define-prefix-command 'squiter/hydra-map)
  :bind (("<f6>" . 'squiter/hydra-map)
         :map squiter/hydra-map
         ("l" . 'hydra-launcher/body)
         ("o" . 'hydra-org/body)
         ("r" . 'hydra-rectangle/body)
         ("m" . 'hydra-move/body)
         ("e" . 'mz/make-and-run-elfeed-hydra)
         ("c" . 'squiter/hydra-edit-conf/body)
         ("[" . 'hydra-smartparens/body))

  :config
  (defhydra hydra-launcher (:color blue :hint nil)
    "
^SQUITER HOTSPOTS:^

 Locaweb                  Pessoal               Paths
--------------------------------------------------------------------------
_p_: Ponto               _C_: Calendar         _D_: ~/Downloads
_c_: CI                  _g_: Github           _e_: ~/.emacs.d
_m_: Merge Requests      _f_: Elfeed           _E_: ../elfeed.org
_r_: Retro               _x_: Exist.io         _l_: ../ledger.beancount
_d_: Docs Hospedagem
--------------------------------------------------------------------------
"
    ("p" (browse-url "https://portalrh.cservices.com.br/PortalLocaweb/"))
    ("c" (browse-url "http://ci.qaservices.locaweb.com.br/job/paas_paas/job/paas_hospedagem/"))
    ("m" (browse-url "https://code.locaweb.com.br/dashboard/merge_requests?scope=all&state=opened&utf8=%E2%9C%93&label_name%5B%5D=novo-provisionamento"))
    ("r" (browse-url "https://maps.groupmap.com/maps"))
    ("d" (browse-url "http://apps-hospedagem.docs.arda.locaweb.com.br"))
    ("C" (cfw:open-all-calendars))
    ("g" (browse-url "https://github.com/squiter"))
    ("x" (browse-url (format "https://exist.io/mood/timeline/edit/%s/" (format-time-string "%Y-%m-%d"))))
    ("D" (find-file "~/Downloads"))
    ("e" (find-file "~/.emacs.d"))
    ("E" (find-file "~/Dropbox/elfeed/elfeed.org"))
    ("l" (find-file "~/Dropbox/ledger/ledger.beancount"))
    ("f" (bjm/elfeed-load-db-and-open))
    ("q" nil "cancel" :color blue))


;;;;;;;;;;;;;;;
  ;; hydra-org ;;
;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;
  ;; hydra-rectangle ;;
;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;
  ;; hydra-move ;;
;;;;;;;;;;;;;;;;


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

  (defun reload-project-links ()
    (eval `(defhydra hydra-project-launcher (:column 3)
             "Fast links by projects"
             ,@(mapcar (lambda (x)
                         (list (car x) `(browse-url ,(cadr x)) (caddr x)))
                       (mapcar (lambda (line)
                                 (split-string line " - "))
                               (read-lines (concat (projectile-project-root) ".hydra-links")))))))

  (add-hook 'projectile-after-switch-project-hook #'reload-project-links)

;;;;;;;;;;;;;;;;;;
  ;; elffed hydra ;;
;;;;;;;;;;;;;;;;;;

  (defun z/hasCap (s)
    "Check if the string S has an upcase char."
    (let ((case-fold-search nil))
      (string-match-p "[[:upper:]]" s)))

  (defun z/get-hydra-option-key (s)
    "Return single upper case letter (converted to lower) or first using string S."
    (interactive)
    (let ( (loc (z/hasCap s)))
      (if loc
	  (downcase (substring s loc (+ loc 1)))
        (substring s 0 1))))

  (defun mz/make-elfeed-cats (tags)
    "Return a list of lists.  Each line is like (c function hint).  Use TAGS to create it."
    (interactive)
    (mapcar (lambda (tag)
	      (let* ((tagstring (symbol-name tag))
		     (c (z/get-hydra-option-key tagstring)))
	        (list c (append '(elfeed-search-set-filter)
                                (list (format "@6-months-ago +%s" tagstring) )) tagstring)))
	    tags))

  (defun mz/make-and-run-elfeed-hydra ()
    "Build the tag list and call hydra body."
    (interactive)
    (mz/make-elfeed-hydra)
    (mz/hydra-elfeed/body))

  (defmacro mz/make-elfeed-hydra ()
    `(defhydra mz/hydra-elfeed (:columns 4)
       "filter"
       ,@(mz/make-elfeed-cats (elfeed-db-get-all-tags))
       ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
       ("M" elfeed-toggle-star "Mark")
       ("A" (elfeed-search-set-filter "@6-months-ago") "All")
       ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
       ("Q" bjm/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
       ("q" nil "quit" :color blue)))


;;;;;;;;;;;;;;;;;;;;;;
  ;; Org-refile hydra ;;
;;;;;;;;;;;;;;;;;;;;;;

  (defmacro josh/make-org-refile-hydra (hydraname file keyandheadline)
    "Make a hydra named HYDRANAME with refile targets to FILE.
KEYANDHEADLINE should be a list of cons cells of the form (\"key\" . \"headline\")"
    `(defhydra ,hydraname (:color blue :after-exit
                                  (unless (or hydra-deactivate
                                              current-prefix-arg) ;If we're just jumping to a location, quit the hydra
                                    (squiter/org-refile-hydra/body)))
       ,file
       ,@(cl-loop for kv in keyandheadline
		  collect (list (car kv) (list 'josh/refile file (cdr kv) 'current-prefix-arg) (cdr kv)))
       ("q" nil "cancel")))

  (josh/make-org-refile-hydra squiter/org-refile-hydra-master
                              "master.org"
                              (("l" . "Learning Tasks")
                               ("p" . "Podcasts")
                               ("m" . "Manutenção de Coisas")
                               ("a" . "Ambiente de Desenvolvimento")
                               ("b" . "Bills")
                               ("n" . "Notes")
                               ("t" . "Tasks")))

  (josh/make-org-refile-hydra squiter/org-refile-hydra-nubank
                              "nubank.org"
                              (("t" . "Tasks")
                               ("n" . "Notes")))

  (josh/make-org-refile-hydra squiter/org-refile-hydra-geloefogo
                              "geloefogo.org"
                              (("t" . "Tasks")))

  (josh/make-org-refile-hydra squiter/org-refile-hydra-crypt
                              "crypt.org"
                              (("n" . "notes")))

  (defhydra squiter/org-refile-hydra (:foreign-keys run)
    "Refile"
    ("m" squiter/org-refile-hydra-master/body "master.org" :exit t)
    ("n" squiter/org-refile-hydra-nubank/body "nubank.org" :exit t)
    ("g" squiter/org-refile-hydra-geloefogo/body "geloefogo.org" :exit t)
    ("k" squiter/org-refile-hydra-crypt/body "crypt.org" :exit t)
    ("q" nil "cancel"))

  (defhydra squiter/hydra-edit-conf (:hint nil :columns 3)
    "Edit system configuration files (Ubuntu)"
    ("d" (find-file "~/.local/share/applications/") ".desktop files in Home")
    ("D" (find-file "/usr/local/share/applications/") ".desktop files in /usr")
    ("i" (find-file "~/.config/i3/config") "i3 config")
    ("3" (find-file "~/.config/i3status/config") "i3 status config")
    ("s" (find-file "/sudo::/etc/apt/sources.list") "Package source list")
    ("S" (find-file "/sudo::/etc/apt/sources.list.d") "Package source dir for ppas")
    ("h" (find-file "/sudo::/etc/hosts") "/etc/hosts")
    ("r" (find-file "/sudo::/etc/resolv.conf") "/etc/resolv.conf")
    ("cr" (find-file "/sudo::/etc/cgrules.conf") "Cgroups rules")
    ("cc" (find-file "/sudo::/etc/cgconfig.conf") "Cgroups config")
    ("q" nil "quit" :color red))

  (defhydra hydra-smartparens (:hint nil)
  "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)

  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)

  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil)))

(provide 'init-hydra)
;;; init-hydra.el ends here
