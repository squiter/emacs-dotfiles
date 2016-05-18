;; Org Capture
(setq org-directory *user-org-cache-directory*)
(setq org-default-notes-file (path-join *user-org-cache-directory* "refile.org"))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file (path-join *user-org-cache-directory* "refile.org"))
               "* TODO %?\n%a\n%U" :clock-in t :clock-resume t)
              ("n" "note" entry (file (path-join *user-org-cache-directory* "refile.org"))
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree (path-join *user-org-cache-directory* "diary.org"))
               "* %?\n%(oc/inc \"Things that I learned\" \"** Three things that I learn today\n\")" :clock-in t :clock-resume t)
              ("s" "Code Snippet" entry
               (file (path-join *user-org-cache-directory* "snippets.org"))
               ;; Prompt for tag and language
               "* %? :NOTE:\t\n%U\n#+BEGIN_SRC %^{language}\n%c\n#+END_SRC")
              ("w" "org-protocol" entry (file (path-join *user-org-cache-directory* "refile.org"))
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("h" "Habit" entry (file (path-join *user-org-cache-directory* "refile.org"))
               "* NEXT %?\nSCHEDULED: %<<%Y-%m-%d %a .+1d/3d>>\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%U\n%a\n"))))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; http://storax.github.io/blog/2016/05/02/org-capture-tricks/
;; Use user input in severall locations
(defvar oc-capture-prmt-history nil
  "History of prompt answers for org capture.")
(defun oc/prmt (prompt variable)
  "PROMPT for string, save it to VARIABLE and insert it."
  (make-local-variable variable)
  (set variable (read-string (concat prompt ": ") nil oc-capture-prmt-history)))

;; Conditionally insert text
(defun oc/inc (what text &rest fmtvars)
  "Ask user to include WHAT.  If user agrees return TEXT."
  (when (y-or-n-p (concat "Include " what "?"))
    (apply 'format text fmtvars)))

;; Thanks for @pashini : https://github.com/pashinin/emacsd/blob/master/elisp/init-org-capture.el
;; Org - Capture
(require 'remember)
;; make the frame contain a single window. by default org-remember
;; splits the window.
(add-hook 'remember-mode-hook 'delete-other-windows)

;;---------------------
;; http://www.windley.com/archives/2010/12/capture_mode_and_emacs.shtml
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)
    ))

(defun make-remember-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  ;; if there are no emacs frames - make frame
  (if (= (length '(frame-list)) 0)
      (progn
        (make-frame '((name . "capture")
                      (width . 90)
                      (height . 20)
                      ;;(auto-raise . t)
                      )))
    (progn
      (rename-frame (selected-frame) "capture")  ; install frame-cmds from melpa for this
      ))
  (select-frame-by-name "capture")
  (set-frame-width  (selected-frame) 90)
  (set-frame-height (selected-frame) 20)
  (setq word-wrap      1)
  (setq truncate-lines nil)
  (org-capture))

(provide 'init-org-captures)
