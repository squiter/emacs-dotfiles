;;;; -*- coding: utf-8-unix -*-
;;;; org-gamify.el -- persistent currencies which tasks can modify
;;;;
;;;; Author: Paul Sexton <eeeickythump@gmail.com>
;;;; Version: 1.0
;;;; Repository at http://bitbucket.org/eeeickythump/org-gamify/
;;;;
;;;;
;;;; What This Does
;;;; ==============
;;;;
;;;; The user defines one or more persistent numerical variables
;;;; ("currencies"). The value (balance) of each currency will be saved across
;;;; sessions.
;;;;
;;;; Tasks can alter currency balances upon completion. The
;;;; details of which balances are altered, and by how much, can be specified
;;;; either per-task by setting a property, or more generally via hook
;;;; functions.
;;;;
;;;; It is possible for currencies to have minimum or maximum allowed values,
;;;; and these limits can be strictly enforced, in which case a task will be
;;;; blocked from completing if this would result in a currency balance going
;;;; outside its allowed limits.
;;;;
;;;; Every active task in the agenda is inspected exactly once per day -- this
;;;; allows the user to define currency-related consequences if a task becomes
;;;; very overdue, or a habit is not maintained (or is very well maintained),
;;;; for example.
;;;;
;;;; Interactive commands exist to:
;;;; - List all currencies and their balances in a temporary buffer.
;;;; - Display the currency changes that would occur if the task at point were
;;;;   marked 'done'.
;;;;
;;;; See the file README.org for more detailed documentation.
;;;;
;;;; TODO
;;;; ----
;;;;
;;;; - Use icons in org-gamify-show-inventory.
;;;; - Improve messages and interactive commands.
;;;; - Message in echo area is immediately overwritten by other messages about
;;;;   changes in the log for the task. There is no obvious way to get around
;;;;   this.
;;;;
;;;; Ideas for Later
;;;; ---------------
;;;;
;;;; Minor mode to display some info in modeline? (currency balances etc)
;;;;
;;;; Integrate with org-protocol
;;;; - link to increment/decrement a specified currency
;;;; - link to mark a todo item done
;;;;


(require 'cl-macs)
(require 'org)
(require 'org-agenda)
(require 'org-habit)
(require 'savehist)

(eval-when-compile
  (require 'cl))


;;; * Customisations and variables


(defcustom org-gamify-currencies nil
  "Alist of (NAME . PLIST) where NAME is a symbol that uniquely identifies
the currency. See `define-gamify-currency' for allowed plist keys."
  :group 'org-gamify
  :type '(alist :key-type symbol :value-type plist))


(defcustom org-gamify-inventory nil
  "Alist of (NAME . PLIST) where NAME is a symbol that uniquely identifies
the currency. NAME should match the name of an entry in `org-gamify-currencies'.
PLIST can contain the keywords:
- :balance NUMBER -- the balance for this currency.
- :date DAYS -- date when balance was last altered, in the form of number of
  days since beginning of the epoch."
  :group 'org-gamify
  :type '(alist :key-type symbol :value-type plist))


(defcustom org-gamify-last-update-day nil
  "The day since the beginning of the epoch when the last daily update scan was performed.
This should not be edited by the user."
  :group 'org-gamify
  :type 'number)


(defcustom org-gamify-message-delay 2.5
  "Number of seconds to wait while displaying org-gamify messages in the
echo area."
  :group 'org-gamify
  :type 'number)


(defcustom org-gamify-use-alert-p nil
  "If non-nil, use the function `alert' (https://github.com/jwiegley/alert)
instead of `message' to alert the user to currency changes. Alert
automatically uses dbus, Growl, or libnotify if available to produce
messages."
  :group 'org-gamify
  :type 'boolean)


(defvar org-gamify-delta-cache (make-hash-table :test 'equal)
  "Hashtable that maps task IDs to alists of currency deltas.")


(defvar org-gamify-delta-cache-day-last-updated 0
  "Day when the cache of deltas was last updated.")


(defvar org-gamify-active-p t
  "If this variable is set to nil, changes in the todo state of a task will not
trigger currency changes.")


;;; * Variables that can be used to customise behaviour


(defvar org-gamify-daily-update-functions nil
  "List of functions. Each must take the arguments (SCHEDULE DEADLINE CHAIN),
where SCHEDULE is the number of days elapsed between today and the day on
which the task is scheduled; DEADLINE is the number of days past the deadline
for the task; and CHAIN is the number of days that the chain has been
maintained.
 The functions will be run in turn until one of them returns non-nil.")


(defvar org-gamify-delta-functions nil
    "List of functions. Each must take the arguments (SCHEDULE DEADLINE CHAIN),
where SCHEDULE is the number of days elapsed between today and the day on
which the task is scheduled; DEADLINE is the number of days past the deadline
for the task; and CHAIN is the number of days that the chain has been
maintained.
The functions will be run in turn until one of them returns non-nil. Each
function must either return nil, or a list of currency deltas which will be
used as the deltas for the current item.")



;;; * Persistence of variables across sessions


(add-to-list 'savehist-additional-variables 'org-gamify-currencies)
(add-to-list 'savehist-additional-variables 'org-gamify-inventory)
(add-to-list 'savehist-additional-variables 'org-gamify-last-update-day)
(unless savehist-mode
  (savehist-mode 1))


;;; * Currency definitions


(defmacro define-gamify-currency (name &rest args)
  "Define a currency, or replace an existing definition.
NAME is a symbol that will uniquely identify this currency.
The rest of the arguments are keyword-value pairs.
Allowable keywords are:
- :name STRING -- optional human-readable version of the currency's name.
- :category STRING -- currencies are grouped by category when the inventory
  is displayed in a buffer.
- :min NUMBER -- the currency's balance cannot go below this number.
- :max NUMBER -- the currency's balance cannot go above this number.
- :enforce-min nil/truncate/block
- :enforce-max nil/truncate/block
- :hide-amount t/nil -- if non-nil, the currency's balance is never displayed,
  rather it is reported as present if greater than zero, and absent/hidden
  otherwise.
- :after-increase-function FUNCTION -- function that will be called after
  the currency's balance has been increased. It must take one numerical
  argument, the amount of the increase.
- :after-decrease-function FUNCTION -- function that will be called after
  the currency's balance has been decreased. It must take one numerical
  argument, the amount of the decrease (always negative).
- :balance-function FUNCTION -- function used to produce a human-readable
  version of the currency's balance. It must take one argument (the balance)
  and must return a string.
- :fg-color COLOR -- used when displaying the currency's name.
- :bg-color COLOR -- used when displaying the currency's name.
- :icon FILENAME -- used when the currency is listed in inventory."
  (let ((entry (gensym)))
    `(let ((,entry (assoc ',name org-gamify-currencies)))
       (if ,entry
           (rplacd ,entry ',args)
         (push (cons ',name ',args) org-gamify-currencies)))))


(defun org-gamify-get-currency-plist (currency)
  (cdr (assoc currency org-gamify-currencies)))


;;; * Inventory


(defun org-gamify-save-inventory ()
  (savehist-autosave))


(defun org-gamify-get-inventory-plist (currency)
  (cdr (assoc currency org-gamify-inventory)))


(defun org-gamify-get-currency-balance (currency)
  (plist-get (org-gamify-get-inventory-plist currency) :balance))


(defun org-gamify-set-currency-balance (currency balance)
  (plist-put (org-gamify-get-inventory-plist currency) :date
             (time-to-days (current-time)))
  (plist-put (org-gamify-get-inventory-plist currency) :balance balance))


;;; * Tasks

(defun org-gamify-get-scheduled-day ()
  (time-to-days (org-get-scheduled-time (point))))
(defun org-gamify-get-deadline-day ()
  (time-to-days (org-get-deadline-time (point))))
(defun org-gamify-get-done-dates ()
  (if (org-is-habit-p)
      (org-habit-done-dates (org-habit-parse-todo))))


;; Any function which needs to access (inspect or use) the deltas for a task
;; must go through this function.
(defun org-gamify-get-deltas (&optional ignore-cache)
  "Return the currency deltas for the task at point. Returns nil if the entry at
point is not a task (does not have a todo keyword).
Return value is a list whose entries take the form (CURRENCY DELTA) where
CURRENCY is a symbol identifying a currency, and DELTA is a positive or
negative number."
  (cond
   ((null (org-get-todo-state))
    nil)
   (t
    (when (and (not ignore-cache)
               (< org-gamify-delta-cache-day-last-updated
                  (time-to-days (current-time))))
      ;; Clear cache if it's not been updated today.
      (setq org-gamify-delta-cache-day-last-updated (time-to-days (current-time)))
      (clrhash org-gamify-delta-cache))
    ;; If a result is cached, return it, else call the user functions in
    ;; `org-gamify-delta-functions' until one of them returns something. If
    ;; none of those return anything, retrieve the CURRENCY_DELTAS or
    ;; DEFAULT_CURRENCY_DELTAS property and return it.
    (let ((today (time-to-days (current-time)))
          (scheduled (org-gamify-get-scheduled-day))
          (deadline (org-gamify-get-deadline-day)))
      (cond
       (ignore-cache
        (or (run-hook-with-args-until-success
             'org-gamify-delta-functions
             (if scheduled (- today scheduled) nil)
             (if deadline (- today deadline) nil)
             (org-gamify-habit-chain-length))
            (org-gamify-get-raw-deltas)))
       (t
        (let ((id (org-id-get (point) t)))
          (gethash id org-gamify-delta-cache
                   ;; Default -- retrieve [DEFAULT_]CURRENCY_DELTAS property,
                   ;; store it in the hashtable, and return it.
                   (progn
                     (puthash id
                              (or (run-hook-with-args-until-success
                                   'org-gamify-delta-functions
                                   (if scheduled (- today scheduled) nil)
                                   (if deadline (- today deadline) nil)
                                   (org-gamify-habit-chain-length))
                                  (org-gamify-get-raw-deltas))
                              org-gamify-delta-cache))))))))))


;; This should only be called by `org-gamify-get-deltas'.
(defun org-gamify-get-raw-deltas ()
  (let ((deltas (or (org-entry-get (point) "CURRENCY_DELTAS")
                    (org-entry-get (point) "DEFAULT_CURRENCY_DELTAS" t))))
    (and deltas (read deltas))))


(defun org-gamify-get-delta (currency)
  "Return the change in currency CURRENCY that would occur if this task were
marked as done."
  (let ((deltas (org-gamify-get-deltas)))
    (second (assoc currency deltas))))


(defun org-gamify-apply-deltas (task-plist &optional invert)
  "Apply the currency deltas for the task at point. This function is called
when a task is marked 'done'.
If 'invert' is non-nil, the deltas are all multiplied by -1 before being
applied."
  ;; We must check that the new state is a 'done' state, because when repeating
  ;; items are marked done, they immediately revert to 'todo', creating a total
  ;; of 2 state changes. We must only apply deltas ONCE for repeating items.
  (when org-gamify-active-p
    (let ((to-state (plist-get task-plist :to))
          (from-state (plist-get task-plist :from)))
      (when (and (member from-state (cons 'todo org-not-done-keywords))
                 (member to-state (cons 'done org-done-keywords)))
        (let ((deltas (org-gamify-get-deltas))
              (id (org-id-get (point) t)))
          (dolist (entry deltas)
            (destructuring-bind (currency delta) entry
              (org-gamify-alter-currency currency (if invert (- delta) delta))))
          (remhash id org-gamify-delta-cache)
          deltas)))))


(add-hook 'org-trigger-hook 'org-gamify-apply-deltas)


(defun org-gamify-alter-currency (currency amt &optional error-if-blocked-p)
  "Alter the balance of currency CURRENCY by amount AMT (a positive or
negative number).
If ERROR-IF-BLOCKED-P is non-nil, raise an error if the transaction would
put the currency's balance outside the allowed range."
  (let* ((inventory-plist (org-gamify-get-inventory-plist currency))
         (info (org-gamify-get-currency-plist currency))
         (balance (or (plist-get inventory-plist :balance) 0))
         (new-balance nil))
    (unless (or inventory-plist (assoc currency org-gamify-inventory))
      (setq inventory-plist (list :balance 0 :date (time-to-days (current-time))))
      (push (cons currency inventory-plist) org-gamify-inventory))
    (setq new-balance
          (cond
           ((and (plist-get info :min)
                 (minusp amt)
                 (< (+ balance amt) (plist-get info :min)))
            (cond
             ((and (eql 'block (plist-get info :enforce-min))
                   error-if-blocked-p)
              (error "Tried to decrease balance of %s below allowed minimum of %d."
                     currency (plist-get info :min)))
             ((eql 'truncate (plist-get info :enforce-min))
              (max (plist-get info :min) (+ balance amt)))
             (t
              (+ balance amt))))
           ((and (plist-get info :max)
                 (plusp amt)
                 (> (+ balance amt) (plist-get info :max)))
            (cond
             ((and (eql 'block (plist-get info :enforce-max))
                   error-if-blocked-p)
              (error "Tried to decrease balance of %s below allowed minimum of %d."
                     currency (plist-get info :min)))
             ((eql 'truncate (plist-get info :enforce-max))
              (min (plist-get info :max) (+ balance amt)))
             (t
              (+ balance amt))))
           (t
            (+ balance amt))))
    (plist-put inventory-plist :balance new-balance)
    (plist-put inventory-plist :date (time-to-days (current-time)))
    (org-gamify-currency-changed-message currency balance new-balance)
    (let ((after-fn (plist-get info (if (plusp amt)
                                        :after-increase-function
                                      :after-decrease-function))))
      (when after-fn
        (funcall after-fn amt)))
    new-balance))


(defun org-gamify-currency-changed-message (currency old-balance new-balance)
  "This function is called after a currency's balance is altered. It displays
a message in the minibuffer notifying the user that the balance has
changed. Returns the message as a string."
  (let* ((currency-plist (org-gamify-get-currency-plist currency))
         (cname (or (plist-get currency-plist :name) currency))
         (balance-function (or (plist-get currency-plist :balance-function)
                               (lambda (b) (format "%d" b))))
         (msg (cond
               ((plist-get currency-plist :hide-amount)
                (format "%s %s!" (if (> new-balance old-balance)
                                     "Gained" "Lost")
                        cname))
               (t
                (format "%+d %s!  Balance now %s" (- new-balance old-balance)
                        cname
                        (funcall balance-function new-balance))))))
    (cond
     ((and org-gamify-use-alert-p
           (featurep 'alert))
      (alert msg
             :title "Currency changed"
             :icon (or (plist-get currency-plist :icon) "emacs")
             :timeout (* 1000 org-gamify-message-delay)))
     (t
      (message msg)
      (sit-for org-gamify-message-delay)))
    msg))



;;; * Block state changes that spend too much currency


(defun org-gamify-block-todo (task-plist)
  "This function is added to the hook variable `org-blocker-hook'. It prevents
the current task from moving to a 'done' state if the associated currency
changes would put a currency's balance outside the allowed range for that
currency, and the currency's enforcement style is 'block'."
  (let ((pos (plist-get task-plist :position))
        (to-state (plist-get task-plist :to))
        (from-state (plist-get task-plist :from)))
    (cond
     ((or (not org-gamify-active-p)
          (not (member from-state (cons 'todo org-not-done-keywords)))
          (not (member to-state (cons 'done org-done-keywords))))
      t)                                ; return t to avoid blocking
     (t
      (save-excursion
        (goto-char pos)
        (block gamify-block-todo
          (let ((deltas (org-gamify-get-deltas)))
            (dolist (entry deltas)
              (destructuring-bind (currency delta) entry
                (let* ((currency-plist (org-gamify-get-currency-plist currency))
                       (balance (org-gamify-get-currency-balance currency))
                       (new-balance (+ delta (or balance 0))))
                  (cond
                   ((and (eql 'block (plist-get currency-plist :enforce-min))
                         (numberp (plist-get currency-plist :min))
                         (minusp delta)
                         (< new-balance (plist-get currency-plist :min)))
                    (org-gamify-currency-blocked-message
                     currency balance new-balance)
                    (return-from gamify-block-todo nil))
                   ((and (eql 'block (plist-get currency-plist :enforce-max))
                         (numberp (plist-get currency-plist :max))
                         (plusp delta)
                         (> new-balance (plist-get currency-plist :max)))
                    (org-gamify-currency-blocked-message
                     currency balance new-balance)
                    (return-from gamify-block-todo nil))))))
            t)))))))


(defun org-gamify-currency-blocked-message (currency old-balance new-balance)
  (let* ((currency-plist (org-gamify-get-currency-plist currency))
         (cname (or (plist-get currency-plist :name) currency))
         (balance-function (or (plist-get currency-plist :balance-function)
                               (lambda (b) (format "%d" b))))
         (msg (cond
               ((plist-get currency-plist :hide-amount)
                (format "You can't %s %s!"
                        (if (> new-balance old-balance) "gain" "remove")
                        cname))
               ((< new-balance old-balance)
                (format "Not enough %s!\nBalance: %s" cname
                        (funcall balance-function old-balance)))
               (t
                (format "Too much %s!\nBalance: %s" cname
                        (funcall balance-function old-balance))))))
    (cond
     ((and org-gamify-use-alert-p
           (featurep 'alert))
      (alert msg
             :title "Action blocked"
             :icon (or (plist-get currency-plist :icon) "emacs")
             :timeout (* 1000 org-gamify-message-delay)))
     (t
      (message (propertize msg 'face 'warning))
      (sit-for org-gamify-message-delay)))
    msg))


(add-hook 'org-blocker-hook 'org-gamify-block-todo)

;;; `org-blocker-hook' are run when a task's state is being changed, and is
;;; also run by the function `org-entry-blocked-p' which tests whether a
;;; hypothetical state change would be blocked. There doesn't seem to be any
;;; way to tell whether a state change is hypothetical or actual. Hence we
;;; advise `org-entry-blocked-p' to silence org-gamify messages.

(defadvice org-entry-blocked-p (around gamify-silence-currency-blocked-message
                                       () activate)
  (cl-flet ((org-gamify-currency-blocked-message (&rest args) nil))
    ad-do-it))


;;; * Daily update


(defun org-gamify-daily-update ()
  "This runs about once per day (sometimes slightly more often if Emacs is
restarted). However it is guaranteed to process each task in the agenda
exactly ONCE per day. For each task, it runs all the functions in the
list `org-gamify-daily-update-functions', stopping as soon as one
of the functions returns non-nil."
  (let* ((now (time-to-days (current-time)))
         (last-updated (or org-gamify-last-update-day
                           (1- now)))
         (update-cycles-due (- now last-updated)))
    (when org-gamify-active-p
      (message "Org-gamify: running daily update of all active tasks...")
      (when (plusp update-cycles-due)
        (org-map-entries
         (lambda ()
           (loop for day from (- now update-cycles-due) to now
                 do
                 (progn
                   (let ((scheduled (org-gamify-get-scheduled-day))
                         (deadline (org-gamify-get-deadline-day))
                         (today (time-to-days (current-time))))
                     (run-hook-with-args-until-success
                      'org-gamify-daily-update-functions
                      (if scheduled (- today scheduled) nil)
                      (if deadline (- today deadline) nil)
                      (org-gamify-habit-chain-length))))))
         "/!"                          ; this search matches all non-done tasks
         'agenda 'archive)
        (setq org-gamify-last-update-day now)))))


(run-at-time "00:00" nil 'org-gamify-daily-update)
(add-hook 'after-init-hook 'org-gamify-daily-update)
(add-hook 'org-agenda-mode-hook 'org-gamify-daily-update)


;;; * Habit integration


(defun org-gamify-habit-chain-length (&optional bad-habit-p)
  "Return the number of days back from the present that the habit has been
maintained. For bad habits, we just return the number of days that have
passed since the habit was last indulged.
If the task has never been completed before, or if the date of last completion
is too far in the past, return 0."
  (let ((today (time-to-days (current-time)))
        (chain 0))
    (cond
     ((org-is-habit-p)
      (destructuring-bind (sched-date habit-min dead-date habit-max
                                      done-dates repeater-type)
          (org-habit-parse-todo)
        (setq done-dates (sort done-dates '>))
        (cond
         ((null done-dates)
          (setq chain 0))
         (bad-habit-p
          (setq chain (- today (first done-dates))))
         (t
          (block chain-count
            (push today done-dates)
            (dotimes (n (1- (length done-dates)))
              (cond
               ((<= (- (nth n done-dates) (nth (1+ n) done-dates))
                   (or habit-max habit-min))
                (setq chain (- today (nth (1+ n) done-dates))))
               (t
                (return-from chain-count nil)))))))
        chain))
     (t
      0))))


;;; * Interactive commands

;;; ** Display deltas for current task


(defun org-gamify-show-deltas ()
  "Display the currency changes for the current task, in the minibuffer."
  (interactive)
  (let ((deltas (org-gamify-get-deltas)))
    (cond
     ((null (org-get-todo-state))
      (message "Not a task."))
     ((not (org-entry-is-todo-p))
      (message "This task is already done."))
     (t
      (let ((strings (mapcar (lambda (entry) (format "%+d %s"
                                                (second entry)
                                                (first entry)))
                             deltas))
            (full ""))
        (dotimes (n (1- (length strings)))
          (setq full (concat full (concat (nth n strings) ", "))))
        (setq full (concat full (nth (1- (length strings)) strings)))
        (message "Completion of this task = %s." full))))))


;;; ** Display inventory in a buffer


(defun org-gamify-show-inventory ()
  "Display a summary of all currency balances, in a temporary buffer."
  (interactive)
  ;; Forcibly run org-gamify-daily-update
  (org-gamify-daily-update)
  (let ((buf (get-buffer-create "*Inventory*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (kill-region (point-min) (point-max))
      (dolist (category
               (cons nil (remove nil (remove-duplicates
                                      (mapcar (lambda (entry)
                                                (plist-get (cdr entry)
                                                           :category))
                                              org-gamify-currencies)
                                      :test 'equal))))
        (when category
          (insert "\n")
          (insert (propertize category 'face '(:bold t)))
          (insert "\n\n"))
        (dolist (entry (remove-if-not
                        (lambda (e) (equal category
                                      (plist-get (cdr e) :category)))
                        org-gamify-currencies))
          (destructuring-bind (currency . currency-plist) entry
            (let* ((in-inventory-p (assoc currency org-gamify-inventory))
                   (balance (or (org-gamify-get-currency-balance currency) 0))
                   (balance-function (or (plist-get currency-plist
                                                    :balance-function)
                                         (lambda (b) (format "%d" b))))
                   (cname (or (plist-get currency-plist :name)
                              currency))
                   (fg-color (or (plist-get currency-plist :fg-color)
                                 (face-foreground 'default)))
                   (bg-color (or (plist-get currency-plist :bg-color)
                                 (face-background 'default))))
              (cond
               ((plist-get currency-plist :hide-amount)
                (if (plusp balance)
                    (insert (capitalize (format "%s\n" cname)))))
               (t
                (insert (format "  %-26s %20s\n"
                                (capitalize (propertize (format "%s" cname) 'face
                                                        `(:foreground
                                                          ,fg-color
                                                          :background
                                                          ,bg-color)))
                                (funcall balance-function balance)))))))))
      (help-mode))
    (display-buffer buf)))


;;; ** Attempt to 'undo' currency changes


(defun org-gamify-undo-deltas ()
  "Apply the currency deltas for the task at point, multiplied by -1.
This command is intended for use if the user accidentally applies a currency
change incorrectly."
  (interactive)
  (org-gamify-apply-deltas '(:from todo :to done) t))


(provide 'org-gamify)
