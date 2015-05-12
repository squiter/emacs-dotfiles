(defgroup org-id nil
  "Options concerning global entry identifiers in Org-mode."
  :tag "Org ID"
  :group 'org)

(defcustom org-id-method 'org
  "The method that should be used to create new ID's.
An ID will consist of the prefix specified in `org-id-prefix', and a unique
part created by the method this variable specifies.
Allowed values are:
org        Org's own internal method, using an encoding of the current time,
           and the current domain of the computer.  This method will
           honor the variable `org-id-include-domain'.
uuidgen    Call the external command uuidgen."
  :group 'org-id
  :type '(choice
	  (const :tag "Org's internal method" org)
	  (const :tag "external: uuidgen" uuidgen)))

(defcustom org-id-prefix nil
  "The prefix for IDs.
This may be a string, or it can be nil to indicate that no prefix is required.
When a string, the string should have no space characters as IDs are expected
to have no space characters in them."
  :group 'org-id
  :type '(choice
	  (const :tag "No prefix")
	  (string :tag "Prefix")))

(defcustom org-id-include-domain t
  "Non-nil means, add the domain name to new IDs.
This ensures global uniqueness of ID's, and is also suggested by
RFC 2445 in combination with RFC 822.  This is only relevant if
`org-id-method' is `org'.  When uuidgen is used, the domain will never
be added."
  :group 'org-id
  :type 'boolean)

(defcustom org-id-locations-file
  (path-join *user-org-cache-directory* ".org-id-locations")
  "The file for remembering the last ID number generated."
  :group 'org-id
  :type 'file)

(defvar org-id-locations nil
  "List of files with ID's in those files.")

(defcustom org-id-extra-files 'org-agenda-text-search-extra-files
  "Files to be searched for ID's, besides the agenda files."
  :group 'org-id
  :type
  '(choice
    (symbol :tag "Variable")
    (repeat :tag "List of files"
	    (file))))

;;; The API functions

;;;###autoload
(defun org-id-get-create (&optional force)
  "Create an ID for the current entry and return it.
If the entry already has an ID, just return it.
With optional argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when force
    (org-entry-put (point) "ID" nil))
  (org-id-get (point) 'create))
  
;;;###autoload
(defun org-id-copy ()
  "Copy the ID of the entry at point to the kill ring.
Create an ID if necessary."
  (interactive)
  (kill-new (org-id-get nil 'create)))  

;;;###autoload
(defun org-id-get (&optional pom create prefix)
  "Get the ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
However, when CREATE is non nil, create an ID if none is present already.
PREFIX will be passed through to `org-id-new'.
In any case, the ID of the entry is returned."
  (let ((id (org-entry-get pom "ID")))
    (cond
     ((and id (stringp id) (string-match "\\S-" id))
      id)
     (create
      (setq id (org-id-new prefix))
      (org-entry-put pom "ID" id)
      (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
      id)
     (t nil))))

;;;###autoload
(defun org-id-get-with-outline-path-completion (&optional targets)
  "Use outline-path-completion to retrieve the ID of an entry.
TARGETS may be a setting for `org-refile-targets' to define the eligible
headlines.  When omitted, all headlines in all agenda files are
eligible.
It returns the ID of the entry.  If necessary, the ID is created."
  (let* ((org-refile-targets (or targets '((nil . (:maxlevel . 10)))))
	 (org-refile-use-outline-path 
	  (if (caar org-refile-targets) 'file t))
	 (spos (org-refile-get-location "Entry: "))
	 (pom (and spos (move-marker (make-marker) (nth 3 spos) 
				     (get-file-buffer (nth 1 spos))))))
    (prog1 (org-id-get pom 'create)
      (move-marker pom nil))))

;;;###autoload
(defun org-id-get-with-outline-drilling (&optional targets)
  "Use an outline-cycling interface to retrieve the ID of an entry.
This only finds entries in the current buffer, using `org-get-location'.
It returns the ID of the entry.  If necessary, the ID is created."
  (let* ((spos (org-get-location (current-buffer) org-goto-help))
	 (pom (and spos (move-marker (make-marker) (car spos)))))
    (prog1 (org-id-get pom 'create)
      (move-marker pom nil))))

;;;###autoload
(defun org-id-goto (id)
  "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer."
  (interactive)
  (let ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (switch-to-buffer (marker-buffer m))
    (goto-char m)
    (move-marker m nil)
    (org-show-context)))    

;;;###autoload
(defun org-id-find (id &optional markerp)
  "Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker."
  (let ((file (org-id-find-id-file id))
	org-agenda-new-buffers where)
    (when file
      (setq where (org-id-find-id-in-file id file markerp)))
    (unless where
      (org-id-update-id-locations)
      (setq file (org-id-find-id-file id))
      (when file
	(setq where (org-id-find-id-in-file id file markerp))))
    where))

;;; Internal functions

;; Creating new IDs

(defun org-id-new (&optional prefix)
  "Create a new globally unique ID.
An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.
PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.
So a typical ID could look like \"Org:4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
		     ""
		   (concat (or prefix org-id-prefix) ":")))
	 unique)
    (if (equal prefix ":") (setq prefix ""))
    (cond
     ((eq org-id-method 'uuidgen)
      (setq unique (org-trim (shell-command-to-string "uuidgen"))))
     ((eq org-id-method 'org)
      (let* ((etime (org-id-reverse-string (org-id-time-to-b36)))
	     (postfix (if org-id-include-domain
			  (progn
			    (require 'message)
			    (concat "@" (message-make-fqdn))))))
	(setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix unique)))

(defun org-id-reverse-string (s)
  (mapconcat 'char-to-string (nreverse (string-to-list s)) ""))

(defun org-id-int-to-b36-one-digit (i)
  "Turn an integer between 0 and 61 into a single character 0..9, A..Z, a..z."
  (cond
   ((< i 10) (+ ?0 i))
   ((< i 36) (+ ?a i -10))
   (t (error "Larger that 35"))))

(defun org-id-b36-to-int-one-digit (i)
  "Turn a character 0..9, A..Z, a..z into a number 0..61.
The input I may be a character, or a single-letter string."
  (and (stringp i) (setq i (string-to-char i)))
  (cond
   ((and (>= i ?0) (<= i ?9)) (- i ?0))
   ((and (>= i ?a) (<= i ?z)) (+ (- i ?a) 10))
   (t (error "Invalid b36 letter"))))

(defun org-id-int-to-b36 (i &optional length)
  "Convert an integer to a base-36 number represented as a string."
  (let ((s ""))
    (while (> i 0)
      (setq s (concat (char-to-string
		       (org-id-int-to-b36-one-digit (mod i 36))) s)
	    i (/ i 36)))
    (setq length (max 1 (or length 1)))
    (if (< (length s) length)
	(setq s (concat (make-string (- length (length s)) ?0) s)))
    s))

(defun org-id-b36-to-int (s)
  "Convert a base-36 string into the corresponding integer."
  (let ((r 0))
    (mapc (lambda (i) (setq r (+ (* r 36) (org-id-b36-to-int-one-digit i))))
	  s)
    r))

(defun org-id-time-to-b36 (&optional time)
  "Encode TIME as a 10-digit string.
This string holds the time to micro-second accuracy, and can be decoded
using `org-id-decode'."
  (setq time (or time (current-time)))
  (concat (org-id-int-to-b36 (nth 0 time) 4)
	  (org-id-int-to-b36 (nth 1 time) 4)
	  (org-id-int-to-b36 (or (nth 2 time) 0) 4)))

(defun org-id-decode (id)
  "Split ID into the prefix and the time value that was used to create it.
The return value is (prefix . time) where PREFIX is nil or a string,
and time is the usual three-integer representation of time."
  (let (prefix time parts)
    (setq parts (org-split-string id ":"))
    (if (= 2 (length parts))
	(setq prefix (car parts) time (nth 1 parts))
      (setq prefix nil time (nth 0 parts)))
    (setq time (org-id-reverse-string time))
    (setq time (list (org-id-b36-to-int (substring time 0 4))
		     (org-id-b36-to-int (substring time 4 8))
		     (org-id-b36-to-int (substring time 8 12))))
    (cons prefix time)))

;; Storing ID locations (files)

(defun org-id-update-id-locations ()
  "Scan relevant files for ID's.
Store the relation between files and corresponding ID's."
  (interactive)
  (let ((files (append (org-agenda-files)
		       (if (symbolp org-id-extra-files)
			   (symbol-value org-id-extra-files)
			 org-id-extra-files)))
	org-agenda-new-buffers
	file ids reg found id)
    (while (setq file (pop files))
      (setq ids nil)
      (with-current-buffer (org-get-agenda-file-buffer file)
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$"
				      nil t)
	      (setq id (org-match-string-no-properties 1))
	      (if (member id found)
		  (error "Duplicate ID \"%s\"" id))
	      (push id found)
	      (push id ids))
	    (push (cons file ids) reg)))))
    (org-release-buffers org-agenda-new-buffers)
    (setq org-agenda-new-buffers nil)
    (setq org-id-locations reg)
    (org-id-locations-save)))

(defun org-id-locations-save ()
  "Save `org-id-locations' in `org-id-locations-file'."
  (with-temp-file org-id-locations-file
    (print org-id-locations (current-buffer))))

(defun org-id-locations-load ()
  "Read the data from `org-id-locations-file'."
  (setq org-id-locations nil)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally org-id-locations-file)
          (goto-char (point-min))
          (setq org-id-locations (read (current-buffer))))
      (error
       (message "Could not read org-id-values from %s. Setting it to nil."
                org-id-locations-file)))))

(defun org-id-add-location (id file)
  "Add the ID with location FILE to the database of ID loations."
  (unless org-id-locations (org-id-locations-load))
  (catch 'exit
    (let ((locs org-id-locations) list)
      (while (setq list (pop locs))
	(when (equal (file-truename file) (file-truename (car list)))
	  (setcdr list (cons id (cdr list)))
	  (throw 'exit t))))
    (push (list file id) org-id-locations))
  (org-id-locations-save))

;; Finding entries with specified id

(defun org-id-find-id-file (id)
  "Query the id database for the file in which this ID is located."
  (unless org-id-locations (org-id-locations-load))
  (catch 'found
    (mapc (lambda (x) (if (member id (cdr x))
			  (throw 'found (car x))))
	  org-id-locations)
    nil))

(defun org-id-find-id-in-file (id file &optional markerp)
  "Return the position of the entry ID in FILE.
If that files does not exist, or if it does not contain this ID,
return nil.
The position is returned as a cons cell (file-name . position).  With
optional argument MARKERP, return the position as a new marker."
  (let (org-agenda-new-buffers m buf pos)
    (cond
     ((not file) nil)
     ((not (file-exists-p file)) nil)
     (t (with-current-buffer (setq buf (org-get-agenda-file-buffer file))
	  (setq pos (org-find-entry-with-id id))
	  (when pos
	    (if markerp
		(move-marker (make-marker) pos buf)
	      (cons file pos))))))))

(provide 'init-org-id)