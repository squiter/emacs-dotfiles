(require 'restclient)
(require 'request)
(require 'dash)

(defvar cas--headers "^CAS-\\(Host\\|Service\\|User\\|Password\\|Auth\\)$")
(defvar cas--cache nil)
(defvar cas--response nil)

(add-hook 'restclient-http-do-hook 'cas-middleware)

(defun cas-middleware ()
  (let ((cas-headers (cas--process-headers url-request-extra-headers)))
    (when cas-headers
      (cas-authenticate cas-headers))))

(defun cas-authenticate (&optional headers)
  (interactive)
  (let* ((host (cas--credential "CAS-Host" headers))
         (service (cas--credential "CAS-Service" headers))
         (user (cas--credential "CAS-User" headers))
         (pass (cas--credential "CAS-Password" headers))
         (ticket (or (cas--cache service)
                     (cas--request host service user pass))))
    (cas--cache-add ticket service)
    (if headers
        (cas--attach-ticket ticket)
      (cas--copy ticket))))

(defun cas--request (host service user pass)
  (message "Requesting TGT...")
  (request host
           :type "POST"
           :data (format "username=%s&password=%s" user pass)
           :sync 't
           :parser 'cas--tgt-parser
           :success (cas--callback 'cas--fetch-ticket host service))
  (cas--response))

(defun cas--fetch-ticket (tgt host service)
  (message "Requesting ST...")
  (request (format "%s/%s" host tgt)
           :type "POST"
           :data (format "service=%s" service)
           :sync 't
           :parser 'buffer-string
           :success (cas--callback 'cas--save-response)))

(defmacro cas--callback (callback &rest args)
  (let ((eval-callback (or (and (symbolp callback) (symbol-function callback))
                           callback)))
    `(lambda (&rest response)
       (funcall ,eval-callback (plist-get response :data) ,@args))))

(defun cas--tgt-parser ()
  (beginning-of-buffer)
  (search-forward-regexp "TGT-[^\"\']+")
  (match-string 0))

(defun cas--cache (service)
  (cdr (assoc service cas--cache)))

(defun cas--cache-add (ticket service)
  (add-to-list 'cas--cache `(,service . ,ticket)))

(defun cas-cache-flush ()
  (interactive)
  (setq cas--cache nil))

(defun cas--partition-headers (headers)
  (--separate
   (string-match-p cas--headers (car it))
   headers))

(defun cas--copy (info)
  (kill-new info)
  (message "%s added to clipboard." info))

(defun cas--attach-ticket (st)
  (add-to-list 'url-request-extra-headers `("Service-Ticket" . ,st)))

(defun cas--credential (param headers)
  (let ((param-symbol (intern (downcase param))))
    (or (cdr (assoc param headers))
        (and (boundp param-symbol) (symbol-value param-symbol))
        (read-string (format "%s: " param)))))

(defun cas--save-response (st)
  (message "Got ticket %s" st)
  (setq cas--response st))

(defun cas--response ()
  (let ((response cas--response))
    (setq cas--response nil)
    response))

(defmacro cas--process-headers (all-headers)
  `(-let [(auth non-auth) (cas--partition-headers ,all-headers)]
     ,(list 'setq all-headers 'non-auth)
     `,auth))

(provide 'init-cas)
