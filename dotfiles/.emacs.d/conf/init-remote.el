;;; init-remote.el --- Remote specific configurations for Emacs
;;
;; Copyright (C) 2015 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/dotfiles
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
(defun remote/configure-ex-unit ()
  (interactive)
  
  (projectile-with-default-dir (projectile-acquire-root)
    (async-shell-command "MIX_ENV=test iex --cookie mycookie --name remote@127.0.0.1 -S mix" "Remote iex" "Remote iex - errors"))

  (setq exunit-mix-command
        (lambda (args)
          (let* ((args (s-join ", " (mapcar (lambda (x) (concat "\"" x "\"")) args)))
                 (mix-command (concat "':erpc.call(:\"remote@127.0.0.1\", IexTests, :test, [" args "])'")))
            (list "elixir" "--cookie" "mycookie" "--name" "remote1@127.0.0.1" "-S" "mix" "run" "--no-start" "-e" mix-command)))))

(provide 'init-remote)
;; init-remote.el ends here
