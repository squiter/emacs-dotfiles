;;; init-dwim-shell-command.el --- Personal config for dwim-shell-command
;;
;; Copyright (C) 2022 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; Created: 19 October 2022
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

;;; Code:
(use-package dwim-shell-command
  :demand t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (require 'dwim-shell-commands)
  (message "loadeii aqui!!")
  (defun dwim-shell-commands--macos-sharing-services ()
    "Return a list of sharing services."
    (let* ((source (format "import AppKit
                         NSSharingService.sharingServices(forItems: [
                           %s
                         ]).forEach {
                           print(\"\\($0.title)\")
                         }"
                           (string-join (mapcar (lambda (file)
                                                  (format "URL(fileURLWithPath: \"%s\")" file))
                                                (dwim-shell-command--files))
                                        ", ")))
           (services (split-string (string-trim (shell-command-to-string (format "echo '%s' | swift -" source)))
                                   "\n")))
      (when (seq-empty-p services)
        (error "No sharing services available"))
      services))

  (defun dwim-shell-commands-macos-share ()
    "Share selected files from macOS."
    (interactive)
    (let* ((services (dwim-shell-commands--macos-sharing-services))
           (service-name (completing-read "Share via: " services))
           (selection (seq-position services service-name #'string-equal)))
      (dwim-shell-command-on-marked-files
       "Share"
       (format
        "import AppKit

       _ = NSApplication.shared

       NSApp.setActivationPolicy(.regular)

       class MyWindow: NSWindow, NSSharingServiceDelegate {
         func sharingService(
           _ sharingService: NSSharingService,
           didShareItems items: [Any]
         ) {
           NSApplication.shared.terminate(nil)
         }

         func sharingService(
           _ sharingService: NSSharingService, didFailToShareItems items: [Any], error: Error
         ) {
           let error = error as NSError
           if error.domain == NSCocoaErrorDomain && error.code == NSUserCancelledError {
             NSApplication.shared.terminate(nil)
           }
           exit(1)
         }
       }

       let window = MyWindow(
         contentRect: NSRect(x: 0, y: 0, width: 0, height: 0),
         styleMask: [],
         backing: .buffered,
         defer: false)

       let services = NSSharingService.sharingServices(forItems: [\"<<*>>\"].map{URL(fileURLWithPath:$0)})
       let service = services[%s]
       service.delegate = window
       service.perform(withItems: [\"<<*>>\"].map{URL(fileURLWithPath:$0)})

       NSApp.run()" selection)
       :silent-success t
       :shell-pipe "swift -"
       :join-separator ", "
       :no-progress t
       :utils "swift")))

  (defun dwim-shell-commands-macos-reveal-in-finder ()
    "Reveal selected files in macOS Finder."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Reveal in Finder"
     "import AppKit
    NSWorkspace.shared.activateFileViewerSelecting([\"<<*>>\"].map{URL(fileURLWithPath:$0)})"
     :join-separator ", "
     :silent-success t
     :shell-pipe "swift -")))

(provide 'init-dwim-shell-command)
;;; init-dwim-shell-command.el ends here
