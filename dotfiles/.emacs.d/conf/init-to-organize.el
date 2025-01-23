;;; init-to-organize.el --- Bootstrap for my emacs configurations
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
;; Thanks to http://github.com/rranelli/emacs-dotfile

;;; Code:

;; Theme
(use-package shades-of-purple-theme
  :config (load-theme 'shades-of-purple t))

(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :after moody
  :config (minions-mode))

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ([remap isearch-forward] . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package restart-emacs)

(use-package which-key
  :init (which-key-mode))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind
  ("C-x f" . projectile-find-file)
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/dev/code/" "~/dev/remote/")))

;; constant variable
(defconst *wakatime-osx-dir*
  (file-name-concat "/" "opt" "homebrew" "bin" "wakatime-cli")
  "Path to the wakatime binary.")

(use-package wakatime-mode
  :init
  (if (file-exists-p *wakatime-osx-dir*)
      (setq wakatime-cli-path *wakatime-osx-dir*)
    (if (file-exists-p *wakatime-dir*)
	(setq wakatime-cli-path *wakatime-dir*)
      (setq wakatime-cli-path *wakatime-nix-dir*)))
  :config
  (global-wakatime-mode))

(use-package expand-region
  :bind ("C-=" . 'er/expand-region))

;; Fish Shell Path Loader
(let*
    ((fish-path (shell-command-to-string "/opt/homebrew/bin/fish -i -c \"echo -n \\$PATH[1]; for val in \\$PATH[2..-1];echo -n \\\":\\$val\\\";end\""))
     (full-path (append exec-path (split-string fish-path ":"))))
  (setenv "PATH" fish-path)
  (setq exec-path full-path))

;; Custom Functions
(defun custom/hsplit-last-buffer ()
  "Ex: | Vertically split window showing last buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun custom/vsplit-last-buffer ()
  "Ex: - Horizontally split window showing last buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun custom/swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun custom/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (move-file-to-trash filename)
        (kill-buffer buffer)
        (message "File '%s' successfully trashed" filename)))))

(defun custom/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun custom/sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))





(defun custom/insert-new-line ()
  "Insert a new line without breaking the current one."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun custom/smart-move-beginning-of-line ()
  "Move to beginning of line or to beginning of indentation depending on POINT."
  (interactive)
  (if (= (point) (line-beginning-position))
      (back-to-indentation)
    (move-beginning-of-line nil)))

(defmacro custom/with-region-info (&rest body)
  "Evaluate BODY provinding BEG, END and NUM-LINES bindings, which represents
regions's beginning, ending and extension in lines."
  `(save-excursion
     (let (beg end num-lines)
       (if (and mark-active (> (point) (mark)))
           (exchange-point-and-mark))
       (setq beg (line-beginning-position))
       (if mark-active
           (exchange-point-and-mark))
       (setq end (line-end-position))
       (setq num-lines (max 1 (count-lines beg end)))
       ,@body)))

(defun custom/duplicate-current-line-or-region (arg)
  "Duplicates the current line or those covered by region ARG times."
  (interactive "p")
  (custom/with-region-info
   (let ((exit-point end)
         (region (buffer-substring-no-properties beg end)))
     (dotimes (_ arg)
       (goto-char end)
       (newline)
       (insert region)
       (setq end (point)))
     (goto-char exit-point)
     (next-line)
     (back-to-indentation))))

(defun custom/copy-line ()
  "Copy current line or those covered by a marked region."
  (interactive)
  (custom/with-region-info (kill-ring-save beg end)))

(defun custom/kill-line ()
  "Kill current line or the ones covered by a marked region."
  (interactive)
  (custom/with-region-info
   (goto-char beg)
   (kill-whole-line num-lines)
   (kill-new (custom/chomp (car kill-ring)))))

(defun custom/join-line ()
  "Join current line with the previous one or all covered by a marked region."
  (interactive)
  (custom/with-region-info
   (goto-char end)
   (dotimes (_ (max 1 (1- num-lines)))
     (join-line))))

(defun custom/toggle-line-comment ()
  "Comment or uncomment current line or the ones covered by a marked region."
  (interactive)
  (custom/with-region-info
   (comment-or-uncomment-region beg end)))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun custom/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun custom/move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun custom/move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (custom/move-line (if (null n) -1 (- n))))

(defun custom/move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (custom/move-line (if (null n) 1 n)))

;; General configs

;; replace marked text when type
(delete-selection-mode 1)

;; move cursor by camelCase
(subword-mode 1)

;; enable cameCase support for all programming modes
(add-hook 'prog-mode-hook 'subword-mode)

;; make indentation commands use space only
(setq-default indent-tabs-mode nil)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

(global-display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode 1)

;; Keybindings
;; init-edit-custom-functions.el keybinds:
(global-set-key (kbd "C-<return>")              'custom/insert-new-line)
(global-set-key [remap move-beginning-of-line] #'custom/smart-move-beginning-of-line)
(global-set-key (kbd "C-c d")                   'custom/duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-w")                 'custom/copy-line)
(global-set-key [remap kill-whole-line]        #'custom/kill-line)
(global-set-key (kbd "C-c j")                   'custom/join-line)
(global-set-key (kbd "C-c C-/")                 'custom/toggle-line-comment)
(global-set-key (kbd "M-<up>")                  'custom/move-line-up)
(global-set-key (kbd "M-<down>")                'custom/move-line-down)
(global-set-key [remap fill-paragraph]         #'endless/fill-or-unfill)
(global-set-key (kbd "C-c i")                   'custom/indent-buffer)

(global-set-key (kbd "C-x C-S-k")               'custom/delete-current-buffer-file)
(global-set-key (kbd "C-x C-r")                 'custom/rename-current-buffer-file)
(global-set-key (kbd "C-x !")                   'custom/sudo-edit)

;; window and buffer manipulation
(global-set-key (kbd "C-x |")                   'custom/vsplit-last-buffer)
(global-set-key (kbd "C-x -")                   'custom/hsplit-last-buffer)
(global-set-key (kbd "C-x =")                   'custom/swap-buffers-in-windows)
(global-set-key (kbd "M-o")                     'other-window)
(global-set-key (kbd "M-k")                     'kill-buffer)
(global-set-key (kbd "M-1")                     'delete-other-windows)
(global-set-key (kbd "M-5")                     'delete-window)

;; this is the most awkward keybind I set in my setup
(global-set-key (kbd "M-@") (lambda () (interactive)  (insert  ?€ )))
(global-set-key (kbd "M-#") (lambda () (interactive)  (insert  ?£ )))

;; TODO: Understand where should I put this call
(elpaca-wait)

(provide 'init-to-organize)
;;; init-to-organize.el ends here
