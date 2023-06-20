;;; init-company.el --- My company configuration
;;; Commentary:
;;; Code:
(use-package company
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.1
        company-echo-delay 0
        company-show-numbers t)

  ;; Dictionary for completion.
  ;; https://emacs.stackexchange.com/questions/54741/using-company-ispell-with-large-text-dictionary/54742#54742
  (setq ispell-complete-word-dict
        (expand-file-name (concat user-emacs-directory "aspell_words.txt")))

  (defun my-generic-ispell-company-complete-setup ()
    ;; Only apply this locally.
    (make-local-variable 'company-backends)
    (setq company-backends (list 'company-ispell))

    (when ispell-complete-word-dict
      (let*
          ((has-dict-complete
            (and ispell-complete-word-dict (file-exists-p ispell-complete-word-dict)))
           (has-dict-personal
            (and ispell-personal-dictionary (file-exists-p ispell-personal-dictionary)))
           (is-dict-outdated
            (and
             has-dict-complete has-dict-personal
             (time-less-p
              (nth 5 (file-attributes ispell-complete-word-dict))
              (nth 5 (file-attributes ispell-personal-dictionary))))))

        (when (or (not has-dict-complete) is-dict-outdated)
          (with-temp-buffer

            ;; Optional: insert personal dictionary, stripping header and inserting a newline.
            (when has-dict-personal
              (insert-file-contents ispell-personal-dictionary)
              (goto-char (point-min))
              (when (looking-at "personal_ws\-")
                (delete-region (line-beginning-position) (1+ (line-end-position))))
              (goto-char (point-max))
              (unless (eq ?\n (char-after))
                (insert "\n")))

            (call-process "aspell" nil t nil "-d" "en_US" "dump" "master")
            (call-process "aspell" nil t nil "-d" "pt_BR" "dump" "master")

            ;; Case insensitive sort is important for the lookup.
            (let ((sort-fold-case t))
              (sort-lines nil (point-min) (point-max)))
            (write-region nil nil ispell-complete-word-dict))))))

  ;; Enable this in appropriate modes.

  (add-hook 'org-mode-hook (lambda () (my-generic-ispell-company-complete-setup)))
  (add-hook 'rst-mode-hook (lambda () (my-generic-ispell-company-complete-setup)))
  (add-hook 'markdown-mode-hook (lambda () (my-generic-ispell-company-complete-setup)))

  (global-company-mode))

(use-package company-emoji
  :hook ((after-init . company-emoji-init)
         (after-make-frame-functions . darwin-set-emoji-font))
  :config
  (defun set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs NS/Cocoa can display emoji properly."
    (if (eq system-type 'darwin)
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

  (setq company-format-margin-function #'company-vscode-light-icons-margin)

  (set-emoji-font nil))

(provide 'init-company)
;;; init-company.el ends here
