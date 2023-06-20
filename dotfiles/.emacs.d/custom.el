(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "Hyrule.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("6c26b114b6f9ab62184a4f1aed4bcfa75ee83c300c9dd4d4b8eb49a60b9f5f72" "bfac9f5b962572739db905a07a2d8d32b25258cd67826727d354013b63d8529e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "ad1c2abad40e11d22156fe3987fd9b74b9e1c822264a07dacb24e0b3133aaed1" "945fe66fbc30a7cbe0ed3e970195a7ee79ee34f49a86bc96d02662ab449b8134" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" default))
 '(exist/access-token *my-exist-access-token*)
 '(gitlab-remote-pattern "\\`.+code\\.locaweb\\.com\\.br:\\(.+\\)\\.git\\'")
 '(gitlab-url "https://code.locaweb.com.br")
 '(ivy-youtube-key *youtube-key*)
 '(ivy-youtube-play-at "/usr/bin/vlc")
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(meghanada-java-path "java")
 '(org-agenda-files nil)
 '(org-now-location '("~/Dropbox/org/org-now.org"))
 '(package-selected-packages
   '(elixir-mode flycheck-projectile ruby-refactor enh-ruby-mode dired-collapse languagetool string-inflection dwim-shell-command fira-code-mode lsp-origami mood-line restclient-jq persp-mode-projectile-bridge persp-mode-projectile-bridge-autoloads persp-mode tzc github-review forge rustic racer rust-mode typescript-mode tide emmet-mode prettier-js lsp-elixir flycheck-mix exunit scala-mode dashboard cider-eval-sexp-fu cider-hydra flycheck-clj-kondo org-gcal persist exec-path-from-shell sml-mode dired-quick-sort company-lsp lsp-treemacs lsp-ivy lsp-ui lsp-mode ivy-youtube shades-of-purple-theme grip-mode nix-mode synosaurus eyebrowse powerline clj-refactor beancount magithub adjust-parens minions moody minesweeper org-now pocket-reader ob-async ag counsel meghanada maghanada magit-todos cheat-sh calfw-ical jira-markup-mode flycheck-credo alchemist elfeed-org elfeed flycheck-clojure clojure-mode flycheck-pos-tip restart-emacs pdf-tools json-navigator symbol-overlay simbol-overlay wgrep ivy-rich instapaper beacon doom-themes doom-theme moe-theme free-keys zeal-at-point yasnippet yaml-mode yagist which-key web-mode wakatime-mode undo-tree twittering-mode telephone-line tagedit smex smart-shift smartparens ruby-tools rubocop rspec-mode rhtml-mode request rbenv rainbow-mode rainbow-delimiters counsel-projectile projectile-rails projectile ox-twbs org-bullets ob-sml ob-restclient neotree mutant multiple-cursors markdown-mode+ magit langtool indent-guide ido-vertical-mode hl-anything highlight haskell-mode google-translate google-this git-timemachine git-gutter-fringe git-gutter gist frame-cmds flycheck expand-region easy-kill docker discover-my-major dired+ company-emoji company clojure-mode-extra-font-locking cider bundler back-button auto-package-update apel anzu ace-window))
 '(safe-local-variable-values
   '((flycheck-disabled-checkers
      '(ruby ruby-jruby ruby-reek ruby-rubocop ruby-rubylint))))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))))
 '(sp-pair-overlay-face ((t (:inherit underline)))))
