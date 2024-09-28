(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-save-default nil)
 '(completion-ignore-case t t)
 '(dired-dwim-target 'dired-dwim-target-next)
 '(display-battery-mode t)
 '(display-line-numbers-type t)
 '(display-time-mode t)
 '(eglot-send-changes-idle-time 0)
 '(enable-recursive-minibuffers t)
 '(global-display-line-numbers-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(magit-diff-refine-hunk 'all)
 '(menu-bar-mode nil)
 '(ns-command-modifier 'meta)
 '(org-adapt-indentation nil)
 '(org-agenda-files nil)
 '(org-agenda-span 'day)
 '(org-archive-location ".archive.org::datetree/* From %s")
 '(org-cycle-global-at-bob t)
 '(org-drill-save-buffers-after-drill-sessions-p nil)
 '(org-drill-scope '("~/Dropbox/org/drill.org"))
 '(org-image-actual-width '(640))
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(org-stuck-projects
   '("+project-someday/-DONE"
     ("WAITING" "TODO" "NEXT" "DELEGATED" "CANCELLED")
     ("ignore" "someday")
     ""))
 '(package-selected-packages
   '(tree-sitter-langs tree-sitter doom-themes consult-eglot eglot company lsp-ui lsp-mode treemacs flycheck wgrep dockerfile-mode docker-mode go-mode yaml projectilex org-roam dashboard vertico rcirc-styles rubocopfmt consult-spotify consult orderless rust-mode git-link yasnippet markdown-mode deft org-brain origami xterm-color graphql-mode org-drill web-mode nix-mode yaml-mode projectile magit use-package))
 '(pcomplete-ignore-case t t)
 '(rcirc-reconnect-attempts 300)
 '(rcirc-reconnect-delay 1)
 '(rcirc-server-alist
   '(("irc.libera.chat" :nick "rcy" :port 6697 :user-name "rcy" :channels
      ("#emb #rcirc #tasteslikeme #djfullmoon")
      :encryption tls)))
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-enable-comment-interpolation t)
 '(web-mode-enable-engine-detection t)
 '(web-mode-markup-indent-offset 2)
 '(webjump-sites
   '(("DuckDuckGo" .
      [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
     ("Google" .
      [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("Wikipedia" .
      [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
     ("http.cat" .
      [simple-query "http.cat" "https://http.cat/status/" ""])
     ("rcy.sh" .
      [simple-query "rcy.sh" "https://rcy.sh/" ""])
     ("github projects" .
      [simple-query "https://github.com/rcy?tab=projects" "https://github.com/users/rcy/projects/" ""])))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit line-number :foreground "white")))))
