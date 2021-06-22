;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; local
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore-errors (load "~/.emacs.private"))
(ignore-errors (load (format "~/.emacs.%s" (system-name))))
(load "~/elisp/rcy-util.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom-set-*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes '(wombat))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(enable-recursive-minibuffers t)
 '(ido-mode 'buffer nil (ido))
 '(indent-tabs-mode nil)
 '(magit-diff-refine-hunk 'all)
 '(menu-bar-mode nil)
 '(org-agenda-files '("~/org/projects/personal.org" "~/org/shopping.org"))
 '(org-agenda-span 'day)
 '(org-archive-location ".archive.org::datetree/* From %s")
 '(org-cycle-global-at-bob t)
 '(org-drill-save-buffers-after-drill-sessions-p nil)
 '(org-drill-scope '("~/org/drill.org"))
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(org-stuck-projects
   '("+project-someday/-DONE"
     ("WAITING" "TODO" "NEXT" "DELEGATED" "CANCELLED")
     ("ignore" "someday")
     ""))
 '(package-selected-packages
   '(yasnippet markdown-mode deft org-brain org-roam origami xterm-color graphql-mode org-drill web-mode nix-mode yaml-mode projectile magit use-package))
 '(pcomplete-ignore-case t)
 '(rcirc-server-alist
   '(("irc.libera.chat" :nick "rcy" :port 6697 :user-name "rcy" :channels
      ("#emb #rcirc #emacs #djfullmoon")
      :encryption tls)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-enable-comment-interpolation t)
 '(web-mode-enable-engine-detection t)
 '(web-mode-markup-indent-offset 2)
  '(webjump-sites
   (quote
    (("JIRA (Pn)" .
      [simple-query "https://precisionnutrition.atlassian.net/jira/your-work" "https://precisionnutrition.atlassian.net/browse/" ""])
     ("DuckDuckGo" .
      [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
     ("Google" .
      [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("Wikipedia" .
      [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-install-selected-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")
        ("javascript" . "\\.mjs\\'")))
(setq web-mode-comment-formats '(("javascript" . "//")
                                 ("jsx" . "//")
                                 ("typescript" . "//")
                                 ("tsx" . "//")))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mjml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("Gemfile." . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      `(("i" "Inbox" entry (file+headline "~/org/inbox.org" "Inbox")
         "* %?\n%i\n")
	;; ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
        ;;  "* NEXT %? %i\n")
	("p" "Personal Project" entry (file+headline "~/org/projects/personal.org" "Personal Projects")
         "* %?")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %<%H:%M>\n\n%?%i"
	 :empty-lines 1
	 :unnarrowed nil
	 )
	("r" "Reading" entry (file+headline "~/org/reading.org" "Reading")
         "* %?")
	("w" "Wishlist" entry (file+headline "~/org/wishlist.org" "Wishlist")
         "* %?")
	("s" "Shopping" entry (file+headline "~/org/shopping.org" "Shopping")
         "* BUY %?")
	("d" "Drill" entry (file+headline "~/org/drill.org" "Drill")
	 ,(concat "* Item :drill:\n"
		  "  :PROPERTIES:\n"
		  "  :DRILL_CARD_TYPE: hide1cloze\n"
		  "  :END:\n\n"
		  "  %?")
	 :empty-lines 1
	 )))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-capture) ;;; remember
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-todo-keywords
      '((sequence "NEXT(n)" "WAITING(w)" "DELEGATED(g)" "|" "DONE(d)" "CANCELLED(c)")))


(setq org-feed-alist
      '(("hn" "https://hnrss.org/frontpage" "~/org/feeds.org" "hn")))

(setq org-agenda-todo-ignore-scheduled 'all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ffap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffap-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'projectile-mode)
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile / compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c C") 'compile)

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; random keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffap-bindings)
(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C") 'compile)
(global-set-key (kbd "C-c g") 'git-link)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-c i") 'rcy-insert-random-id)
(global-set-key (kbd "C-c J") 'rcy-prettify-json-region)
(global-set-key (kbd "C-c j") 'webjump)
(global-set-key (kbd "C-c k") 'comment-region)
(global-set-key (kbd "C-c m") 'notmuch)
(global-set-key (kbd "C-c P") 'rcy-insert-xkcd-password)
(global-set-key (kbd "C-c q") 'quick-calc)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c R") 'remember)
(global-set-key (kbd "C-c T") 'rcy-insert-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rcirc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq rcirc-debug-flag t)
(rcirc-track-minor-mode t)
(setq rcirc-fill-column 'window-text-width)
(setq rcirc-time-format "%H:%M:%S ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(winner-mode +1)
(define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
(define-key winner-mode-map (kbd "<M-right>") #'winner-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-c d") 'deft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-brain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'org-load-hook (lambda ()
                           (define-key org-mode-map (kbd "C-c b") 'org-brain-prefix-map)))
(global-set-key (kbd "C-c v") 'org-brain-visualize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; completion settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq completion-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq deft-directory "~/org")
(setq deft-default-extension "org")
(setq deft-markdown-mode-title-level 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paren
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; comint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
