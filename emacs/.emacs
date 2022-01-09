;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; local
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore-errors (load "~/.emacs.private"))
(ignore-errors (load (format "~/.emacs.%s" (system-name))))
;;(add-to-list 'load-path "~/elisp")
(load "~/elisp/rcy-util.el")
(setq visible-bell t)

(use-package jira-at-point
  :load-path "~/elisp"
  :bind (("C-c j" . rcy/browse-jira-issue-at-point)))

(use-package rcy-roam-agenda
  :demand
  :after org-roam
  :load-path "~/elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; (package-install-selected-packages)

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
 '(indent-tabs-mode nil)
 '(magit-diff-refine-hunk 'all)
 '(menu-bar-mode nil)
 '(ns-command-modifier 'meta)
 '(org-adapt-indentation nil)
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
   '(dashboard vertico rcirc-styles rubocopfmt consult-spotify consult orderless rust-mode git-link yasnippet markdown-mode deft org-brain origami xterm-color graphql-mode org-drill web-mode nix-mode yaml-mode projectile magit use-package))
 '(pcomplete-ignore-case t)
 '(rcirc-server-alist
   '(("irc.libera.chat" :nick "rcy" :port 6697 :user-name "rcy" :channels
      ("#emb #rcirc #tasteslikeme #djfullmoon")
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
   '((#("JIRA (Pn)" 0 9
        (face nil))
      .
      [simple-query "https://precisionnutrition.atlassian.net/jira/your-work" "https://precisionnutrition.atlassian.net/browse/" ""])
     ("DuckDuckGo" .
      [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
     ("Google" .
      [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("Wikipedia" .
      [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "yellow"))))
 '(highlight ((t (:background "dark green" :foreground "white" :underline nil))))
 '(region ((t (:extend t :background "#000077" :foreground "white")))))

;;(package-install-selected-packages)

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
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("Gemfile." . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      `(("x" "Dynamic Inbox" entry (file ,(format "~/Dropbox/org/inbox.%s.org" system-name))
         "\n* NEXT [#A] %?")
        ("i" "Inbox" entry (file+headline "~/Dropbox/org/inbox.org" "Inbox")
         "* %?\n%i\n")
	;; ("t" "Todo" entry (file+headline "~/Dropbox/org/gtd.org" "Tasks")
        ;;  "* NEXT %? %i\n")
	("p" "Personal Project" entry (file+headline "~/Dropbox/org/personal.org" "Personal Projects")
         "* %?")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %<%H:%M>\n\n%?%i"
	 :empty-lines 1
	 :unnarrowed nil
	 )
	("r" "Reading" entry (file+headline "~/Dropbox/org/reading.org" "Reading")
         "* %?")
	("w" "Wishlist" entry (file+headline "~/Dropbox/org/wishlist.org" "Wishlist")
         "* %?")
	("s" "Shopping" entry (file+headline "~/Dropbox/org/shopping.org" "Shopping")
         "* BUY %?")
	("d" "Drill" entry (file+headline "~/Dropbox/org/drill.org" "Drill")
	 ,(concat "* Item :drill:\n"
		  "  :PROPERTIES:\n"
		  "  :DRILL_CARD_TYPE: hide1cloze\n"
		  "  :END:\n\n"
		  "  %?")
	 :empty-lines 1
	 )))

(global-set-key (kbd "C-c r") 'org-capture) ;;; remember
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c B") 'bury-buffer)

(setq org-todo-keywords
      '((sequence "NEXT(n)" "WAITING(w)" "DELEGATED(g)" "|" "DONE(d)" "CANCELLED(c)")))


(setq org-feed-alist
      '(("hn" "https://hnrss.org/frontpage" "~/Dropbox/org/feeds.org" "hn")))

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
(global-set-key (kbd "C-c H") 'hl-line-mode)
(global-set-key (kbd "C-c i") 'rcy-insert-random-id)
(global-set-key (kbd "C-c J") 'rcy-prettify-json-region)
;;(global-set-key (kbd "C-c j") 'webjump)
(global-set-key (kbd "C-c k") 'comment-region)
;;(global-set-key (kbd "C-c m") 'notmuch)
(global-set-key (kbd "C-c P") 'rcy-insert-xkcd-password)
(global-set-key (kbd "C-c q") 'quick-calc)
(global-set-key (kbd "C-c o c") 'org-capture)
;;(global-set-key (kbd "C-c R") 'remember)
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
(define-key winner-mode-map (kbd "<C-left>") #'winner-undo)
(define-key winner-mode-map (kbd "<C-right>") #'winner-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-c E") (lambda () (interactive) (find-file (format "~/.emacs.%s" (system-name)))))
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
(setq deft-directory "~/Dropbox/org")
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
(setq comint-buffer-maximum-size 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vertico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; ;; Configure directory extension
;; (use-package vertico-directory
;;   ;; More convenient directory navigation commands
;;   :bind (:map vertico-map
;;               ("RET" . vertico-directory-enter)
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   ;; Tidy shadowed file names
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :init
  (setq consult-project-root-function #'vc-root-dir)
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch
  )

(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-roam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-roam
  :ensure t
;;  :after org
  :init
  (setq org-roam-v2-ack t)
  (make-directory "~/Dropbox/org-roam" t)
  (setq org-roam-directory (file-truename "~/Dropbox/org")) ;; FIXME
  (make-directory (concat org-roam-directory "/daily") t)
  (setq org-roam-dailies-directory "daily/")
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  :bind (
         ("C-c n c" . org-roam-capture)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n b" . org-roam-buffer-toggle)

         ("C-c n d ." . org-roam-dailies-goto-today)
         ("C-c n d d" . org-roam-dailies-goto-date)
         ("C-c n d <" . org-roam-dailies-goto-yesterday)
         ("C-c n d >" . org-roam-dailies-goto-tomorrow)
         ("C-c n d c" . org-roam-dailies-capture-date)

         ("C-c n t a" . org-roam-tag-add)
         ("C-c n t r" . org-roam-tag-remove)

         ("C-c n ." . org-roam-dailies-capture-today)
         )
  )

(defun rcy/org-grep (regexp)
  (interactive "sorg-grep regex: ")
  (grep-compute-defaults)
  (rgrep regexp "*.org" org-directory))
(global-set-key (kbd "C-c n g") 'rcy/org-grep)

;; TODO: install these better
(require 'org-protocol)
(require 'org-roam-protocol)

(setq org-roam-capture-templates
      '(
        ;; default
        ("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
"))
        ;; project
        ("p" "project" plain "
* ${title}
%?"
         :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+filetags: :project:
")
         :empty-lines-before 1
         :unnarrowed t)
))

(setq org-roam-dailies-capture-templates
      '(
        ("d" "default" entry "* NEXT [#A] %?" :target
         (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
"))
;;         ("i" "daily inbox" entry "* %?" :target
;;          (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
;; " ("NEXT [#A] INBOX")))
        ))
