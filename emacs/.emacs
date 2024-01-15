;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; local
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore-errors (load "~/.emacs.private"))
(ignore-errors (load (format "~/.emacs.%s" (system-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c B") 'bury-buffer)
(global-set-key (kbd "C-c C") 'compile)
(global-set-key (kbd "C-c H") 'hl-line-mode)
(global-set-key (kbd "C-c J") 'rcy-prettify-json-region)
(global-set-key (kbd "C-c P") 'rcy-insert-xkcd-password)
(global-set-key (kbd "C-c T") 'rcy-insert-time)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-c E") (lambda () (interactive) (find-file (format "~/.emacs.%s" (system-name)))))
(global-set-key (kbd "C-c g") 'git-link)
(global-set-key (kbd "C-c i") 'rcy-insert-random-id)
(global-set-key (kbd "C-c k") 'comment-region)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o l") #'org-capture-goto-last-stored)
(global-set-key (kbd "C-c q") 'quick-calc)
(global-set-key (kbd "C-c n g") 'rcy/org-grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package jira-at-point
  :load-path "~/elisp"
  :bind (("C-c j" . rcy/browse-jira-issue-at-point)))

(use-package rcy-roam-agenda
  :demand
  :after org-roam
  :load-path "~/elisp")

(use-package rcy-util
  :load-path "~/elisp")

(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(use-package consult
  :ensure t
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

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t)
  (make-directory "~/Dropbox/org" t)
  (setq org-roam-directory (file-truename "~/Dropbox/org")) ;; FIXME
  (make-directory (concat org-roam-directory "/daily") t)
  (setq org-roam-dailies-directory "daily/")
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
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
          ("d" "default" entry "* TODO [#A] %?" :target
           (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
"))
          ;;         ("i" "daily inbox" entry "* %?" :target
          ;;          (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
          ;; " ("TODO [#A] INBOX")))
          ))
  :config
  (org-roam-db-autosync-mode)
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

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
          ("javascript" . "\\.mjs\\'")))
  (setq web-mode-script-padding 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-comment-formats '(("javascript" . "//")
                                   ("jsx" . "//")
                                   ("typescript" . "//")
                                   ("tsx" . "//")))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mjml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
  )

(use-package org
;;  :ensure t
  :init
  (global-set-key (kbd "C-c r") 'org-capture) ;;; remember
  (global-set-key (kbd "C-c L") 'org-store-link)


  ;; inspired by https://d12frosted.io/posts/2021-05-21-task-management-with-roam-vol7.html
  (defun rcy-capture-meeting-template ()
    (let ((person (read-from-minibuffer "Meeting with: ")))
      (org-capture-put :meeting-person person)
      (concat "* MEETING with "
              person
              " on [%<%Y-%m-%d %a>] :meeting:\n%U\n\n%?")))

  (defun rcy-capture-meeting-target ()
    (let ((person (org-capture-get :meeting-person)))
      (rcy-get-or-create-meeting-file person)))

  (defun rcy-get-or-create-meeting-file (person)
    (let ((filename (concat
                     "~/Dropbox/org/meetings/"
                     (format-time-string "%Y-%m-%d-%H%M") "-" person
                     ".org")))
      (when (not (file-exists-p filename))
        (save-excursion
          (find-file-literally filename)
          (org-with-point-at 1
            (insert "#+TITLE: " (concat "Meetings: " person))
            (org-id-get-create)
            (save-buffer)
            (kill-buffer))))
      (set-buffer (org-capture-target-buffer filename))
      (org-capture-put-target-region-and-position)
      (widen)))

  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "DELEGATED(g)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        `(("i" "Inbox" entry (file ,(format "~/Dropbox/org/inbox.%s.org" system-name))
           "\n* REFILE %?")

          ("m" "Meeting" entry
           (function rcy-capture-meeting-target)
           (function rcy-capture-meeting-template)
           :empty-lines 1
           :kill-buffer t)

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
   (setq org-agenda-todo-ignore-scheduled 'future)
  )

(use-package rcirc
  :config
  (setq rcirc-debug-flag t)
  (rcirc-track-minor-mode t)
  (setq rcirc-fill-column 'window-text-width)
  (setq rcirc-time-format "%H:%M:%S "))

(use-package winner
  :init
  (winner-mode +1)
  :bind (("<C-left>" . winner-undo)
         ("<C-right>" . winner-redo)))

(use-package ffap
  :init
  (ffap-bindings))

(use-package dired-x
  :init
  (set-default 'dired-omit-mode t))

(use-package deft
  :ensure t
  :bind (("C-c d" . deft))  
  :init
  (setq deft-directory "~/Dropbox/org")
  (setq deft-default-extension "org")
  (setq deft-markdown-mode-title-level 1))

(use-package comint
  :init
  (setq comint-buffer-maximum-size 10000)
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer))

(use-package magit
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :init
  ;; (defun lsp-go-install-save-hooks ()
  ;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun rcy/eglot-go-install-save-hooks ()
    (add-hook 'before-save-hook #'eglot-format-buffer t t)
    (add-hook 'before-save-hook
              (lambda () (call-interactively 'eglot-code-action-organize-imports))
              t t))

    ;;(add-hook 'before-save-hook #'eglot-code-action-organize-imports t t))

  ;;(add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook #'rcy/eglot-go-install-save-hooks)
  (add-hook 'go-mode-hook (lambda () (setq tab-width 8)))
  (load-library "~/elisp/go-scratch.el"))

(use-package dockerfile-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package git-link
  :ensure t)

;; (use-package server
;;   :ensure t
;;   :init
;;   (make-directory server-socket-dir t)
;;   (chmod server-socket-dir #o700)
;;   (ignore-errors (server-start)))

(use-package lsp-mode
  :ensure t
  :defer t
  ;; :hook (lsp-mode . (lambda ()
  ;;                     (let ((lsp-keymap-prefix "C-c l"))
  ;;                       (lsp-enable-which-key-integration))))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

  ;; make lsp look at web-mode-code-indent-offset, which it does not by default
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset)
  ;;:hook
  ;;(web-mode . lsp)
  )

(use-package lsp-ui
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (company-tng-mode)
  )

(use-package wgrep
  :ensure t)

(use-package graphql-mode
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package prog-mode
  :hook
  ((prog-mode . display-fill-column-indicator-mode)
   (prog-mode . (lambda () (setq fill-column 100)))))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))
