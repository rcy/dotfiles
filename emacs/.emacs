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
 '(ido-mode 'buffer nil (ido))
 '(indent-tabs-mode nil)
 '(org-agenda-files
   '("~/org/projects/ryanyeske.com.org" "~/org/projects/collaborama.org" "~/org/projects/surabhi.org" "~/org/projects/personal.org" "~/org/shopping.org"))
 '(org-agenda-span 'day)
 '(org-archive-location "~/org/.archive.org::")
 '(org-drill-save-buffers-after-drill-sessions-p nil)
 '(org-drill-scope '("~/org/drill.org"))
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(org-stuck-projects
   '("+project-someday/-DONE"
     ("WAITING" "TODO" "NEXT" "DELEGATED" "CANCELLED")
     ("ignore" "someday")
     ""))
 '(package-selected-packages
   '(org-roam origami xterm-color graphql-mode org-drill web-mode nix-mode yaml-mode projectile magit use-package))
 '(rcirc-server-alist
   '(("irc.libera.chat" :nick "rcy" :port 6697 :user-name "rcy" :password "foobar" :channels
      ("#emb #rcirc #emacs #djfullmoon")
      :encryption tls)))
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-enable-comment-interpolation t)
 '(web-mode-enable-engine-detection t)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(global-set-key (kbd "C-c k") 'comment-region)

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
;;; org-roam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c n r") #'org-roam-buffer-toggle-display)
(global-set-key (kbd "C-c n i") #'org-roam-insert)
(global-set-key (kbd "C-c n /") #'org-roam-find-file)
(global-set-key (kbd "C-c n t") #'org-roam-dailies-capture-today)
