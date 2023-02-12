;; adapted from https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(defun rcy/roam-agenda-current-buffer-roam-p ()
  "Returns non-nil if current buffer is a roam note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun rcy/roam-agenda-current-buffer-has-todo-p ()
  "Returns non-nil if current buffer has an incomplete todo entry."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))

(defun rcy/roam-agenda-files ()
  "Return a list of roam files containing :agenda: tag."
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
              :from tags
              :left-join nodes
              :on (= tags:node-id nodes:id)
              :where (like tag (quote "%\"agenda\"%")) :or (like tag (quote "%\"force\"%"))]))))

(defun rcy/roam-agenda-current-buffer-update-tag ()
  "Add or remove :agenda: filetag if incomplete todo entry in current buffer."
  (when (rcy/roam-agenda-current-buffer-roam-p)
    (org-with-point-at 1
      (if (rcy/roam-agenda-current-buffer-has-todo-p)
          (org-roam-tag-add ["agenda"])
        (ignore-error user-error
          (org-roam-tag-remove ["agenda"]))))))

;; prevent every node in the file inheriting the :agenda: tag
(add-to-list 'org-tags-exclude-from-inheritance "agenda")

;;(remove-hook 'find-file-hook #'rcy/roam-agenda-current-buffer-update-tag)
(add-hook 'before-save-hook #'rcy/roam-agenda-current-buffer-update-tag)
(add-hook 'org-capture-before-finalize-hook #'rcy/roam-agenda-current-buffer-update-tag)

(defun rcy/roam-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (message "Updating org-agenda-files...")
  (setq org-agenda-files (rcy/roam-agenda-files))
  (message "Updating org-agenda-files...done"))

(advice-add 'org-agenda :before #'rcy/roam-agenda-files-update)

(defun rcy/roam-agenda-update-files ()
  "Update :agenda: tags in roam files."
  (interactive)
  (dolist (file (org-roam-list-files))
    (message "processing %s" file)
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (rcy/roam-agenda-current-buffer-update-tag)
      (save-buffer))))

(provide 'rcy-roam-agenda)
