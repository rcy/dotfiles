;;; browse-jira-at-point.el
;;; 2021-12-28T16:44:58-0700
;;;
;;; Author: emacs@ryanyeske.com
;;;
;;; When calling function with point near text like XY-12, browse to jira issue
;;;
;;; (global-set-key (kbd "C-c j") 'rcy/browse-jira-issue-at-point)

(defvar rcy/browse-jira-url-format "https://precisionnutrition.atlassian.net/browse/%s")

(defun rcy/jira-issue-at-point ()
  (let ((regex "\\b[a-z]+-[0-9]+\\b"))
    (when (or
           (looking-at regex)           ; _XY-12
           (save-excursion
             (backward-word)
             (looking-at regex))        ; X_Y-12, XY_-12, XY-_12
           (save-excursion
             (backward-word)
             (backward-word)
             (looking-at regex)))       ; XY-1_2, XY-12_
      (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

(defun rcy/browse-jira-issue-at-point ()
  (interactive)
  (let ((issue (completing-read "Jira Issue: " nil nil nil (rcy/jira-issue-at-point))))
    (browse-url (format rcy/browse-jira-url-format issue))))

(provide 'jira-at-point)
