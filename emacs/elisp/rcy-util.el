(defun rcy-insert-time ()
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun rcy-insert-random-id (chars)
  "Return a unique identifier, such as `DfzDKe5QBntWqW7ij`."
  (interactive "p")
  (let* ((unmistakable-chars "23456789ABCDEFGHJKLMNPQRSTWXYZabcdefghijkmnopqrstuvwxyz")
         (len (length unmistakable-chars)))
    (dotimes (i chars)
      (insert (elt unmistakable-chars (random len))))))
(global-set-key (kbd "C-c i") 'rcy-insert-random-id)

(defun rcy-prettify-json-region ()
  "Prettify json in region"
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "python -m json.tool"
   nil
   t))

(defun xkcd-password ()
  (with-current-buffer (get-buffer-create "*words*")
    (when (= (buffer-size) 0)
      (erase-buffer)
      (print "hello")
      (insert-file-contents-literally "/usr/share/dict/words")
      (let ((case-fold-search nil))
        (keep-lines "^[a-z]\\{4,8\\}$" 0 (point-max))))
    ;; choose 4 random words
    (let ((buffer-line-count (count-lines (point-min) (point-max))))
      (mapconcat (lambda (word)
                   (goto-char (point-min))
                   (forward-line (1+ (random buffer-line-count)))
                   (current-word))
                 (list 1 2 3 4)
                 "-"))))

(defun rcy-insert-xkcd-password ()
  "Insert a xkcd style password."
  (interactive)
  (insert (xkcd-password)))

(defun rcy/org-grep (regexp)
  (interactive "sorg-grep regex: ")
  (grep-compute-defaults)
  (rgrep regexp "*.org" org-directory))

(defvar rcy-println-debug-count 0)

(defun rcy-println-debug ()
  (interactive)

  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command)
  (insert "fmt.Println(\"DEBUGX ")
  (rcy-insert-random-id 4)
  (insert (format " %d" rcy-println-debug-count))
  (insert "\")")
  (setq rcy-println-debug-count (1+ rcy-println-debug-count)))

(provide 'rcy-util)
