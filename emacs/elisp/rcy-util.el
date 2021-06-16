(defun rcy-insert-time ()
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun rcy-insert-random-id ()
  "Return a unique identifier, such as `DfzDKe5QBntWqW7ij`."
  (interactive)
  (let* ((unmistakable-chars "23456789ABCDEFGHJKLMNPQRSTWXYZabcdefghijkmnopqrstuvwxyz")
         (len (length unmistakable-chars)))
    (dotimes (i 17)
      (insert (elt unmistakable-chars (random len))))))

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
