(defvar go-scratch-base-directory "~/personal/go-scratch")
(defun go-scratch ()
  (interactive)
  (let ((dir (format "%s/%s" go-scratch-base-directory (format-time-string "%Y%m%d-%H%M"))))
    (make-directory dir t)
    (find-file (file-name-concat dir "main.go"))
    (when (eq (point-min) (point-max))
      (insert "// -*- compile-command: \"go run main.go\"; -*-
package main

import (
	\"fmt\"
)

func main() {
	fmt.Println(\"Hello, scratch\")
}
")
      (hack-local-variables))))
