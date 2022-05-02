;;; init-tools.el

(defun latrokles/commit-current-file ()
  (interactive)
  (let ((path (buffer-file-name))
	(commit-message  (read-string "enter commit message: ")))
    (shell-command (concat "git add " path))
    (shell-command (concat "git commit -m " "\"" commit-message "\""))
    (message "file committed successfully")))

(evil-leader/set-key
  "cf" 'latrokles/commit-current-file)

(provide 'init-tools)
