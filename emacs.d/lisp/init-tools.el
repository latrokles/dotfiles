;;; init-tools.el

(defun latrokles/commit-current-file ()
  (interactive)
  (let ((path (buffer-file-name))
	(commit-message  (read-string "enter commit message: ")))
    (shell-command (concat "git add " path))
    (shell-command (concat "git commit -m " "\"" commit-message "\""))
    (message "file committed successfully")))

(provide 'init-tools)
