;;; init-wiki.el
;;; Commentary:
;;;
;;; some settings for personal logs and task tracking (not using ORG... I know... the HORROR!)

(setq latrokles-wiki-path (getenv "WIKIDIR"))
(setq latrokles-logs-path (expand-file-name "logs" latrokles-wiki-path))

(defun latrokles/open-todays-log ()
  (interactive)
  (let ((file-name (concat (format-time-string "%Y-%m-%d")
			   ".md")))
    (find-file (expand-file-name file-name
				 latrokles-logs-path))))

(defun latrokles/commit-logs ()
  (interactive)
  (start-process-shell-command ""
			       nil
			       "commit_logs"))

(defun latrokles/commit-bookmarks ()
  (interactive)
  (start-process-shell-command ""
			       nil
			       "commit_bookmarks"))

(evil-leader/set-key
  "je" 'latrokles/open-todays-log)

(evil-leader/set-key
  "jc" 'latrokles/commit-logs)

(evil-leader/set-key
  "bc" 'latrokles/commit-bookmarks)

(provide 'init-wiki)
