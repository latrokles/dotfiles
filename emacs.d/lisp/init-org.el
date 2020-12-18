;;; init-org.el
;;; Commentary:

;;; Code:
(defun latrokles/org-capture-file-path (path)
  "Given a directory path, prompt the user for a name and
prepend it with the path and the timestamp.

The returned file path is of the form path/YYYYmmdd-HHMM-name.org"
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s-%s.org"
			      (format-time-string "%Y%m%d-%H%M")
			      name)
		      path)))

(defun latrokles/org-capture-date-only-path (path)
  "Given a directory path return a filepath that uses the current
date.

The returned file path is of the form path/YYYYmmdd.org"
  (let ((filename (format-time-string "%Y%m%d.org")))
    (expand-file-name filename path)))

(use-package org
  :ensure org-plus-contrib
  :config

  (require 'org-tempo)
  (require 'org-protocol)

  (use-package ob-clojure
    :init
    (setq org-babel-clojure-backend 'cider))

  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell . t)
				 (emacs-lisp . t)
				 (scheme . t)
				 (clojure . t)))

  ;; set up top level org dir
  (setq org-directory "~/notizbuch")

  ;; set up org capture
  ;; keeping things super simple. This also means potentially
  ;; not taking full advantage of everything that org-mode and
  ;; emacs can provide, but last time I went with something
  ;; more elaborate it fell out of use fairly quickly.
  (setq org-capture-templates
	`(("z"
	   "zettel - little notes"
	   plain
	   (file ,(concat org-directory "/zettel/" (format-time-string "%Y%m%d-%H%M.org")))
	   "#+TITLE:%?\n#+CREATED: %U\n#+KEYWORDS:")
	  ("t"
	   "tracker - track shit"
	   item
	   (file ,(concat org-directory "/tracker/current.org"))
	   "%u|%?|||")
	  ("u"
	   "unerledigt - the pending stuff"
	   checkitem
	   (file ,(concat org-directory "/tasks/unerledigt.org"))
	   "[ ] %u:%?"))))

(provide 'init-org)
