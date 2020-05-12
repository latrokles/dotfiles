;;; init-org.el
;;; Commentary:

;;; Code:

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
				 (clojure . t)))

  ;; indent text to match heading in org mode
  (add-hook 'org-mode 'org-indent-mode)

  ;; set up top level org dir
  (setq org-directory "~/org")

  ;; set up org capture
  (setq org-capture-templates
	`(("t" "Capture Tasks")
	  ("tp" "Todo (self)" entry
	   (file+headline "org-self/todo.org" "Tasks")
	   "* TODO %?\n %i\n %a")
	  ("tw" "Todo (work)" entry
	   (file+headline "org-work/todo.org" "Tasks")
	   "* TODO %?\n %i\n %a")
	  ("n" "Capture Notes")
	  ("np" "Notes (self)" entry
	   (file+datetree "org-self/notes.org")
	   "* %?\nEntered on %U\n %i\n %a"
	   :empty-lines 1)
	  ("nw" "Notes (work)" entry
	   (file+datetree "org-work/notes.org")
	   "* %?\nEntered on %U\n %i\n %a"
	   :empty-lines 1)
	  ("b" "Capture Bookmarks")
	  ("bp" "Bookmarks (self)" entry
	   (file+datetree "org-self/bookmarks.org")
	   ,(concat "* TODO %? %:description\n"
		    ":PROPERTIES:\n"
		    ":CREATED: %u\n"
		    ":END:\n")
	   :empty-lines 1)
	  ("bw" "Bookmarks (work)" entry
	   (file+datetree "org-work/bookmarks.org")
	   ,(concat "* TODO %? %:description\n"
		    ":PROPERTIES:\n"
		    ":CREATED: %u\n"
		    ":END:\n"
		    ":END:\n")
	   :empty-lines 1))))

(provide 'init-org)
