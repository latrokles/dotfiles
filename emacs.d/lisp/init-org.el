;;; init-org.el
;;; Commentary:

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :config

  (use-package ob-clojure
    :init
    (setq org-babel-clojure-backend 'cider))

  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell . t)
				 (emacs-lisp . t)
				 (clojure . t)))

  ;; indent text to match heading in org mode
  (add-hook 'org-mode 'org-indent-mode))

(provide 'init-org)
