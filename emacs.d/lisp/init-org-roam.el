;;; init-org-roam.el
;;; Commentary:

;; keep init-org from growing too much

;;; Code:

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/wald")
  :bind
  (:map org-roam-mode-map
	(("C-c n l" . org-roam)
	 ("C-c n f" . org-roam-find-file)
	 ("C-c n g" . org-roam-show-graph))
	:map org-mode-map
	(("C-c n i" . org-roam-insert)))
  :config
  (setq org-roam-completion-system 'helm)
  (setq org-roam-capture-templates
	'(("d" "default" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "org/%<%Y%m%d>-${slug}"
	   :head "#+TITLE: ${title}\n#+CREATED_AT: %U\n#+ROAM_TAGS:\n\n%x"
	   :unnarrowed t)
	  ("p" "private" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "org/private/%<%Y%m%d>-${slug}"
	   :head "#+TITLE: ${title}\n#+CREATED_AT: %U\n#+ROAM_TAGS:\n\n%x"
	   :unnarrowed t)
	  ("w" "website")
	  ("wd" "draft" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "journal/_drafts/%<%Y-%m-%d>-${slug}"
	   :head "#+TITLE: ${title}\n#+CREATED_AT: %U\n#+LAYOUT: post\n#+ROAM_TAGS:\n\n%x"
	   :unnarrowed t)
	  ("wp" "post" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "journal/_posts/%<%Y-%m-%d>-${slug}"
	   :head "#+TITLE: ${title}\n#+CREATED_AT: %U\n#+LAYOUT: post\n#+ROAM_TAGS:\n\n%x"
	   :unnarrowed t))))

(use-package company-org-roam
  :ensure t
  :config
  (push 'company-org-roam company-backends))

(use-package deft
  :ensure t
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/wald"))

(provide 'init-org-roam)
