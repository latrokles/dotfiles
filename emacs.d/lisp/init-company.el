;;; init-company.el
;;; Commentary:

;;; Code:

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-require-match 'never)
  (setq company-idle-delay 0.2)
  (setq company-show-numbers t))

(use-package company-box
  :ensure t
  :after company
  :hook (global-company-mode . company-box-mode))

(provide 'init-company)
