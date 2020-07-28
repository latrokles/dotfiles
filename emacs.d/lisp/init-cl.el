;;; init-cl.el

;;; Code:
(use-package lispy
  :ensure t
  :defer
  :config)

(use-package evil-lispy
  :ensure t
  :after (evil lispy)
  :config
  (add-hook 'lisp-mode-hook #'evil-lispy-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
  (add-hook 'clojure-mode-hook #'evil-lispy-mode))

(use-package sly
  :ensure t
  :config
  (setq sly-lisp-implementations
	`((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit"))
	  (ccl ("/usr/local/bin/ccl64")))))

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sly-asdf
  :ensure t
  :after sly)

(provide 'init-cl)
