;;; init-unison.el
(use-package unisonlang-mode
  :ensure t
  :config
  (add-hook 'after-init-hook #'auto-revert-mode)
  :mode (("\\.u\\'" . unisonlang-mode)))

(provide 'init-unison)
