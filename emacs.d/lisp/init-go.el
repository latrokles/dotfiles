;;; init-go.el
(use-package go-mode
  :ensure t
  :mode "\\.go$"
  :config
  (add-to-list 'exec-path "/opt/homebrew/bin/")
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'init-go)
