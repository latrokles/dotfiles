(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))

  (use-package helm-lsp
    :ensure t
    :commands helm-lsp-workspace-symbol)

  (add-hook 'python-mode-hook #'lsp)
  (setq lsp-pyls-server-command "~/bin/start-python-lsp.sh")
  :commands lsp)

(provide 'init-lsp)
