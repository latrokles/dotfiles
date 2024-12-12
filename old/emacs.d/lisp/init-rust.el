;; init-rust.el

(use-package rust-mode
  :ensure t
  :init (setq rust-path-dir (format "%s/%s" (getenv "HOME") "./cargo/bin"))
  :hook
  (rust-mode . lsp)
  :config
  (evil-leader/set-key "rr" 'rust-run)
  (evil-leader/set-key "rc" 'rust-run-clippy)

  (setq rust-format-on-save t)
  (setq lsp-enable-snippet nil)
  (add-hook 'rust-mode-hook
	    (lambda () (prettify-symbols-mode))))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode
  :ensure t)

(provide 'init-rust)
