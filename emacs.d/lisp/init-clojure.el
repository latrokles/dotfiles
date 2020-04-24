;;; init-clojure.el
;;; Commentary:

;; a place to put all my clojure related configurations

;;; Code:

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'linum-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'idle-highlight-mode)

  :config

  (use-package cider
    :ensure t
    :defer t
    :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
    :diminish subword-mode
    :config
    (setq nrepl-log-messages t
	  cider-repl-display-in-current-window t
	  cider-repl-use-clojure-font-lock t    
	  cider-prompt-save-file-on-load 'always-save
	  cider-font-lock-dynamically '(macro core function var)
	  nrepl-hide-special-buffers t            
	  cider-overlays-use-font-lock t)         
    (cider-repl-toggle-pretty-printing)

    (use-package cider-eval-sexp-fu :defer t))

  (use-package clj-refactor
    :ensure t
    :defer t
    :diminish clj-refactor-mode
    :config (cljr-add-keybindings-with-prefix "C-c C-m")))
  
(provide 'init-clojure)
