;;; init-scheme.el
;;; Commentary:

;; configure emacs for working scheme programming (gerbil, chicken, guile, racket)

;;; Code:

(use-package scheme-complete :ensure t)

;; https://www.nongnu.org/geiser/
(use-package geiser
  :ensure t
  :defer t
  :config
  (use-package quack
    :ensure t)

  (setq geiser-mode-start-repl-p t
       geiser-repl-history-filename "~/.emacs.d/.cache/geiser-history"))

;; racket mode
(use-package racket-mode
  :ensure t
  :init
  (progn
    (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist)
    (delete '("\\.rktd\\'" . scheme-mode) auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.rkt[dl]?\\'" . racket-mode)))

  :config
  (use-package scribble-mode
    :ensure t)

  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (put 'union-case 'racket-indent-function 2)
  (put 'fresh 'racket-indent-function 1)
  (put 'do 'racket-indent-function 0)
  (put 'syntax-id-rules 'racket-indent-function 1))

(provide 'init-scheme)
