;;; init-evil.el -- My evil mode configuration.
;;; Commentary:

;; this is likely going to grow a lot

;;; Code:
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  
  (use-package evil-visual-mark-mode
    :ensure t)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes))

(provide 'init-evil)
