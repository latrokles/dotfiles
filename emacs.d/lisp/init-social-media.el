;;; init-social-media.el
;;; Commentary:

;; catch all for internet forums, twitter, chat

;;; Code:
(use-package twittering-mode
  :ensure t
  :defer t
  :config
  (setq twittering-icon-mode t)
  (setq twittering-timer-interval 1800)
  (setq twittering-timer-interval-for-redisplaying 180))

(use-package md4rd
  :ensure t
  :defer t
  :config
  (add-to-list 'evil-emacs-state-modes 'md4rd-mode)
  (setq md4rd-subs-active '(aws
			    climbharder
			    climbing
			    clojure
			    clojurescript
			    compilers
			    compsci
			    emacs
			    haskell
			    programming
			    racket
			    serverless
			    unisonweb)))

(use-package helm-lobsters
  :ensure t
  :defer t)

(provide 'init-social-media)
