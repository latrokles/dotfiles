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

;; gopher and gemini client
;; https://thelambdalab.xyz/elpher/
;; https://gemini.circumlunar.space/clients.html
(use-package elpher
  :ensure t)

;;(use-package soundklaus
;;  :ensure t
;;  :commands
;;  (soundklaus-activities
;;   soundklaus-connect
;;   soundklaus-my-favorites
;;   soundklaus-my-playlists
;;   soundklaus-my-tracks
;;   soundklaus-playlists
;;   soundklaus-tracks))

;; (require 'emms-setup)
;; (emms-standard)
;; (emms-default-players)

(provide 'init-social-media)
