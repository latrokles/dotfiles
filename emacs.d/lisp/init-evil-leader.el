;;; init-evil-leader.el
;;; Commentary:

;; I was not able to use this with a config I was reading online
;; and I spent an inordinate amount of time trying to get it
;; working, so now I'm trying to get this on its own and invoke
;; it before evil-mode per:
;; https://github.com/cofi/evil-leader/blob/master/README.org

;;; Code:
(defun latrokles/config-evil-leader ()
  "Configure evil leader bindings"
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "d" 'kill-this-buffer
    "eb" 'eval-buffer
    "ee" 'eval-expression
    "g" 'magit-status
    "W" (lambda () (interactive) (save-buffer) (kill-this-buffer))))
  
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (latrokles/config-evil-leader))

(provide 'init-evil-leader)
