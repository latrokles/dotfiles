;;; init-cl.el

;;; Code:
(defun latrokles/configure-lisp-leaders ()
  "define leader keys for sly repl"
  (evil-leader/set-key
    "l" 'sly 				; start sly repl
    "srl" 'sly-restart-inferior-lisp 	; restart sly
    "sql" 'sly-quit-lisp 		; quit repl
    "scr" 'sly-connect 			; connet to a running slynk session
    "scd" 'sly-compile-defun 		; compile defun
    "scf" 'sly-compile-file 		; compile file
    "sed" 'sly-eval-defun 		; eval defun
    "sel" 'sly-eval-last-expression 	; eval last sexpr
    "suf" 'sly-undefine-function 	; undefine function
    "swc" 'sly-who-calls 		; find who calls this function
    "swu" 'sly-edit-uses 		; edit users of this symbol
    ))

(use-package lispy :ensure t)

(use-package evil-lispy
  :ensure t
  :after (evil lispy)
  :config
  (add-hook 'lisp-mode-hook #'evil-lispy-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
  (add-hook 'clojure-mode-hook #'evil-lispy-mode))

(use-package sly
  :ensure t
  :config
  (latrokles/configure-lisp-leaders)
  (setq sly-lisp-implementations
	`((ccl ("ccl64"))
	  (sbcl ("sbcl" "--noinform" "--no-linedit")))))

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sly-asdf
  :ensure t
  :after sly)


(provide 'init-cl)
