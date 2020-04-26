;;; init.el
					;-*-Emacs-Lisp-*-

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; would like to set my own initial scratch message, maybe later
(setq initial-scratch-message
      (concat
       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
       ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n"
       ";;\n"))

(package-initialize)

;; load more specific configs from lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; set binary path
(if (not (eq window-system 'w32))
    (add-to-list 'exec-path "/usr/local/bin"))

;; prevent emacs from modifying init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)
(require 'init-platform)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; -- basic settings
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(tool-bar-mode -1)           ; disable toolbar
(setq vc-follow-symlinks t)  ; allways follow the symlink and edit the file it points to directly
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq explicit-shell-file-name "/usr/local/bin/zsh")
(setq-default mac-option-modifier 'meta) ; set alt/option as meta

; automatically kill the ansi-term buffer after exiting terminal
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'oleh-term-exec-hook)

;; set a way to paste into ansi-term
(eval-after-load "ansi-term"
  '(define-key ansi-term-raw-map (kbd "C-c C-y") 'term-paste))

;; -- packages
(require 'init-evil-leader)
(require 'init-evil)
(require 'init-clojure)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t))

(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

;; make time to read: https://github.com/auto-complete/auto-complete
;; or consider switching over to https://github.com/company-mode/company-mode
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package helm
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer t
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck-haskell
  :ensure t
  :defer t)

(use-package smartparens
  :ensure t
  :defer t
  :config
  (require 'smartparens-config)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode))

(use-package magit
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package perspective
  :ensure t
  :config
  (persp-mode))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c C-s") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (projectile-mode +1))

(use-package all-the-icons :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  ;; maybe a good thing to come back and look at
  ;; https://github.com/Alexander-Miller/treemacs#installation later for
  ;; configuration options

  (use-package treemacs-evil
    :after treemacs evil
    :ensure t)

  (use-package treemacs-projectile
    :after treemacs projectile
    :ensure t)

  (use-package treemacs-icons-dired
    :after treemacs dired
    :ensure t
    :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :after treemacs magit
    :ensure t)

  (use-package treemacs-persp
    :after treemacs persp-mode
    :ensure t
    :config (treemacs-set-scope-type 'Perspectives))

  (evil-leader/set-key
    "to" 'treemacs
    "tf" 'treemacs-find-file
    "tp" 'treemacs-projectile
    "tca" 'treemacs-add-project-to-workspace
    "tco" 'treemacs-collapse-all-projects))

(use-package lsp-mode
  :commands lsp
  :ensure t)

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :after lsp-mode)

(use-package lsp-treemacs
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

(use-package helm-lsp
  :ensure t
  :defer t)

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t
  :after lsp-mode lsp-ui)

(use-package dap-java
  :after (lsp-java))

(use-package kotlin-mode
  :ensure t
  :defer t)

(use-package engine-mode
  :ensure t
  :defer t
  :config
  (defengine ddg "https://duckduckgo.com/?q=%s")
  (defengine g "https://google.com/search?ie=utf-8&oe=utf-8&q=%s")
  (defengine gh "https://github.com/search?ref=simplesearch&q=%s")
  (defengine yt "https://www.youtube.com/results?aq=f&oq=&search_query=%s"))

(use-package oauth2
  :ensure t
  :defer t)

(use-package twittering-mode
  :ensure t
  :defer t)

(provide 'init)
