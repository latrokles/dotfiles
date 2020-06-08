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
(tool-bar-mode -1)           ; disable toolbar
(savehist-mode 1)            ; save minibuffer history (per machine really)
(global-linum-mode 1)        ; always show line numbers
(global-hl-line-mode 1)      ; highlight current line
(show-paren-mode 1)          ; highlight parens
(setq show-paren-style 'expression) ; highlight expression inside parens
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq vc-follow-symlinks t)  ; allways follow the symlink and edit the file it points to directly
(setq ibuffer-expert t)      ; stop prompting me to confirm killing a buffer
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default mac-option-modifier 'meta) ; set alt/option as meta
(defalias 'yes-or-no-p 'y-or-n-p) ; I'm tired of typing yes (yolo!)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set a way to paste into ansi-term
(eval-after-load "ansi-term"
  '(define-key ansi-term-raw-map (kbd "C-c C-y") 'term-paste))

;; -- packages
(require 'init-evil-leader)
(require 'init-evil)
(require 'init-clojure)
(require 'init-scheme)
(require 'init-company)

(use-package restart-emacs :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t))

(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

(use-package helm
  :ensure t
  :defer t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("C-x p" . helm-projectile-switch-project))
  :config
  (use-package helm-projectile
    :ensure t)

  (use-package helm-company
    :ensure t)

  (use-package helm-eww
    :ensure t)

  (use-package helm-osx-app
    :ensure t
    :defer t
    :config
    (evil-leader/set-key
      "ha" 'helm-osx-app)))

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

  (evil-leader/set-key
    "pc" 'projectile-command-map)

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
  :ensure t
  :init (setq lsp-keymap-prefix "<f1>"))

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
  :defer t
  :config (add-hook 'kotlin-mode-hook 'lsp))

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

(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package doc-view
  :magic ("%pdf" . pdf-view-mode))

(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

(require 'init-org)
(require 'init-org-roam)
(require 'init-social-media)

(use-package zoom
  :ensure t
  :config
  (zoom-mode t))

(use-package xkcd
  :commands (xkcd-get xkcd-get-latest)
  :ensure t)

(use-package dashboard
  :ensure t
  :config

  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
	`(;; line 1
	  ((,(all-the-icons-alltheicon "git" :height 0.8 :v-adjust 0.0)
	    "code"
	    "Dired to Code"
	    (lambda (&rest _) (dired "~/src/github.com/latrokles")))
	   (,(all-the-icons-fileicon "org" :height 0.8 :v-adjust 0.0)
	    "org"
	    "Dired to Org"
	    (lambda (&rest _) (dired "~/org")))
	   (,(all-the-icons-octicon "home" :height 0.8 :v-adjust 0.0)
	    "home"
	    "Dired to Home"
	    (lambda (&rest _) (dired "~")))
	   (,(all-the-icons-fileicon "emacs" :height 0.8 :v-adjust 0.0)
	    "init.el"
	    "Open init.el config file"
	    (lambda (&rest _) (find-file "~/.emacs.d/init.el"))))))

  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  :custom
  (dashboard-items '((recents . 5)
		     (projects . 5))))

;; -- ibuffer config

(use-package ibuffer-sidebar
  :ensure t
  :config
  (evil-leader/set-key
    "tib" 'ibuffer-sidebar-toggle-sidebar))

(use-package imenu-list
  :ensure t
  :config
  (evil-leader/set-key
    "tim" 'imenu-list-smart-toggle))

;; shell configuration
(use-package multi-term
  :ensure t
  :bind
  ("<f5>" . 'multi-term-dedicated-toggle)

  :config
  (setq multi-term-program "/usr/local/bin/zsh")
  (add-hook 'term-mode-hook (lambda ()
			      (setq term-buffer-maximum-size 10000)
			      (define-key term-raw-map (kbd "C-c y") 'term-paste)
			      (linum-mode 0)))
  (add-hook 'term-mode-hook (lambda ()
			      (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
			      (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev)))))

; automatically kill the ansi-term buffer after exiting terminal
(setq explicit-shell-file-name "/usr/local/bin/zsh")
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'oleh-term-exec-hook)


;; run server if it's not running
(load "server")
(setq server-socket-dir "~/.emacs.d/server")
(unless (server-running-p) (server-start))

(provide 'init)
