;; init.el
;; latrokles

;; set encoding and file formats
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq org-src-preserve-indentation t)

;; set binary path
(if (not (eq window-system 'w32))
  (add-to-list 'exec-path (getenv "PATH")))

;; do not modify init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; styling
(tool-bar-mode -1)                       ; disable toolbar
(savehist-mode 1)                        ; save minibuffer history
(global-auto-revert-mode 1)              ; automatically refresh buffe if file changes on disk
(recentf-mode 1)                         ; remember most recent files
(global-display-line-numbers-mode 1)     ; always show line numbers
(global-hl-line-mode 1)                  ; highlight current line
(show-paren-mode 1)                      ; highlight parens

(setq show-trailing-whitespace 1)        ; highlight blank space at the end of lines
(setq show-paren-style 'expression)      ; highlight expression inside parens
(setq make-backup-files nil)             ; stop creating backup~ files
(setq auto-save-default nil)             ; stop creating #autosave# files
(setq vc-follow-symlinks t)              ; always follow symlink and edit the file it points to directly
(setq ibuffer-expert t)                  ; stop prompting me to confirm killing a buffer
(setq-default mac-option-modifier 'meta) ; set alt/option as meta
(setq select-enable-clipboard t)         ; enable copying
(setq interporgram-paste-function        ; enable pasting
      (lambda ()
	(shell-command-to-string "pbpaste")))

(add-hook 'before-save-hook 'delete-trailing-whitespace)  ; delete trailing blank space on save
(add-hook 'text-mode-hook 'visual-line-mode)              ; wrap text
(defalias 'yes-or-no-p 'y-or-n-p)                         ; tired of typing "yes"


;; set standard package repositories
(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  ;; (add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)  ; Official MELPA Mirror, in case necessary.
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t))
(package-initialize)

;; TODO set up vertico and related

(use-package restart-emacs
  :ensure t)

(use-package which-key
  :ensure t

  :init
  (setq which-key-separator " ")
  (setq which-key-prefix "+")

  :config
  (which-key-mode 1))

(use-package dired-sidebar
  :ensure t
  :commands
  (dired-sidebar-toggle-sidebar))

(use-package company
  :ensure t
  :custom
  (global-company-mode 1))

;; narrowing search and completion https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; https://github.com/minad/consult#use-package-example
(use-package consult
  :ensure t)

(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-syntax 'simplified)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))


(use-package magit
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)


(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t)

  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode +1))

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'"
  :interpreter "swift")

(use-package rainbow-delimiters
    :ensure t
    :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package lsp-mode
  :ensure t
  :hook ((swift-mode . lsp))
  :commands lsp
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package lsp-sourcekit
    :ensure t
    :after lsp-mode)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-esc-mode 1)

  (use-package evil-visual-mark-mode
    :ensure t)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes))

(defun latrokles/configure-leader-keys ()
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "eb"   'eval-buffer
   "ee"   'eval-expression

   ;; file navigation
   "ff"   'consult-find
   "fg"   'consult-grep
   "ba"   'bookmark-set
   "bf"   'consult-bookmark

   ;; markdown control
   "ml"  'markdown-insert-link
   "mi"  'markdown-insert-image
   "mt"  'markdown-insert-table

   ;; lsp
   "lsu"  'lsp-ui-imenu
   "lsr"  'lsp-find-references
   "lse"  'flycheck-list-error
   "lsn"  'lsp-rename
   "lswr" 'lsp-worspace-restart
   "lsws" 'lsp-workspace-shutdown

   ;; rust mode
   "ras" 'lsp-rust-analyzer-status

   ;; narrow / widen
   "nd"  'narrow-to-defun
   "nr"  'narrow-to-region
   "nw"  'widen

   "g"    'magit-status
   "k"    'kill-this-buffer-and-window
   "W"    (lambda () (interactive) (save-buffer) (kill-this-buffer))))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (latrokles/configure-leader-keys))
