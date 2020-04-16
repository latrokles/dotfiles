(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(package-selected-packages
   (quote
    (evil-magit magit powerline twittering-mode evil-tabs dracula-theme helm use-package evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; install selected packages (from package-selected-packages) automatically
(package-install-selected-packages)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(tool-bar-mode -1)           ; disable toolbar


(require 'powerline)
(require 'evil)
(evil-mode t)
(load-theme 'dracula t)

(require 'tramp)
(setq tramp-default-method "scpx")

;; SOME ANSI-TERM CONFIGURATION
(setq explicit-shell-file-name "/usr/local/bin/zsh")   ; use brew zsh default
;(evil-set-initial-state 'term-mode 'emacs)             ; use emacs mode in ansi-term

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

; paste from outside into ansi term with C-C C-y
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

; set option as meta
(setq-default mac-option-modifier 'meta)

;; some configuration to get emacs working with chunkwm
;; a lot of skhd shortcuts interfere with emacs (and terminal commands)
;; so I have disabled skhd in both emacs and iterm, but I can have emcas
;; execute commands to the chunkwm daemon to place the current emacs frame
;; wherever I want it.
(defun swap-window (direction)
  (call-process-shell-command (concat "chunkc tiling:window --swap " direction) nil t))

(defun swap-right nil
  (interactive)
  (swap-window "east"))

(defun swap-left nil
  (interactive)
  (swap-window "west"))

(defun swap-up nil
  (interactive)
  (swap-window "north"))

(defun swap-down nil
  (interactive)
  (swap-window "south"))
