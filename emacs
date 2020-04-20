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
    (auto-complete flycheck-haskell exec-path-from-shell flycheck oauth2 yaml-mode engine-mode markdown-mode evil-magit magit powerline twittering-mode evil-tabs dracula-theme helm use-package evil-visual-mark-mode))))
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

(require 'evil)
(evil-mode t)
(load-theme 'dracula t)

(require 'powerline)
(powerline-center-evil-theme)

;; basic auto complete config
;; make time to read: https://github.com/auto-complete/auto-complete
(ac-config-default)

;; copy paste per: https://apple.stackexchange.com/a/127082
(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)


(require 'tramp)
(setq tramp-default-method "scpx")

;; SOME ANSI-TERM CONFIGURATION
(setq explicit-shell-file-name "/usr/local/bin/zsh")   ; use brew zsh default

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

;; using engine mode to perform-searches in browser
(require 'engine-mode)
(engine-mode t)

;; duckduckgo (mainline)
(defengine ddg
  "https://duckduckgo.com/?q=%s")
  
;; google as a fallback
(defengine goog
  "https://google.com/search?ie=utf-8&oe=utf-8&q=%s")

;; github
(defengine gh
  "https://github.com/search?ref=simplesearch&q=%s")

;; youtube
(defengine yt
  "https://www.youtube.com/results?aq=f&oq=&search_query=%s")

;; for flycheck
(exec-path-from-shell-initialize)

;; OSX control from emacs
;; some configuration to get emacs working with chunkwm
;; a lot of skhd shortcuts interfere with emacs (and terminal commands)
;; so I have disabled skhd in both emacs and iterm, but I can have emcas
;; execute commands to the chunkwm daemon to place the current emacs frame
;; wherever I want it.

;; ------ window manager
(defun wm-swap-window (direction)
  "move the active emacs buffer to direction [east, west, north, south]"
  (start-process-shell-command "" nil (concat "yabai -m window --swap " direction)))

(defun wm-swap-window-e nil
  (interactive)
  (wm-swap-window "east"))

(defun wm-swap-window-w nil
  (interactive)
  (wm-swap-window "west"))

(defun wm-swap-window-n nil
  (interactive)
  (wm-swap-window "north"))

(defun wm-swap-window-s nil
  (interactive)
  (wm-swap-window "south"))

;; use C-(wasd) to interact with chunkc
(global-set-key (kbd "C-c h") 'wm-swap-window-w)
(global-set-key (kbd "C-c j") 'wm-swap-window-s)
(global-set-key (kbd "C-c k") 'wm-swap-window-n)
(global-set-key (kbd "C-c l") 'wm-swap-window-e)

;; control Mac OS volume from emacs
;; pass an integer from 0 - 100
(defun set-volume (level)
  (interactive "sVolume level [0-100]: ")
    (start-process-shell-command "" nil (concat "osascript -e \"set Volume output volume " level "\"")))
 
;; for the two below I should use an integer and convert to a string?
(defun mute nil
  (interactive)
  (start-process-shell-command "" nil "osascript -e \"set Volume output muted true\""))

(defun unmute nil
  (interactive)
  (start-process-shell-command "" nil "osascript -e \"set Volume output muted false\""))

