;;; init-platform.el --- Platform specific settings
;;; 
;;; Right now this is a series of osx only configuration and a bunch of
;;; hacks to get some things working right now. It'll see a lot of
;;; changes as I learn more about emacs and evil (would like to use evil
;;; bindings for some of these.

;; OSX control from emacs
;; some configuration to get emacs working with chunkwm
;; a lot of skhd shortcuts interfere with emacs (and terminal commands)
;; so I have disabled skhd in both emacs and iterm, but I can have emcas
;; execute commands to the chunkwm daemon to place the current emacs frame
;; wherever I want it.
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
(defun volume-up (level)
  "raise volume by 10%"
  (interactive)
  (start-process-shell-command "" nil "volume_up"))

(defun volume-down (level)
  "lower volume by 10%"
  (interactive)
  (start-process-shell-command "" nil "volume_down"))
 
(defun mute nil
  "mute audio"
  (interactive)
  (start-process-shell-command "" nil "mute"))

(defun unmute nil
  "unmute audio"
  (interactive)
  (start-process-shell-command "" nil "unmute"))

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

(provide 'init-platform)

