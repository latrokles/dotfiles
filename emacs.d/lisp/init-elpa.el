;;; init-elpa.el --- Initialize elpa

;;; This is mostly lifted from Aaron Bieber's configuration

(require 'package)

;; set package repositories
(setq package-archives '(
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages")))


;; enable signature checking. if gpg is not found it will fail, this fucntion
;; enables this only if it finds gpg.
(defun sanityinc/package-maybe-enable-signatures ()
  "Conditionally enable signature checking if gpg is available."
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

;; enable signature checking after $PATH has been configured (in case it failed above)
(sanityinc/package-maybe-enable-signatures)
(after-load 'init-exec-path
	    (sanityinc/package-maybe-enable-signatures))

;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))

;; don't do this because it's being handled in init.el
(setq package-enable-at-startup nil)

(provide 'init-elpa)

