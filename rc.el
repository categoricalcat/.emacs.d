(require 'package)

;; add melpa to package-archives, there is a stable version as well
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  "Refresh package contents once per session."
  (unless rc/package-contents-refreshed
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-package (package)
  "Ensure PACKAGE is installed and loaded."
  (unless (package-installed-p package)
    (rc/package-refresh-contents-once)
    (package-install package))
  (require package))

(defun rc/require (&rest packages)
  "Ensure all PACKAGES are installed and loaded."
  (dolist (package packages)
    (rc/require-package package)))

(defun rc/require-theme (theme)
  "Ensure THEME's package is installed and load the theme."
  (let ((theme-package (intern (concat (symbol-name theme) "-theme"))))
    (rc/require-package theme-package)
    (load-theme theme t)))

