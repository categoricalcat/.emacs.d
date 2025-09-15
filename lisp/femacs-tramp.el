;;; femacs-tramp.el --- TRAMP -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Enable TRAMP
(require 'tramp)

;; Set default SSH method (recommended)
(setq tramp-default-method "ssh")

(setq tramp-use-ssh-controlmaster-options t)

(setq tramp-verbose 6
  password-cache-expiry 3600)

;; This message is helpful for confirming your setup.
(add-hook 'after-init-hook
  (lambda ()
    (message "TRAMP method: %s, ControlMaster: %s"
      tramp-default-method
      (if tramp-use-ssh-controlmaster-options "enabled" "disabled"))))

(provide 'femacs-tramp)
;;; femacs-tramp.el ends here
