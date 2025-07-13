;;; femacs-tramp.el --- TRAMP -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Enable TRAMP
(require 'tramp)

;; Set default SSH method (recommended)
(setq tramp-default-method "sshx")

;; Optional: Reuse SSH connections for speed (uses ControlMaster)
(setq tramp-use-ssh-controlmaster-options nil)

(setq tramp-default-user "fufu"
  tramp-verbose 6
  password-cache-expiry 3600
  tramp-ssh-controlmaster-options "")  ; Ensure ControlMaster is disabled

;; Verify TRAMP settings on load
(add-hook 'after-init-hook
  (lambda ()
    (message "TRAMP method: %s (sshx recommended for Prelude)"
      tramp-default-method)))

(provide 'femacs-tramp)
;;; femacs-tramp.el ends here
