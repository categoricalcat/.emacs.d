;;; femacs-tramp.el --- TRAMP -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Enable TRAMP
(require 'tramp)

;; Set default SSH method (recommended)
(setq tramp-default-method "ssh")

;; Optional: Reuse SSH connections for speed (uses ControlMaster)
(setq tramp-use-ssh-controlmaster-options t)

(setq tramp-default-user "fufu")

(setq password-cache-expiry 3600)

(provide 'femacs-tramp)
;;; femacs-tramp.el ends here
