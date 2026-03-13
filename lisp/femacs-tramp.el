;;; femacs-tramp.el --- TRAMP -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Enable TRAMP
(require 'tramp)

;; Set default SSH method (recommended)
(setq tramp-default-method "sshx")

;; Explicitly disable ControlMaster for SSH connections
(setq tramp-use-ssh-controlmaster-options nil)

(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
(add-to-list 'tramp-remote-path "/run/wrappers/bin")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(use-package magit
  :config
  (setq magit-git-executable "git"))

  (setq tramp-remote-process-environment
        (append '("NIX_PROFILES=/nix/var/nix/profiles/default /run/current-system/sw")
                tramp-remote-process-environment))

(setq tramp-verbose 10
  magit-debug t
  password-cache-expiry 3600)

;; This message is helpful for confirming your setup.
(add-hook 'after-init-hook
  (lambda ()
    (message "TRAMP method: %s, ControlMaster: disabled"
      tramp-default-method)))

(provide 'femacs-tramp)
;;; femacs-tramp.el ends here
