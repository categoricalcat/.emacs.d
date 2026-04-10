;;; femacs-tramp.el --- TRAMP & Magit configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Remote editing via TRAMP, with NixOS remote path support and Magit
;; integration.  Debug logging is intentionally kept at maximum verbosity.

;;; Code:

(require 'tramp)

(setq tramp-default-method "sshx")
(setq tramp-use-ssh-controlmaster-options nil)

(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
(add-to-list 'tramp-remote-path "/run/wrappers/bin")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(setq tramp-remote-process-environment
      (append '("NIX_PROFILES=/nix/var/nix/profiles/default /run/current-system/sw")
              tramp-remote-process-environment))

(setq tramp-verbose 10
      password-cache-expiry 3600)

;; Disable diff-hl for remote buffers (Prelude enables it globally)
(with-eval-after-load 'diff-hl
  (add-hook 'diff-hl-mode-hook
            (lambda ()
              (when (file-remote-p (or buffer-file-name default-directory ""))
                (diff-hl-mode -1))))
  (add-hook 'dired-mode-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (diff-hl-dired-mode -1)))))

;; Disable vc over TRAMP entirely
(setq vc-ignore-dir-regexp
      (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

;; Disable treemacs git-mode for remote projects
(with-eval-after-load 'treemacs
  (treemacs-git-mode -1))

;; Magit
(use-package magit
  :config
  (setq magit-git-executable "git"
        magit-debug t))

(provide 'femacs-tramp)
;;; femacs-tramp.el ends here
