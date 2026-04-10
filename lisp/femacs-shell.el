;;; femacs-shell.el --- Shell / terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure the EAT terminal emulator: PowerShell on Windows, zsh elsewhere.

;;; Code:

(with-eval-after-load 'eat
  (let ((shell (if (eq system-type 'windows-nt) "powershell.exe" "zsh")))
    (dolist (var '(eat-shell
                   eat-default-shell
                   eat-shell-command
                   eat-default-shell-command))
      (when (boundp var)
        (set var shell)))))

(provide 'femacs-shell)
;;; femacs-shell.el ends here
