;;; femacs-shell.el --- Shell selection for Eshell/EAT -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal shell config: Windows -> PowerShell, Others -> zsh.

;;; Code:

;;;###autoload
(defun femacs-shell-setup ()
  "Setup EAT to use PowerShell on Windows and zsh elsewhere."
  (let ((shell (if (eq system-type 'windows-nt) "powershell.exe" "zsh")))
    (with-eval-after-load 'eat
      (dolist (var '(eat-shell
                     eat-default-shell
                     eat-shell-command
                     eat-default-shell-command))
        (when (boundp var)
          (set var shell))))))

;; Apply immediately on load.
(femacs-shell-setup)

(provide 'femacs-shell)
;;; femacs-shell.el ends here
