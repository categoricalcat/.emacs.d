;;; treemacs-config.el --- A minimal configuration for Treemacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a basic setup for the Treemacs file explorer.
;; For a full list of options and features, you can check out the official
;; documentation: https://github.com/Alexander-Miller/treemacs

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  ;; Set the theme. 'treemacs-spacegrey-theme' is a nice choice.
  ;; You can also try 'treemacs-colors-alduin' or 'treemacs-classic-theme'.
  (treemacs-load-theme "treemacs-spacegrey")

  ;; --- Basic Customizations ---
  ;; For a minimal setup, we only set values that differ from the defaults.
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0))
  (setq treemacs-expand-after-init t)
  (setq treemacs-eldoc-display 'simple)
  (setq treemacs-no-delete-other-windows t)
  (setq treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (setq treemacs-read-string-input 'from-child-frame)
  (setq treemacs-litter-directories '("/node_modules" "/.venv" "/.git" "/.cask"))
  (setq treemacs-sorting 'alphabetic-asc)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

:bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("<f8>"      . treemacs)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t p"   . treemacs-add-and-display-current-project)))

;; Integration with projectile
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(provide 'treemacs-config)
;;; treemacs-config.el ends here
