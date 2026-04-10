;;; treemacs-config.el --- Treemacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; File explorer with projectile, magit, and TRAMP integration.
;; https://github.com/Alexander-Miller/treemacs

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-load-theme "Default")

  (setq treemacs-collapse-dirs              (if treemacs-python-executable 3 0)
        treemacs-expand-after-init          t
        treemacs-eldoc-display              'simple
        treemacs-no-delete-other-windows    t
        treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-read-string-input          'from-child-frame
        treemacs-litter-directories         '("/node_modules" "/.venv" "/.git" "/.cask")
        treemacs-sorting                    'alphabetic-asc)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("<f8>"      . treemacs)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t p"   . treemacs-add-and-display-current-project)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(provide 'treemacs-config)
;;; treemacs-config.el ends here
