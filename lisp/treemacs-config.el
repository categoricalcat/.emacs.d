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
  (progn
    ;; Set the theme. 'treemacs-spacegrey-theme' is a nice choice.
    ;; You can also try 'treemacs-colors-alduin' or 'treemacs-classic-theme'.
    (treemacs-load-theme "treemacs-spacegrey")

    ;; --- Basic Customizations ---
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 'simple
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-find-workspace-method         'find-for-file-or-pick-first
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'ref-history-when-available
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-navigate
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.git")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphanumeric-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil))

:bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("<f8>"      . treemacs)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t p"   . treemacs-add-and-display-current-project)))

;; Integration with projectile
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(provide 'treemacs-config)
;;; treemacs-config.el ends here
