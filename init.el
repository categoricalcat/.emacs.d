;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; 福福的emacs配置文件

;;; Code:

(set-language-environment "UTF-8")

(dolist (dir '("desktop" "cache" "server"))
  (let ((path (expand-file-name dir user-emacs-directory)))
    (unless (file-exists-p path)
      (make-directory path t))))

;; ---------------------------------------------------------------------------
;; Prelude
;; ---------------------------------------------------------------------------
(load (expand-file-name "prelude/init.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(prelude-require-packages
 '(eat
   yasnippet
   drag-stuff
   nix-mode
   lsp-mode
   super-save
   golden-ratio
   elcord))

;; ---------------------------------------------------------------------------
;; Personal modules
;; ---------------------------------------------------------------------------
(require 'path)
(require 'femacs-tramp)
(require 'femacs-shell)
(require 'femacs-server)
(require 'treemacs-config)

;; ---------------------------------------------------------------------------
;; Theme
;; ---------------------------------------------------------------------------
(load-theme 'modus-vivendi t)

(setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
      modus-themes-to-rotate modus-themes-items
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui t
      modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-completions '((t . (bold)))
      modus-themes-prompts '(bold)
      modus-themes-headings
      '((agenda-structure . (variable-pitch light 2.2))
        (agenda-date . (variable-pitch regular 1.3))
        (t . (regular 1.15))))

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------
(when (display-graphic-p)
  (condition-case err
      (set-face-attribute 'default nil
                          :family "Maple Mono NF CN"
                          :width 'ultra-condensed)
    (error (message "Failed to set font: %s" err))))

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(display-time-mode 1)
(global-visual-line-mode 1)
(setq-default truncate-lines t)
(setq inhibit-startup-message t)
(setq frame-title-format "%m 🩵🩷🤍 %b")

;; Discord RPC (only in graphics mode)
(when (display-graphic-p)
  (condition-case err
      (elcord-mode)
    (error (message "Failed to load elcord: %s" err))))

;; ---------------------------------------------------------------------------
;; Desktop save
;; ---------------------------------------------------------------------------
(require 'desktop)

(setq desktop-dirname             (expand-file-name "desktop" user-emacs-directory)
      desktop-base-file-name      "emacs-desktop"
      desktop-load-locked-desktop t
      desktop-auto-save-timeout   300
      desktop-save                t
      desktop-restore-frames      nil)

(desktop-save-mode 1)

;; ---------------------------------------------------------------------------
;; Modes
;; ---------------------------------------------------------------------------
(super-save-mode +1)

(condition-case err
    (golden-ratio-mode 1)
  (error (message "Failed to load golden-ratio: %s" err)))

;; ---------------------------------------------------------------------------
;; Nix
;; ---------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.nixinc\\'" . nix-mode))

(with-eval-after-load 'nix-mode
  (when (fboundp 'lsp-deferred)
    (add-hook 'nix-mode-hook #'lsp-deferred)
    (add-hook 'nix-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'lsp-format-buffer nil t)))))

;; ---------------------------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-c r")
                (lambda () (interactive) (load-file user-init-file)))

;; ---------------------------------------------------------------------------
;; Post-init
;; ---------------------------------------------------------------------------
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1024 1024))))

;;; init.el ends here
