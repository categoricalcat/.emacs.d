;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; 福福的emacs配置文件

;;; Code:

;; git submodule update --init --recursive
;; (shell-command "git submodule update --init --recursive")

(set-language-environment "UTF-8")

;; Create necessary directories
(defun femacs/ensure-directories ()
  "Ensure necessary directories exist."
  (let ((dirs (list
               (expand-file-name "desktop" user-emacs-directory)
               (expand-file-name "cache" user-emacs-directory)
               (expand-file-name "server" user-emacs-directory))))
    (dolist (dir dirs)
      (unless (file-exists-p dir)
        (make-directory dir t)
        (message "Created directory: %s" dir)))))

(femacs/ensure-directories)

;; Load Prelude :3
(load (expand-file-name "prelude/init.el" user-emacs-directory))

;; Add the lisp directory to the load path
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

;; Them requires
(require 'rc)
(require 'path)
(require 'femacs-shell)
(rc/require 'eat)
(rc/require 'yasnippet)

;; Load optional modules with error handling
(condition-case err
    (require 'treemacs-config)
  (error (message "Failed to load treemacs-config: %s" err)))

(require 'desktop)

(condition-case err
    (require 'femacs-server)
  (error (message "Failed to load femacs-server: %s" err)))

(condition-case err
    (require 'femacs-tramp)
  (error (message "Failed to load femacs-tramp: %s" err)))

;; windows size
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default line-spacing 0.0)

;; Definitions
(defvar dir-prelude
  (path-resolve path-emacs "prelude")
  "Prelude directory.")

(defvar this-file
  (path-resolve path-emacs "init.el")
  "This file.")

(defun load-file-user-init ()
  "Reload this Emacs initialization file."
  (interactive)
  (load-file this-file))

(global-set-key (kbd "C-c r") 'load-file-user-init)

(rc/require 'modus-themes)

;; Your customizations here:
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

(modus-themes-load-theme 'modus-vivendi)

;; (rc/require 'kaolin-themes)

;; Discord RPC (only in graphics mode)
(when (display-graphic-p)
  (condition-case err
      (progn
        (rc/require 'elcord)
        (elcord-mode))
    (error (message "Failed to load elcord: %s" err))))

;; (rc/require 'vterm)

;; Desktop save mode configuration
(setq desktop-dirname             (expand-file-name "desktop" user-emacs-directory)
      desktop-base-file-name      "emacs-desktop"
      desktop-load-locked-desktop nil
      desktop-restore-frames nil)

(condition-case err
    (desktop-save-mode 1)
  (error (message "Failed to enable desktop-save-mode: %s" err)))

;; Super save mode
(condition-case err
    (progn
      (rc/require 'super-save)
      (super-save-mode +1))
  (error (message "Failed to load super-save: %s" err)))

;; Golden ratio mode
(condition-case err
    (progn
      (rc/require 'golden-ratio)
      (golden-ratio-mode 1))
  (error (message "Failed to load golden-ratio: %s" err)))

;; UI Configuration
(custom-set-faces)

;; Font configuration (only in graphics mode)
(when (display-graphic-p)
  (condition-case err
      (set-face-attribute 'default nil
                          :family "Maple Mono NF CN"
                          :width 'ultra-condensed)
    (error (message "Failed to set font: %s" err))))

;; UI elements
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (and (display-graphic-p) (fboundp 'scroll-bar-mode))
  (scroll-bar-mode 0))

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(display-time-mode 1)
(global-visual-line-mode 1)
(setq-default truncate-lines t)
(setq inhibit-startup-message t)
;; (setq initial-major-mode 'eshell-mode)
;; (setq initial-buffer-choice (lambda () (get-buffer-create "eshell")))
;; (setq-default major-mode 'eshell-mode)

;; Mode Line Customization
;; (when (display-graphic-p)
;;   (add-to-list 'default-frame-alist '(undecorated . t)))
(setq frame-title-format "%m 🩵🩷🤍 %b")

(defun femacs/setup-initial-layout ()
  (when (and (null command-line-args-left)
             (null buffer-file-name)
             (string= (buffer-name) "*scratch*"))
    (delete-other-windows)
    (treemacs)
    (other-window 1)
    (split-window-below)))
;; (add-hook 'emacs-startup-hook #'femacs/setup-initial-layout)

;; Nix language support: syntax highlighting and LSP (nil/nixd)
(rc/require 'nix-mode)
(rc/require 'lsp-mode)

;; File associations
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.nixinc\\'" . nix-mode))

;; Enable LSP and format-on-save for Nix
(with-eval-after-load 'nix-mode
  (when (fboundp 'lsp-deferred)
    (add-hook 'nix-mode-hook #'lsp-deferred)
    (add-hook 'nix-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'lsp-format-buffer nil t)))))

;;; init.el ends here
