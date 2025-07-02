;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; 福福的emacs配置文件

;;; Code:

;; git submodule update --init --recursive
(shell-command "git submodule update --init --recursive")

;; Load Prelude :3
(load (expand-file-name "prelude/init.el" user-emacs-directory))

;; Add the lisp directory to the load path
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

;; Them requires
(require 'server)
(require 'rc)
(require 'path)
(require 'treemacs-config)
(require 'desktop)

;; windows size
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq server-port 13245)
(setq server-use-tcp t)
(setq server-host "0.0.0.0")
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

(custom-set-variables
 '(custom-enabled-themes '(leuven-dark)))

(rc/require 'elcord)
(elcord-mode)

(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs-desktop"
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)
(super-save-mode +1)

(rc/require 'golden-ratio)
(golden-ratio-mode 1)

(server-start)

;; UI
(custom-set-faces)
(load-theme 'leuven-dark t)
(set-face-attribute 'default nil
                    :family "Maple Mono NF CN"
                    :width 'ultra-condensed)
; (menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(display-time-mode 1)
(global-visual-line-mode 1)
(setq-default truncate-lines t)
(setq inhibit-startup-message t)
;; (setq initial-major-mode 'eshell-mode)
;; (setq initial-buffer-choice (lambda () (get-buffer-create "eshell")))
;; (setq-default major-mode 'eshell-mode)

; ;; Mode Line Customization
; (add-to-list 'default-frame-alist '(undecorated . t))
(setq frame-title-format "%m 🩵🩷🤍 %b")

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Ensure we're starting from a single window
            (delete-other-windows)
            (treemacs)
            (other-window 1)
            (split-window-below)
            ))

;;; init.el ends here
