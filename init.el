;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; 福福的emacs配置文件

;;; Code:

; run git submodule update --init --recursive
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

;; windows size
(setq initial-frame-alist '((width . 130) (height . 40)))
(setq default-frame-alist '((width . 130) (height . 30)))
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

;; Theme Setup
(load-theme 'leuven-dark t)

;; Font and Line Spacing Configuration
(set-face-attribute 'default nil
                    :family "Maple Mono NF CN"
                    :width 'ultra-condensed)

(custom-set-variables
 '(custom-enabled-themes '(leuven-dark)))

(rc/require 'elcord)
(elcord-mode)

(desktop-save-mode 1)
(super-save-mode +1)

(rc/require 'golden-ratio)
(golden-ratio-mode 1)

(server-start)

;; UI
; (menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(display-time-mode 1)
(global-visual-line-mode 1)
(setq-default truncate-lines t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice (lambda () (get-buffer-create "eshell")))
(setq initial-major-mode 'eshell-mode)
; (setq-default major-mode 'eshell-mode)

; ;; Mode Line Customization
; (add-to-list 'default-frame-alist '(undecorated . t))
; (setq frame-title-format "%b")


; (custom-set-faces)


; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(package-selected-packages '(gitconfig-mode git-modes)))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
                                        ;  )
;;; init.el ends here
