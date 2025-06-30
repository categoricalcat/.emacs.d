;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; 福福的emacs配置文件

;;; Code:

; run git submodule update --init --recursive
(shell-command "git submodule update --init --recursive")

(load (expand-file-name "prelude/init.el" user-emacs-directory))

(require 'server)
(setq server-host "0.0.0.0")
(setq server-port 13245)
(setq server-use-tcp t)
(server-start)

;; Add custom lisp path at compile and run time
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

;; Load custom path helpers
(require 'path)

(load (expand-file-name "rc.el" path-lisp))

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

;; prelude

;; Theme Setup
(load-theme 'leuven-dark t)

;; Font and Line Spacing Configuration
(set-face-attribute 'default nil
                    :family "Maple Mono NF CN"
                    :width 'ultra-condensed)

(setq-default line-spacing 0.0)

(custom-set-variables
 '(custom-enabled-themes '(leuven-dark)))

;; windows size
(setq initial-frame-alist '((width . 130) (height . 40)))
(setq default-frame-alist '((width . 130) (height . 30)))

;; elcord mode
(rc/require 'elcord)
(elcord-mode)

;; super save mode
(desktop-save-mode 1)
(super-save-mode +1)


; ;; Basic UI Customizations
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
