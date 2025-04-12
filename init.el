;;; init.el --- Emacs configuration
;;; Commentary: 福福的emacs配置文件
;;; Code:

(defvar emacs-config-dir
  "~/.emacs.d/"
  "Path to emacs config directory.")

;; Custom Reload Function
(defvar user-init-file (concat emacs-config-dir "init.el") "Path to user's init file.")

(defun load-user-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'load-user-init-file)

; ;; Package Management Setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(package-install 'gitconfig-mode)

(require 'gitconfig-mode)
(add-hook 'git-commit-mode-hook 'gitconfig-mode)



; load prelude init.el
; (load (concat emacs-config-dir "prelude/init.el"))


; ;; Basic UI Customizations
; (menu-bar-mode 0)
; (tool-bar-mode 0)
; (scroll-bar-mode 0)
; (column-number-mode 1)
; (global-display-line-numbers-mode 1)
; (display-time-mode 1)
; (setq inhibit-startup-message t)
; (global-visual-line-mode 1)
; (setq-default truncate-lines t)

; ;; Theme Setup
; (load-theme 'leuven-dark t)
; ;; Mode Line Customization
; (add-to-list 'default-frame-alist '(undecorated . t))
; (setq frame-title-format "%b")

; ;; Font and Line Spacing Configuration
; (set-face-attribute 'default nil
;                     :family "OpenDyslexicMono"
;                     :width 'ultra-condensed)

; (setq-default line-spacing 0.0)

; (custom-set-variables
;  '(custom-enabled-themes '(leuven-dark)))

; (custom-set-faces)


