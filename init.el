;;; init.el --- Emacs configuration
;;; Commentary: 福福的emacs配置文件
;;; Code:

; run git submodule update --init --recursive
(shell-command "git submodule update --init --recursive")

(load (expand-file-name "prelude/init.el"))
(load (expand-file-name "path.el"))
(load (expand-file-name "rc.el"))

(defvar dir-prelude
  (path dir-emacs "prelude")
  "prelude directory.")

(defvar this-file
  (path dir-emacs "init.el")
  "this file.")

(defun load-file-user-init ()
  (interactive)
  (load-file this-file))

(global-set-key (kbd "C-c r") 'load-file-user-init)







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
