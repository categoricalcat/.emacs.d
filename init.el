;;; init.el --- Emacs configuration
;;; Commentary: 福福的emacs配置文件
;;; Code:

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(display-time-mode 1)
(setq inhibit-startup-message t)

(require 'package)

; make variable for user init file location
(defvar user-init-file "~/.emacs.d/init.el")

; make function load-user-init-file that runs (load-file user-init-file)
(defun load-user-init-file ()
  (interactive)
  (load-file user-init-file))


(global-set-key (kbd "C-c r") 'load-user-init-file)

; bind for alt + x + e + b to evaluate the current buffer
(global-set-key (kbd "M-x e b") 'eval-buffer)





