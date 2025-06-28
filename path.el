;;; path.el --- vars for paths

;;; Commentary:
;;; bruh

;;; Code:
(defvar dir-home
  (or (getenv "HOME") "~")
  "Home.")

;;; Sure
(defun path (dir file)
  "Nice DIR and FILE function."
  (expand-file-name file (or dir dir-home)))

(defvar dir-emacs
  (path dir-home ".emacs.d")
  "Emacs config directory.")

(provide 'path)

;;; path.el ends here
