(defvar dir-home
  (or (getenv "HOME") "~")
  "home.")

(defun path (dir file)
  (expand-file-name file (or dir dir-home)))

(defvar dir-emacs
  (path dir-home ".emacs.d")
  "emacs config directory.")
