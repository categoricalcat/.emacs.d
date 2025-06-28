;;; path.el --- vars for paths

;;; Commentary:
;;; Helpers for paths

;;; Code:
(defconst path-home
  (or (getenv "HOME") "~")
  "Home.")

(defun path-resolve (dir file)
  "DIR and FILE function."
  (expand-file-name file (or dir path-home)))

(defconst path-emacs
  (path-resolve path-home ".emacs.d")
  "Emacs config directory.")

(provide 'path)

;;; path.el ends here
