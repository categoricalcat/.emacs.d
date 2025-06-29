;;; path.el --- vars for paths -*- lexical-binding: t; -*-

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

(defconst path-lisp
  (path-resolve path-emacs "lisp")
  "Emacs lisp directory.")

(provide 'path)

;;; path.el ends here
