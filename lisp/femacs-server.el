;;; femacs-server.el --- Network-accessible Emacs server -*- lexical-binding: t; -*-

;;; Commentary:
;; Starts a TCP server on all interfaces so a second machine on the LAN
;; can connect with `emacsclient -f <server-auth-file> -c`.
;;
;; Workflow (two machines on the same network):
;;
;;   SERVER (machine A):
;;     emacs --daemon          ; starts the TCP server on 0.0.0.0:13245
;;
;;   CLIENT (machine B):
;;     1. Copy the server auth file from machine A:
;;        scp A:~/.emacs.d/server/femacs ~/.emacs.d/server/femacs
;;     2. Connect:
;;        emacsclient -f ~/.emacs.d/server/femacs -c
;;
;;   Or use the SSH tunnel approach (see connect-remote-emacs.sh):
;;     ssh -N -L 13245:localhost:13245 user@machine-A &
;;     emacsclient -f ~/.emacs.d/server/femacs -c

;;; Code:

(require 'server)

(defvar femacs-server-port 13245
  "TCP port for the Emacs server.  Override via EMACS_SERVER_PORT env var.")

(when-let ((env-port (getenv "EMACS_SERVER_PORT")))
  (setq femacs-server-port (string-to-number env-port)))

(unless (server-running-p)
  (setq server-host "0.0.0.0"
        server-port femacs-server-port
        server-use-tcp t
        server-name "femacs"
        server-auth-dir (expand-file-name "server" user-emacs-directory)
        server-auth-key (server-generate-key))

  (condition-case err
      (progn
        (server-start)
        (message "Emacs server started on %s:%s" server-host server-port))
    (error (message "Failed to start Emacs server: %s" err))))

(provide 'femacs-server)
;;; femacs-server.el ends here
