;;; femacs-server.el --- Server -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'server)

;; Only configure and start server if not already running
(unless (server-running-p)
  (setq server-host "127.0.0.1")  ; Not 0.0.0.0!

  ;; 2. Choose a fixed port (or use 0 for random)
  (setq server-port 13245)        ; Recommend 5000-65535

  ;; 3. Use TCP protocol (required for tunneling)
  (setq server-use-tcp t)

  ;; 4. Optional: Set server name for identification
  (setq server-name "remote-emacs-server")

  ;; 5. Optional: Persistent server (survives frame closes)
  ;; (setq server-mode t)

  ;; 6. Security hardening (recommended)
  (setq server-auth-dir (expand-file-name "~/.emacs.d/server/"))
  (setq generated-key (server-generate-key))
  (setq server-auth-key (secure-hash 'sha256 (current-time-string)))

  ;; 7. Start server
  ;; (server-start)

  (message "Emacs server started on %s:%s" server-host server-port))

(provide 'femacs-server)
;;; femacs-server.el ends here
