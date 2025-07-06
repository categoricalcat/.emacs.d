;; ~/.emacs.d/init.el on 192.168.31.21 (remote server)

;; ==============================================
;; Emacs Server Configuration for SSH Tunneling
;; ==============================================

;;; Code:
(require 'server)

;; 1. Bind to localhost only (security critical!)
(setq server-host "127.0.0.1")  ; Not 0.0.0.0!

;; 2. Choose a fixed port (or use 0 for random)
(setq server-port 13245)        ; Recommend 5000-65535

;; 3. Use TCP protocol (required for tunneling)
(setq server-use-tcp t)

;; 4. Optional: Set server name for identification
(setq server-name "remote-emacs-server")

;; 5. Optional: Persistent server (survives frame closes)
(setq server-mode t)

;; 6. Start server if not already running
(unless (server-running-p)
  (server-start))

;; 7. Security hardening (recommended)
(setq server-auth-dir (expand-file-name "~/.emacs.d/server/"))
(setq server-auth-key (secure-hash 'sha1 (current-time-string)))

(provide 'femacs-server)
;;; server.el ends here
