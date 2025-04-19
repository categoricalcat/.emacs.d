(defun fufu-make-link (target linkname description)
  "Create symlink LINKNAME pointing to TARGET, ensuring target exists and handling existing LINKNAME."
  (condition-case err
      (progn
        (unless (file-exists-p target)
          (error "Source '%s' not found for '%s'" target description))
        (let ((link-dir (file-name-directory linkname)))
          (unless (file-directory-p link-dir) (make-directory link-dir t)))
        (when (file-symlink-p linkname) (delete-file linkname))
        (if (file-exists-p linkname)
            (message "Skipping: '%s' exists (not a symlink) for '%s'" linkname description)
          (make-symbolic-link target linkname)
          (message "Linked (%s): %s -> %s" description linkname target)))
    (error (message "Failed link for '%s': %s" description err))))

(defun fufu-setup-links-from-csv (csv-file)
  "Process CSV-FILE to create symlinks defined within."
  (let ((dir-emacs (getenv "DIR_EMACS")))
    (unless dir-emacs (error "$DIR_EMACS environment variable not set"))
    (setenv "DIR_EMACS" dir-emacs) ;; Ensure available for substitute-in-file-name

    (unless (file-readable-p csv-file) (error "Cannot read CSV file: %s" csv-file))

    (with-temp-buffer
      (insert-file-contents csv-file)
      (dolist (line (cdr (split-string (buffer-string) "\n" t 'trim))) ;; Skip header row
        (let* ((fields (split-string line ","))
               (source-str (and (> (length fields) 1) (string-trim (nth 1 fields))))
               (target-str (and (> (length fields) 2) (string-trim (nth 2 fields))))
               (desc (and (> (length fields) 3)
                          (string-remove-suffix "\"" (string-remove-prefix "\"" (string-trim (nth 3 fields)))))))

          (when (and source-str target-str desc) ;; Check for valid row data
            (let ((src-pattern (expand-file-name (substitute-in-file-name source-str))))
              (if (string-match-p "\\*" src-pattern) ;; Handle wildcard source
                  (let* ((target-dir (expand-file-name (substitute-in-file-name (file-name-directory target-str))))
                         (sources (file-expand-wildcards src-pattern)))
                    (unless (file-directory-p target-dir) (make-directory target-dir t))
                    (dolist (src sources)
                      (when (file-regular-p src)
                        (let ((link (expand-file-name (file-name-nondirectory src) target-dir)))
                          (fufu-make-link src link (format "%s (%s)" desc (file-name-nondirectory src)))))))
                ;; Handle single file source
                (let ((link (expand-file-name (substitute-in-file-name target-str))))
                  (fufu-make-link src-pattern link desc))))))))))

;; --- How to Use ---
;; 1. Ensure $DIR_EMACS environment variable is set before starting Emacs.
;; 2. Save this code to a file, e.g., ~/setup-links.el
;; 3. Load the file in Emacs: M-x load-file RET ~/setup-links.el RET
;; 4. Run the function: M-x fufu-setup-links-from-csv RET /path/to/your/data.csv RET
;;    (Replace /path/to/your/data.csv with the actual path)

;; Example Call:
;; (fufu-setup-links-from-csv "/home/fufu/my-configs/symlinks.csv")
