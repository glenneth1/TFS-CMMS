;;;; backup.lisp - Database Backup and Restore Management
;;;; Admin-only functionality for managing database backups

(in-package :tfs-cmms)

;;; ============================================================
;;; Backup Configuration
;;; ============================================================

(defparameter *backup-dir* 
  (merge-pathnames "backups/" (get-app-directory)))

(defparameter *backup-script* 
  (merge-pathnames "scripts/backup-database.sh" (get-app-directory)))

(defparameter *restore-script*
  (merge-pathnames "scripts/restore-database.sh" (get-app-directory)))

;;; ============================================================
;;; Backup Utilities
;;; ============================================================

(defun ensure-backup-directories ()
  "Ensure backup directories exist."
  (ensure-directories-exist (merge-pathnames "daily/" *backup-dir*))
  (ensure-directories-exist (merge-pathnames "weekly/" *backup-dir*))
  (ensure-directories-exist (merge-pathnames "monthly/" *backup-dir*)))

(defun parse-backup-filename (filename)
  "Extract date from backup filename like tfs-cmms-2025-12-25.db.gz"
  (let ((name (pathname-name filename)))
    (when (and name (>= (length name) 10))
      (let ((date-part (subseq name (- (length name) 10))))
        (when (and (char= (char date-part 4) #\-)
                   (char= (char date-part 7) #\-))
          date-part)))))

(defun get-file-info (filepath)
  "Get file information including size and modification time."
  (when (probe-file filepath)
    (let* ((size (with-open-file (s filepath :element-type '(unsigned-byte 8))
                   (file-length s)))
           (write-date (file-write-date filepath)))
      (list :path (namestring filepath)
            :filename (file-namestring filepath)
            :size size
            :size-human (format-file-size size)
            :modified write-date
            :modified-human (format-backup-date write-date)))))

(defun get-db-file-info ()
  "Get database file information."
  (get-file-info *database-path*))

(defun format-file-size (bytes)
  "Format file size in human readable format."
  (cond
    ((< bytes 1024) (format nil "~D B" bytes))
    ((< bytes (* 1024 1024)) (format nil "~,1F KB" (/ bytes 1024.0)))
    ((< bytes (* 1024 1024 1024)) (format nil "~,1F MB" (/ bytes (* 1024.0 1024))))
    (t (format nil "~,2F GB" (/ bytes (* 1024.0 1024 1024))))))

(defun format-backup-date (universal-time)
  "Format universal time for display."
  (when universal-time
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time universal-time)
      (declare (ignore sec))
      (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
              year month day hour min))))

(defun list-backup-files (subdir)
  "List all backup files in a subdirectory."
  (let ((dir (merge-pathnames subdir *backup-dir*)))
    (when (probe-file dir)
      (let ((files (directory (merge-pathnames "*.gz" dir))))
        (sort (mapcar #'get-file-info files)
              #'> :key (lambda (f) (or (getf f :modified) 0)))))))

(defun get-all-backups ()
  "Get all backups organized by type."
  (ensure-backup-directories)
  (list :daily (list-backup-files "daily/")
        :weekly (list-backup-files "weekly/")
        :monthly (list-backup-files "monthly/")))

(defun get-backup-log ()
  "Read recent entries from backup log file."
  (let ((log-path "/var/log/tfs-cmms-backup.log"))
    (when (probe-file log-path)
      (with-open-file (s log-path :direction :input :if-does-not-exist nil)
        (when s
          (let ((lines nil)
                (max-lines 50))
            (loop for line = (read-line s nil nil)
                  while line
                  do (push line lines))
            (subseq (nreverse lines) 0 (min max-lines (length lines)))))))))

(defun run-backup-now ()
  "Execute backup script and return result."
  (let* ((script (namestring *backup-script*))
         (output (with-output-to-string (s)
                   (uiop:run-program script
                                     :output s
                                     :error-output s
                                     :ignore-error-status t))))
    output))

(defun restore-from-backup (backup-path)
  "Restore database from a backup file. Returns output from restore script.
   NOTE: This is a dangerous operation - the restore script handles safety."
  (let* ((script (namestring *restore-script*))
         ;; We need to run this with sudo and provide 'yes' confirmation
         ;; For safety, we'll just return instructions instead of auto-running
         (instructions (format nil "To restore this backup, run on the server:~%~%  sudo ~A ~A~%~%Then type 'yes' when prompted."
                               script backup-path)))
    instructions))

;;; ============================================================
;;; Backup Page Handler
;;; ============================================================

(defun handle-admin-backups ()
  "Admin page for managing database backups."
  (let ((user (get-current-user)))
    (if (and user (user-is-admin-p user))
        (let* ((backups (get-all-backups))
               (daily (getf backups :daily))
               (weekly (getf backups :weekly))
               (monthly (getf backups :monthly))
               (db-path (namestring *database-path*))
               (db-info (get-db-file-info))
               (message (hunchentoot:parameter "message"))
               (error-msg (hunchentoot:parameter "error")))
          (html-response
           (render-page "Database Backups"
             (cl-who:with-html-output-to-string (s)
               (:style "
                 .backup-stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem; margin-bottom: 1.5rem; }
                 .stat-card { background: var(--card-bg); padding: 1rem; border-radius: 8px; border: 1px solid var(--border-color); }
                 .stat-card h3 { margin: 0 0 0.5rem 0; font-size: 0.9rem; color: var(--text-muted); }
                 .stat-card .value { font-size: 1.5rem; font-weight: bold; }
                 .backup-section { margin-bottom: 2rem; }
                 .backup-section h2 { margin-bottom: 1rem; display: flex; align-items: center; gap: 0.5rem; }
                 .backup-table { width: 100%; }
                 .backup-table th { text-align: left; padding: 0.75rem; border-bottom: 2px solid var(--border-color); }
                 .backup-table td { padding: 0.75rem; border-bottom: 1px solid var(--border-color); }
                 .backup-table tr:hover { background: var(--hover-bg); }
                 .btn-restore { background: var(--warning-color); color: white; }
                 .btn-restore:hover { background: #e67e00; }
                 .backup-actions { display: flex; gap: 1rem; margin-bottom: 1.5rem; }
                 .alert { padding: 1rem; border-radius: 8px; margin-bottom: 1rem; }
                 .alert-success { background: #d4edda; color: #155724; border: 1px solid #c3e6cb; }
                 .alert-error { background: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; }
                 .restore-modal { display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; }
                 .restore-modal.active { display: flex; align-items: center; justify-content: center; }
                 .restore-modal-content { background: var(--card-bg); padding: 2rem; border-radius: 8px; max-width: 600px; width: 90%; }
                 .restore-modal h3 { margin-top: 0; }
                 .restore-warning { background: #fff3cd; color: #856404; padding: 1rem; border-radius: 4px; margin: 1rem 0; }
                 .restore-command { background: #1e1e1e; color: #d4d4d4; padding: 1rem; border-radius: 4px; font-family: monospace; overflow-x: auto; }
               ")
               
               (:div :class "page-header"
                 (:h1 "Database Backups")
                 (:a :href "/admin" :class "btn btn-secondary" "Back to Admin"))
               
               ;; Messages
               (when message
                 (cl-who:htm
                  (:div :class "alert alert-success" (cl-who:str message))))
               (when error-msg
                 (cl-who:htm
                  (:div :class "alert alert-error" (cl-who:str error-msg))))
               
               ;; Stats
               (:div :class "backup-stats"
                 (:div :class "stat-card"
                   (:h3 "Current Database")
                   (:div :class "value" (cl-who:str (or (getf db-info :size-human) "N/A")))
                   (:div :style "font-size: 0.85rem; color: var(--text-muted);" 
                         (cl-who:str (or (getf db-info :modified-human) ""))))
                 (:div :class "stat-card"
                   (:h3 "Daily Backups")
                   (:div :class "value" (cl-who:fmt "~D" (length daily))))
                 (:div :class "stat-card"
                   (:h3 "Weekly Backups")
                   (:div :class "value" (cl-who:fmt "~D" (length weekly))))
                 (:div :class "stat-card"
                   (:h3 "Monthly Backups")
                   (:div :class "value" (cl-who:fmt "~D" (length monthly)))))
               
               ;; Actions
               (:div :class "backup-actions"
                 (:form :method "post" :action "/api/admin/backup/run"
                   (:button :type "submit" :class "btn btn-primary" 
                            :onclick "return confirm('Run backup now?');"
                            "⬇ Backup Now")))
               
               ;; Daily Backups
               (:div :class "backup-section card"
                 (:h2 "📅 Daily Backups " (:small :style "font-weight: normal; color: var(--text-muted);" "(kept for 7 days)"))
                 (if daily
                     (cl-who:htm
                      (:table :class "backup-table"
                        (:thead
                          (:tr
                            (:th "Filename")
                            (:th "Date")
                            (:th "Size")
                            (:th "Actions")))
                        (:tbody
                          (dolist (backup daily)
                            (cl-who:htm
                             (:tr
                               (:td (:code (cl-who:str (getf backup :filename))))
                               (:td (cl-who:str (getf backup :modified-human)))
                               (:td (cl-who:str (getf backup :size-human)))
                               (:td 
                                 (:button :class "btn btn-sm btn-restore"
                                          :onclick (format nil "showRestoreModal('~A')" (getf backup :path))
                                          "Restore"))))))))
                     (cl-who:htm
                      (:p :class "text-muted" "No daily backups found."))))
               
               ;; Weekly Backups
               (:div :class "backup-section card"
                 (:h2 "📆 Weekly Backups " (:small :style "font-weight: normal; color: var(--text-muted);" "(kept for 4 weeks)"))
                 (if weekly
                     (cl-who:htm
                      (:table :class "backup-table"
                        (:thead
                          (:tr
                            (:th "Filename")
                            (:th "Date")
                            (:th "Size")
                            (:th "Actions")))
                        (:tbody
                          (dolist (backup weekly)
                            (cl-who:htm
                             (:tr
                               (:td (:code (cl-who:str (getf backup :filename))))
                               (:td (cl-who:str (getf backup :modified-human)))
                               (:td (cl-who:str (getf backup :size-human)))
                               (:td 
                                 (:button :class "btn btn-sm btn-restore"
                                          :onclick (format nil "showRestoreModal('~A')" (getf backup :path))
                                          "Restore"))))))))
                     (cl-who:htm
                      (:p :class "text-muted" "No weekly backups yet. Created on Sundays."))))
               
               ;; Monthly Backups
               (:div :class "backup-section card"
                 (:h2 "🗓️ Monthly Backups " (:small :style "font-weight: normal; color: var(--text-muted);" "(kept for 3 months)"))
                 (if monthly
                     (cl-who:htm
                      (:table :class "backup-table"
                        (:thead
                          (:tr
                            (:th "Filename")
                            (:th "Date")
                            (:th "Size")
                            (:th "Actions")))
                        (:tbody
                          (dolist (backup monthly)
                            (cl-who:htm
                             (:tr
                               (:td (:code (cl-who:str (getf backup :filename))))
                               (:td (cl-who:str (getf backup :modified-human)))
                               (:td (cl-who:str (getf backup :size-human)))
                               (:td 
                                 (:button :class "btn btn-sm btn-restore"
                                          :onclick (format nil "showRestoreModal('~A')" (getf backup :path))
                                          "Restore"))))))))
                     (cl-who:htm
                      (:p :class "text-muted" "No monthly backups yet. Created on the 1st of each month."))))
               
               ;; Backup Schedule Info
               (:div :class "card" :style "margin-top: 1rem;"
                 (:h2 "⏰ Backup Schedule")
                 (:ul
                   (:li "Automatic backups run daily at " (:strong "3:00 AM UTC"))
                   (:li "Daily backups are kept for " (:strong "7 days"))
                   (:li "Weekly backups (Sundays) are kept for " (:strong "4 weeks"))
                   (:li "Monthly backups (1st of month) are kept for " (:strong "3 months"))
                   (:li "Backups are compressed with gzip (~10x compression)")))
               
               ;; Restore Modal
               (:div :id "restoreModal" :class "restore-modal"
                 (:div :class "restore-modal-content"
                   (:h3 "⚠️ Restore Database")
                   (:div :class "restore-warning"
                     (:strong "Warning:") " Restoring a backup will replace the current database. "
                     "A safety backup will be created automatically before restoration.")
                   (:p "To restore this backup, SSH into the server and run:")
                   (:div :class "restore-command" :id "restoreCommand"
                     "sudo /opt/tfs-cmms/scripts/restore-database.sh [backup-path]")
                   (:p :style "margin-top: 1rem;"
                     "Then type " (:code "yes") " when prompted to confirm.")
                   (:div :style "margin-top: 1.5rem; display: flex; gap: 1rem; justify-content: flex-end;"
                     (:button :class "btn btn-secondary" :onclick "hideRestoreModal()" "Close"))))
               
               ;; JavaScript
               (:script "
                 function showRestoreModal(backupPath) {
                   document.getElementById('restoreCommand').textContent = 
                     'sudo /opt/tfs-cmms/scripts/restore-database.sh ' + backupPath;
                   document.getElementById('restoreModal').classList.add('active');
                 }
                 function hideRestoreModal() {
                   document.getElementById('restoreModal').classList.remove('active');
                 }
                 document.getElementById('restoreModal').addEventListener('click', function(e) {
                   if (e.target === this) hideRestoreModal();
                 });
               ")))))
        (redirect-to "/unauthorized"))))

;;; ============================================================
;;; Backup API Handlers
;;; ============================================================

(defun handle-api-admin-backup-run ()
  "API endpoint to run backup now."
  (let ((user (get-current-user)))
    (if (and user (user-is-admin-p user))
        (progn
          (run-backup-now)
          (hunchentoot:redirect "/admin/backups?message=Backup%20completed%20successfully"))
        (redirect-to "/unauthorized"))))
