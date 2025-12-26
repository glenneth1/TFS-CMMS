;;;; system-monitor.lisp - System Resource Monitoring for Admin
;;;; Provides CPU, memory, disk, network, and process monitoring

(in-package :tfs-cmms)

;;; ============================================================
;;; System Information Utilities
;;; ============================================================

(defun run-shell-command (command)
  "Run a shell command and return output as string."
  (handler-case
      (with-output-to-string (s)
        (uiop:run-program command
                          :output s
                          :error-output nil
                          :ignore-error-status t))
    (error () "")))

(defun parse-proc-file (path)
  "Read a /proc file and return contents as string."
  (handler-case
      (with-open-file (s path :direction :input :if-does-not-exist nil)
        (when s
          (with-output-to-string (out)
            (loop for line = (read-line s nil nil)
                  while line
                  do (write-line line out)))))
    (error () "")))

;;; ============================================================
;;; CPU Information
;;; ============================================================

(defun get-cpu-info ()
  "Get CPU usage information."
  (let* ((stat-output (run-shell-command "cat /proc/stat | head -1"))
         (loadavg (run-shell-command "cat /proc/loadavg"))
         (cpu-count (parse-integer 
                     (string-trim '(#\Space #\Newline) 
                                  (run-shell-command "nproc")) 
                     :junk-allowed t)))
    ;; Parse load averages
    (let* ((loads (cl-ppcre:split "\\s+" (string-trim '(#\Space #\Newline) loadavg)))
           (load-1 (when (> (length loads) 0) (first loads)))
           (load-5 (when (> (length loads) 1) (second loads)))
           (load-15 (when (> (length loads) 2) (third loads))))
      (list :cpu-count (or cpu-count 1)
            :load-1 (or load-1 "0")
            :load-5 (or load-5 "0")
            :load-15 (or load-15 "0")
            :load-percent (when (and load-1 cpu-count)
                           (let ((load (parse-number load-1)))
                             (when load
                               (min 100 (round (* 100 (/ load cpu-count)))))))))))

(defun parse-number (str)
  "Parse a number from string, returning nil on failure."
  (handler-case
      (parse-float:parse-float str :junk-allowed t)
    (error () nil)))

;;; ============================================================
;;; Memory Information
;;; ============================================================

(defun get-memory-info ()
  "Get memory usage information from /proc/meminfo."
  (let* ((meminfo (run-shell-command "cat /proc/meminfo"))
         (lines (cl-ppcre:split "\\n" meminfo))
         (mem-total 0)
         (mem-free 0)
         (mem-available 0)
         (mem-buffers 0)
         (mem-cached 0)
         (swap-total 0)
         (swap-free 0))
    (dolist (line lines)
      (let ((parts (cl-ppcre:split ":\\s+" line)))
        (when (= (length parts) 2)
          (let* ((key (first parts))
                 (value-str (cl-ppcre:regex-replace "\\s*kB.*" (second parts) ""))
                 (value (parse-integer value-str :junk-allowed t)))
            (when value
              (cond
                ((string= key "MemTotal") (setf mem-total value))
                ((string= key "MemFree") (setf mem-free value))
                ((string= key "MemAvailable") (setf mem-available value))
                ((string= key "Buffers") (setf mem-buffers value))
                ((string= key "Cached") (setf mem-cached value))
                ((string= key "SwapTotal") (setf swap-total value))
                ((string= key "SwapFree") (setf swap-free value))))))))
    (let* ((mem-used (- mem-total mem-available))
           (swap-used (- swap-total swap-free)))
      (list :total (format-kb mem-total)
            :used (format-kb mem-used)
            :free (format-kb mem-free)
            :available (format-kb mem-available)
            :buffers (format-kb mem-buffers)
            :cached (format-kb mem-cached)
            :swap-total (format-kb swap-total)
            :swap-used (format-kb swap-used)
            :swap-free (format-kb swap-free)
            :percent-used (if (> mem-total 0)
                             (round (* 100 (/ mem-used mem-total)))
                             0)
            :swap-percent (if (> swap-total 0)
                             (round (* 100 (/ swap-used swap-total)))
                             0)))))

(defun format-kb (kb)
  "Format kilobytes to human readable."
  (cond
    ((< kb 1024) (format nil "~D KB" kb))
    ((< kb (* 1024 1024)) (format nil "~,1F MB" (/ kb 1024.0)))
    (t (format nil "~,2F GB" (/ kb (* 1024.0 1024))))))

(defun format-bytes (bytes)
  "Format bytes to human readable."
  (cond
    ((< bytes 1024) (format nil "~D B" bytes))
    ((< bytes (* 1024 1024)) (format nil "~,1F KB" (/ bytes 1024.0)))
    ((< bytes (* 1024 1024 1024)) (format nil "~,1F MB" (/ bytes (* 1024.0 1024))))
    (t (format nil "~,2F GB" (/ bytes (* 1024.0 1024 1024))))))

;;; ============================================================
;;; Disk Information
;;; ============================================================

(defun get-disk-info ()
  "Get disk usage information."
  (let* ((df-output (run-shell-command "df -h --output=source,size,used,avail,pcent,target 2>/dev/null | grep -E '^/dev/'"))
         (lines (cl-ppcre:split "\\n" (string-trim '(#\Space #\Newline) df-output)))
         (disks nil))
    (dolist (line lines)
      (let ((parts (cl-ppcre:split "\\s+" (string-trim '(#\Space) line))))
        (when (>= (length parts) 6)
          (push (list :device (first parts)
                      :size (second parts)
                      :used (third parts)
                      :available (fourth parts)
                      :percent (parse-integer (cl-ppcre:regex-replace "%" (fifth parts) "") :junk-allowed t)
                      :mount (sixth parts))
                disks))))
    (nreverse disks)))

;;; ============================================================
;;; Network Information
;;; ============================================================

(defun interface-matches-p (iface prefixes)
  "Check if interface name matches any of the prefixes."
  (loop for prefix in prefixes
        thereis (or (string= iface prefix)
                    (and (> (length iface) (length prefix))
                         (string= (subseq iface 0 (length prefix)) prefix)))))

(defun get-network-info ()
  "Get network interface statistics for main interfaces only."
  (let* ((output (run-shell-command "cat /proc/net/dev"))
         (lines (cl-ppcre:split "\\n" output))
         (interfaces nil)
         ;; Only show these interfaces (skip docker/veth noise)
         (show-interfaces '("eth0" "ens" "enp" "wlan" "tailscale0")))
    (dolist (line (cddr lines)) ; Skip header lines
      (when (search ":" line)
        (let* ((parts (cl-ppcre:split ":\\s*" line))
               (iface (string-trim '(#\Space) (first parts))))
          ;; Only include main interfaces
          (when (and parts (interface-matches-p iface show-interfaces))
            (let* ((stats (cl-ppcre:split "\\s+" (string-trim '(#\Space) (second parts))))
                   (rx-bytes (parse-integer (or (first stats) "0") :junk-allowed t))
                   (tx-bytes (parse-integer (or (ninth stats) "0") :junk-allowed t)))
              (push (list :interface iface
                          :rx-bytes (or rx-bytes 0)
                          :tx-bytes (or tx-bytes 0)
                          :rx-human (format-bytes (or rx-bytes 0))
                          :tx-human (format-bytes (or tx-bytes 0)))
                    interfaces))))))
    (nreverse interfaces)))

;;; ============================================================
;;; Application Directory Sizes
;;; ============================================================

(defun get-directory-size (path)
  "Get size of a directory in bytes using du."
  (let* ((output (run-shell-command (format nil "du -sb ~A 2>/dev/null | cut -f1" path)))
         (size-str (string-trim '(#\Space #\Newline) output)))
    (or (parse-integer size-str :junk-allowed t) 0)))

(defun get-app-storage-info ()
  "Get storage information for application directories."
  (let* ((app-dir (namestring (get-app-directory)))
         (data-dir (merge-pathnames "data/" (get-app-directory)))
         (uploads-dir (merge-pathnames "data/uploads/" (get-app-directory)))
         (backups-dir (merge-pathnames "backups/" (get-app-directory)))
         (reports-dir (merge-pathnames "data/reports/" (get-app-directory)))
         (db-path *database-path*)
         ;; Get sizes
         (db-size (if (probe-file db-path)
                      (with-open-file (s db-path :element-type '(unsigned-byte 8))
                        (file-length s))
                      0))
         (uploads-size (get-directory-size (namestring uploads-dir)))
         (backups-size (get-directory-size (namestring backups-dir)))
         (reports-size (get-directory-size (namestring reports-dir)))
         (total-data (+ db-size uploads-size reports-size))
         (total-app (get-directory-size app-dir)))
    (list :database (list :path (namestring db-path)
                          :size db-size
                          :size-human (format-bytes db-size))
          :uploads (list :path (namestring uploads-dir)
                         :size uploads-size
                         :size-human (format-bytes uploads-size))
          :backups (list :path (namestring backups-dir)
                         :size backups-size
                         :size-human (format-bytes backups-size))
          :reports (list :path (namestring reports-dir)
                         :size reports-size
                         :size-human (format-bytes reports-size))
          :total-data (list :size total-data
                            :size-human (format-bytes total-data))
          :total-app (list :size total-app
                           :size-human (format-bytes total-app)))))

;;; ============================================================
;;; Process Information
;;; ============================================================

(defun get-top-processes ()
  "Get top processes by CPU and memory usage."
  (let* ((ps-output (run-shell-command "ps aux --sort=-%cpu | head -11"))
         (lines (cl-ppcre:split "\\n" ps-output))
         (processes nil))
    (dolist (line (cdr lines)) ; Skip header
      (let ((parts (cl-ppcre:split "\\s+" (string-trim '(#\Space) line) :limit 11)))
        (when (>= (length parts) 11)
          (push (list :user (first parts)
                      :pid (second parts)
                      :cpu (third parts)
                      :mem (fourth parts)
                      :vsz (fifth parts)
                      :rss (sixth parts)
                      :command (nth 10 parts))
                processes))))
    (nreverse processes)))

;;; ============================================================
;;; System Uptime
;;; ============================================================

(defun get-uptime-info ()
  "Get system uptime."
  (let* ((uptime-str (run-shell-command "uptime -p 2>/dev/null || uptime"))
         (uptime (string-trim '(#\Space #\Newline) uptime-str)))
    uptime))

;;; ============================================================
;;; Log Reading
;;; ============================================================

(defun get-log-files ()
  "Get list of available log files."
  (let ((logs nil))
    ;; Application log
    (let ((app-log (merge-pathnames "tfs-cmms.log" (get-app-directory))))
      (when (probe-file app-log)
        (push (list :name "Application Log" :path (namestring app-log)) logs)))
    ;; Backup log
    (when (probe-file "/var/log/tfs-cmms-backup.log")
      (push (list :name "Backup Log" :path "/var/log/tfs-cmms-backup.log") logs))
    ;; System logs (if accessible)
    (when (probe-file "/var/log/syslog")
      (push (list :name "System Log" :path "/var/log/syslog") logs))
    (nreverse logs)))

(defun read-log-tail (log-path &optional (lines 100))
  "Read the last N lines of a log file."
  (handler-case
      (let ((output (run-shell-command (format nil "tail -n ~D ~A 2>/dev/null" lines log-path))))
        (if (string= output "")
            "Log file is empty or not accessible."
            output))
    (error (e) (format nil "Error reading log: ~A" e))))

(defun get-journalctl-logs (&optional (lines 50))
  "Get recent systemd journal entries for tfs-cmms."
  (let ((output (run-shell-command 
                 (format nil "journalctl -u tfs-cmms -n ~D --no-pager 2>/dev/null" lines))))
    (if (string= output "")
        "No journal entries found or journalctl not available."
        output)))

;;; ============================================================
;;; System Monitor Page Handler
;;; ============================================================

(defun handle-admin-system-monitor ()
  "Admin page for system resource monitoring."
  (let ((user (get-current-user)))
    (if (and user (user-is-admin-p user))
        (let* ((cpu (get-cpu-info))
               (memory (get-memory-info))
               (disks (get-disk-info))
               (network (get-network-info))
               (processes (get-top-processes))
               (uptime (get-uptime-info))
               (app-storage (get-app-storage-info))
               (log-file (hunchentoot:parameter "log"))
               (log-content nil))
          ;; Get log content if requested
          (when log-file
            (setf log-content 
                  (cond
                    ((string= log-file "journal") (get-journalctl-logs 100))
                    ((probe-file log-file) (read-log-tail log-file 100))
                    (t "Log file not found."))))
          (html-response
           (render-page "System Monitor"
             (cl-who:with-html-output-to-string (s)
               (:style "
                 .monitor-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 1.5rem; margin-bottom: 1.5rem; }
                 .monitor-card { background: var(--card-bg); padding: 1.5rem; border-radius: 8px; border: 1px solid var(--border-color); }
                 .monitor-card h3 { margin: 0 0 1rem 0; display: flex; align-items: center; gap: 0.5rem; }
                 .stat-row { display: flex; justify-content: space-between; padding: 0.5rem 0; border-bottom: 1px solid var(--border-color); }
                 .stat-row:last-child { border-bottom: none; }
                 .stat-label { color: var(--text-muted); }
                 .stat-value { font-weight: 600; font-family: monospace; }
                 .progress-bar { height: 8px; background: var(--border-color); border-radius: 4px; overflow: hidden; margin-top: 0.5rem; }
                 .progress-fill { height: 100%; border-radius: 4px; transition: width 0.3s; }
                 .progress-fill.green { background: #28a745; }
                 .progress-fill.yellow { background: #ffc107; }
                 .progress-fill.red { background: #dc3545; }
                 .process-table { width: 100%; font-size: 0.85rem; }
                 .process-table th, .process-table td { padding: 0.5rem; text-align: left; border-bottom: 1px solid var(--border-color); }
                 .process-table th { font-weight: 600; }
                 .log-viewer { background: #1e1e1e; color: #d4d4d4; padding: 1rem; border-radius: 8px; font-family: monospace; font-size: 0.8rem; white-space: pre-wrap; word-wrap: break-word; max-height: 500px; overflow-y: auto; }
                 .log-selector { margin-bottom: 1rem; display: flex; gap: 0.5rem; flex-wrap: wrap; }
                 .log-selector a { padding: 0.5rem 1rem; background: var(--card-bg); border: 1px solid var(--border-color); border-radius: 4px; text-decoration: none; }
                 .log-selector a:hover, .log-selector a.active { background: var(--primary-color); color: white; border-color: var(--primary-color); }
                 .uptime-banner { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 1rem 1.5rem; border-radius: 8px; margin-bottom: 1.5rem; display: flex; justify-content: space-between; align-items: center; }
                 .refresh-btn { background: rgba(255,255,255,0.2); border: none; color: white; padding: 0.5rem 1rem; border-radius: 4px; cursor: pointer; }
                 .refresh-btn:hover { background: rgba(255,255,255,0.3); }
               ")
               
               (:div :class "page-header"
                 (:h1 "🖥️ System Monitor")
                 (:a :href "/admin/users" :class "btn btn-secondary" "Back to Admin"))
               
               ;; Uptime Banner
               (:div :class "uptime-banner"
                 (:div 
                   (:strong "System Uptime: ")
                   (cl-who:str uptime))
                 (:button :class "refresh-btn" :onclick "location.reload();" "🔄 Refresh"))
               
               ;; Main Stats Grid
               (:div :class "monitor-grid"
                 ;; CPU Card
                 (:div :class "monitor-card"
                   (:h3 "⚡ CPU")
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Cores")
                     (:span :class "stat-value" (cl-who:fmt "~A" (getf cpu :cpu-count))))
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Load (1m)")
                     (:span :class "stat-value" (cl-who:str (getf cpu :load-1))))
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Load (5m)")
                     (:span :class "stat-value" (cl-who:str (getf cpu :load-5))))
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Load (15m)")
                     (:span :class "stat-value" (cl-who:str (getf cpu :load-15))))
                   (let ((pct (or (getf cpu :load-percent) 0)))
                     (cl-who:htm
                      (:div :class "progress-bar"
                        (:div :class (format nil "progress-fill ~A" 
                                            (cond ((< pct 60) "green")
                                                  ((< pct 85) "yellow")
                                                  (t "red")))
                              :style (format nil "width: ~D%;" pct))))))
                 
                 ;; Memory Card
                 (:div :class "monitor-card"
                   (:h3 "🧠 Memory")
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Total")
                     (:span :class "stat-value" (cl-who:str (getf memory :total))))
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Used")
                     (:span :class "stat-value" (cl-who:str (getf memory :used))))
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Available")
                     (:span :class "stat-value" (cl-who:str (getf memory :available))))
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Cached")
                     (:span :class "stat-value" (cl-who:str (getf memory :cached))))
                   (let ((pct (getf memory :percent-used)))
                     (cl-who:htm
                      (:div :class "progress-bar"
                        (:div :class (format nil "progress-fill ~A" 
                                            (cond ((< pct 60) "green")
                                                  ((< pct 85) "yellow")
                                                  (t "red")))
                              :style (format nil "width: ~D%;" pct))))))
                 
                 ;; Swap Card
                 (:div :class "monitor-card"
                   (:h3 "💾 Swap")
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Total")
                     (:span :class "stat-value" (cl-who:str (getf memory :swap-total))))
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Used")
                     (:span :class "stat-value" (cl-who:str (getf memory :swap-used))))
                   (:div :class "stat-row"
                     (:span :class "stat-label" "Free")
                     (:span :class "stat-value" (cl-who:str (getf memory :swap-free))))
                   (let ((pct (getf memory :swap-percent)))
                     (cl-who:htm
                      (:div :class "progress-bar"
                        (:div :class (format nil "progress-fill ~A" 
                                            (cond ((< pct 30) "green")
                                                  ((< pct 60) "yellow")
                                                  (t "red")))
                              :style (format nil "width: ~D%;" pct))))))
                 
                 ;; Network Card
                 (:div :class "monitor-card"
                   (:h3 "🌐 Network")
                   (if network
                       (dolist (iface network)
                         (cl-who:htm
                          (:div :style "margin-bottom: 0.5rem;"
                            (:strong (cl-who:str (getf iface :interface))))
                          (:div :class "stat-row"
                            (:span :class "stat-label" "↓ Received")
                            (:span :class "stat-value" (cl-who:str (getf iface :rx-human))))
                          (:div :class "stat-row"
                            (:span :class "stat-label" "↑ Sent")
                            (:span :class "stat-value" (cl-who:str (getf iface :tx-human))))))
                       (cl-who:htm
                        (:p :class "text-muted" "No network interfaces found.")))))
               
               ;; Application Storage
               (:div :class "monitor-card" :style "margin-bottom: 1.5rem;"
                 (:h3 "📁 Application Storage")
                 (:table :class "process-table"
                   (:thead
                     (:tr
                       (:th "Directory")
                       (:th "Size")))
                   (:tbody
                     (:tr
                       (:td "Database")
                       (:td :class "stat-value" (cl-who:str (getf (getf app-storage :database) :size-human))))
                     (:tr
                       (:td "Uploads (images)")
                       (:td :class "stat-value" (cl-who:str (getf (getf app-storage :uploads) :size-human))))
                     (:tr
                       (:td "PDF Reports")
                       (:td :class "stat-value" (cl-who:str (getf (getf app-storage :reports) :size-human))))
                     (:tr
                       (:td "Backups")
                       (:td :class "stat-value" (cl-who:str (getf (getf app-storage :backups) :size-human))))
                     (:tr :style "border-top: 2px solid var(--border-color); font-weight: bold;"
                       (:td "Total Data")
                       (:td :class "stat-value" (cl-who:str (getf (getf app-storage :total-data) :size-human))))
                     (:tr :style "font-weight: bold;"
                       (:td "Total App Directory")
                       (:td :class "stat-value" (cl-who:str (getf (getf app-storage :total-app) :size-human)))))))
               
               ;; Disk Usage
               (:div :class "monitor-card" :style "margin-bottom: 1.5rem;"
                 (:h3 "💿 Disk Usage")
                 (if disks
                     (cl-who:htm
                      (:table :class "process-table"
                        (:thead
                          (:tr
                            (:th "Mount")
                            (:th "Size")
                            (:th "Used")
                            (:th "Available")
                            (:th "Usage")))
                        (:tbody
                          (dolist (disk disks)
                            (let ((pct (or (getf disk :percent) 0)))
                              (cl-who:htm
                               (:tr
                                 (:td (cl-who:str (getf disk :mount)))
                                 (:td (cl-who:str (getf disk :size)))
                                 (:td (cl-who:str (getf disk :used)))
                                 (:td (cl-who:str (getf disk :available)))
                                 (:td :style "min-width: 150px;"
                                   (:div :style "display: flex; align-items: center; gap: 0.5rem;"
                                     (:div :class "progress-bar" :style "flex: 1;"
                                       (:div :class (format nil "progress-fill ~A" 
                                                           (cond ((< pct 70) "green")
                                                                 ((< pct 90) "yellow")
                                                                 (t "red")))
                                             :style (format nil "width: ~D%;" pct)))
                                     (:span :style "font-size: 0.85rem;" 
                                            (cl-who:fmt "~D%" pct)))))))))))
                     (cl-who:htm
                      (:p :class "text-muted" "No disk information available."))))
               
               ;; Top Processes
               (:div :class "monitor-card" :style "margin-bottom: 1.5rem;"
                 (:h3 "📊 Top Processes (by CPU)")
                 (if processes
                     (cl-who:htm
                      (:table :class "process-table"
                        (:thead
                          (:tr
                            (:th "PID")
                            (:th "User")
                            (:th "CPU %")
                            (:th "MEM %")
                            (:th "Command")))
                        (:tbody
                          (dolist (proc processes)
                            (cl-who:htm
                             (:tr
                               (:td (cl-who:str (getf proc :pid)))
                               (:td (cl-who:str (getf proc :user)))
                               (:td (cl-who:str (getf proc :cpu)))
                               (:td (cl-who:str (getf proc :mem)))
                               (:td :style "max-width: 300px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"
                                    (cl-who:str (getf proc :command)))))))))
                     (cl-who:htm
                      (:p :class "text-muted" "No process information available."))))
               
               ;; Log Viewer
               (:div :class "monitor-card"
                 (:h3 "📜 Log Viewer")
                 (:div :class "log-selector"
                   (:a :href "/admin/system?log=journal" 
                       :class (when (string= log-file "journal") "active")
                       "Systemd Journal")
                   (dolist (log (get-log-files))
                     (cl-who:htm
                      (:a :href (format nil "/admin/system?log=~A" 
                                        (hunchentoot:url-encode (getf log :path)))
                          :class (when (string= log-file (getf log :path)) "active")
                          (cl-who:str (getf log :name))))))
                 (if log-content
                     (cl-who:htm
                      (:div :class "log-viewer" (cl-who:str log-content)))
                     (cl-who:htm
                      (:p :class "text-muted" "Select a log file to view."))))
               
               ;; Auto-refresh script
               (:script "
                 // Optional: Auto-refresh every 30 seconds
                 // setTimeout(function() { location.reload(); }, 30000);
               ")))))
        (redirect-to "/unauthorized"))))
