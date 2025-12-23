;;;; auth.lisp - User Authentication and Authorization

(in-package #:tfs-cmms)

;;; Password Hashing (simple SHA256 - in production use bcrypt/argon2)

(defun hash-password (password)
  "Hash a password using SHA256. In production, use bcrypt or argon2."
  (let ((digest (ironclad:make-digest :sha256)))
    (ironclad:update-digest digest (ironclad:ascii-string-to-byte-array password))
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defun verify-password (password hash)
  "Verify a password against its hash."
  (string= (hash-password password) hash))

;;; Session Management

(defun generate-session-token ()
  "Generate a random session token."
  (let ((bytes (make-array 32 :element-type '(unsigned-byte 8))))
    (dotimes (i 32)
      (setf (aref bytes i) (random 256)))
    (ironclad:byte-array-to-hex-string bytes)))

(defun create-session (user-id)
  "Create a new session for a user. Returns the session token."
  (let* ((token (generate-session-token))
         (expires (format nil "~A" 
                          (local-time:format-timestring 
                           nil 
                           (local-time:timestamp+ (local-time:now) 
                                                  *session-duration-hours* :hour)
                           :format '(:year "-" (:month 2) "-" (:day 2) " " 
                                     (:hour 2) ":" (:min 2) ":" (:sec 2))))))
    (execute-sql "INSERT INTO sessions (user_id, session_token, expires_at) VALUES (?, ?, ?)"
                 user-id token expires)
    ;; Update last login
    (execute-sql "UPDATE users SET last_login = datetime('now') WHERE id = ?" user-id)
    token))

(defun get-session (token)
  "Get session by token if valid and not expired."
  (fetch-one 
   "SELECT u.id, u.username, u.full_name, u.role, u.email, u.bog_date, 
           u.hire_date, u.staff_category, u.current_location, u.team_number, s.expires_at
    FROM sessions s
    JOIN users u ON s.user_id = u.id
    WHERE s.session_token = ? 
      AND s.expires_at > datetime('now')
      AND u.active = 1"
   token))

(defun delete-session (token)
  "Delete a session (logout)."
  (execute-sql "DELETE FROM sessions WHERE session_token = ?" token))

(defun cleanup-expired-sessions ()
  "Remove expired sessions."
  (execute-sql "DELETE FROM sessions WHERE expires_at < datetime('now')"))

;;; User Management

(defun create-user (username password full-name role &optional email)
  "Create a new user. Returns user ID or NIL if username exists."
  (handler-case
      (progn
        (execute-sql 
         "INSERT INTO users (username, password_hash, full_name, role, email) VALUES (?, ?, ?, ?, ?)"
         username (hash-password password) full-name role email)
        (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))
    (error () nil)))

(defun get-user (user-id)
  "Get user by ID."
  (fetch-one "SELECT * FROM users WHERE id = ?" user-id))

(defun get-user-by-username (username)
  "Get user by username."
  (fetch-one "SELECT * FROM users WHERE username = ?" username))

(defun authenticate-user (username password)
  "Authenticate user. Returns user record or NIL."
  (let ((user (get-user-by-username username)))
    (when (and user 
               (= 1 (getf user :|active|))
               (verify-password password (getf user :|password_hash|)))
      user)))

(defun list-users (&key role active-only)
  "List users with optional filters."
  (let ((conditions '("1=1"))
        (params '()))
    (when role
      (push "role = ?" conditions)
      (push role params))
    (when active-only
      (push "active = 1" conditions))
    (let ((sql (format nil 
                "SELECT id, username, full_name, email, role, active, created_at, last_login
                 FROM users WHERE ~{~A~^ AND ~} ORDER BY full_name"
                (reverse conditions))))
      (if params
          (apply #'fetch-all sql (reverse params))
          (fetch-all sql)))))

(defun update-user (user-id &key full-name email role electrician-type team-number hire-date bog-date active)
  "Update user details."
  (let ((updates '())
        (params '()))
    (when full-name
      (push "full_name = ?" updates)
      (push full-name params))
    (when email
      (push "email = ?" updates)
      (push email params))
    (when role
      (push "role = ?" updates)
      (push role params))
    ;; electrician-type can be nil to clear it
    (push "electrician_type = ?" updates)
    (push electrician-type params)
    ;; team-number can be nil to clear it
    (push "team_number = ?" updates)
    (push team-number params)
    ;; hire-date can be nil to clear it
    (push "hire_date = ?" updates)
    (push hire-date params)
    ;; bog-date can be nil to clear it
    (push "bog_date = ?" updates)
    (push bog-date params)
    (when active
      (push "active = ?" updates)
      (push (if (eq active t) 1 0) params))
    (when updates
      (apply #'execute-sql 
             (format nil "UPDATE users SET ~{~A~^, ~} WHERE id = ?"
                     (reverse updates))
             (append (reverse params) (list user-id))))))

(defun change-password (user-id new-password)
  "Change user password."
  (execute-sql "UPDATE users SET password_hash = ? WHERE id = ?"
               (hash-password new-password) user-id))

;;; Authorization Helpers

(defun user-can-qc-report-p (user report)
  "Check if user can QC a report (must be QC/Admin and not the inspector)."
  (let ((role (getf user :|role|))
        (inspector1 (getf report :|inspector1_name|))
        (inspector2 (getf report :|inspector2_name|))
        (user-name (getf user :|full_name|)))
    (and (member role '("QC" "Admin") :test #'string=)
         (not (string= user-name inspector1))
         (not (and inspector2 (string= user-name inspector2))))))

(defun user-is-admin-p (user)
  "Check if user is admin."
  (string= (getf user :|role|) "Admin"))

(defun user-is-qc-p (user)
  "Check if user is QC, QC Manager, or Admin."
  (member (getf user :|role|) '("qc" "qc_manager" "Admin") :test #'string-equal))

(defun user-is-qc-manager-p (user)
  "Check if user is QC Manager or Admin."
  (member (getf user :|role|) '("qc_manager" "Admin") :test #'string-equal))

(defun user-can-manage-inventory-p (user)
  "Check if user can manage inventory (property/materials staff or admin)."
  (member (getf user :|role|) 
          '("Admin" "property_manager" "materials_supervisor" "materials_specialist") 
          :test #'string-equal))

(defun user-can-access-master-tracker-p (user)
  "Check if user can access Master Tracker (Admin, QC Manager, Program Manager, QC, AO Lead)."
  (when user
    (let ((role (string-downcase (or (getf user :|role|) ""))))
      (or (user-is-admin-p user)
          (string= role "qc_manager")
          (string= role "program_manager")
          (string= role "qc")
          (string= role "ao_lead")))))

(defun get-qc-users ()
  "Get all QC users for assignment dropdown."
  (fetch-all "SELECT id, full_name, role FROM users WHERE role IN ('qc', 'qc_manager') AND active = 1 ORDER BY full_name"))

;;; Notifications

(defun create-notification (title message &key user-id role-target link)
  "Create a notification for a user or role."
  (execute-sql 
   "INSERT INTO notifications (user_id, role_target, title, message, link) VALUES (?, ?, ?, ?, ?)"
   user-id role-target title message link))

(defun notify-qc-team (report-id report-number inspector-name)
  "Notify QC team that a report is ready for review."
  (create-notification 
   "Report Ready for QC"
   (format nil "Report ~A submitted by ~A is ready for QC review." report-number inspector-name)
   :role-target "QC"
   :link (format nil "/inspection-reports/~A" report-id)))

(defun get-notifications-for-user (user)
  "Get unread notifications for a user (by ID or role)."
  (let ((user-id (getf user :|id|))
        (role (getf user :|role|)))
    (fetch-all 
     "SELECT * FROM notifications 
      WHERE (user_id = ? OR role_target = ? OR role_target = 'All')
        AND read = 0
      ORDER BY created_at DESC
      LIMIT 20"
     user-id role)))

(defun get-user-notifications (user)
  "Alias for get-notifications-for-user."
  (get-notifications-for-user user))

(defun mark-notification-read (notification-id)
  "Mark a notification as read."
  (execute-sql "UPDATE notifications SET read = 1 WHERE id = ?" notification-id))

(defun get-pending-qc-count ()
  "Get count of reports pending QC (all pending)."
  (getf (fetch-one "SELECT COUNT(*) as count FROM inspection_reports WHERE status = 'Pending QC'") 
        :|count|))

(defun get-pending-qc-count-for-user (user)
  "Get count of pending QC reports for a specific user based on role."
  (let* ((role (string-downcase (or (getf user :|role|) "")))
         (user-id (getf user :|id|)))
    (cond
      ;; Admin and QC Manager see all pending
      ((or (string= role "admin") (string= role "qc_manager"))
       (get-pending-qc-count))
      ;; QC specialists see only their assigned reports
      ((string= role "qc")
       (getf (fetch-one 
              "SELECT COUNT(*) as count FROM inspection_reports 
               WHERE status = 'Pending QC' AND assigned_qc_id = ?" 
              user-id) 
             :|count|))
      ;; Others see nothing
      (t 0))))

;;; Current User (from session cookie)

(defvar *current-user* nil
  "Current logged-in user for the request.")

(defun get-current-user ()
  "Get the current user from session cookie."
  (let ((token (hunchentoot:cookie-in "session")))
    (when token
      (get-session token))))

(defun require-login ()
  "Redirect to login if not logged in. Returns user if logged in."
  (let ((user (get-current-user)))
    (unless user
      (hunchentoot:redirect "/login"))
    user))

(defun require-role (allowed-roles)
  "Require user to have one of the allowed roles."
  (let ((user (require-login)))
    (unless (member (getf user :|role|) allowed-roles :test #'string=)
      (hunchentoot:redirect "/unauthorized"))
    user))

;;; Seed Default Admin User

(defun seed-admin-user ()
  "Create default admin user if no users exist."
  (let ((count (getf (fetch-one "SELECT COUNT(*) as count FROM users") :|count|)))
    (when (zerop count)
      (format t "~&Creating default admin user...~%")
      (create-user "admin" "admin123" "System Administrator" "Admin" "admin@tfsafe.mil")
      (format t "~&Default admin created: username=admin, password=admin123~%")
      (format t "~&*** CHANGE THIS PASSWORD IMMEDIATELY ***~%"))))
