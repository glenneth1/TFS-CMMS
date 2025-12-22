(in-package #:tfs-cmms)

;;; Hunchentoot Web Server

(defvar *acceptor* nil
  "Hunchentoot acceptor instance.")

(defvar *static-directory* 
  (merge-pathnames "static/" (get-app-directory))
  "Path to static files.")

(defvar *template-directory*
  (merge-pathnames "template/" (get-app-directory))
  "Path to template files.")

;;; Utility functions

(defun get-param (name)
  "Get request parameter by name."
  (hunchentoot:parameter name))

(defun parse-int (string)
  "Parse string to integer, return nil if invalid."
  (when (and string (> (length string) 0))
    (parse-integer string :junk-allowed t)))

(defun json-response (data)
  "Return JSON response."
  (setf (hunchentoot:content-type*) "application/json")
  (cl-json:encode-json-to-string data))

(defun html-response (html)
  "Return HTML response."
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  html)

(defun redirect-to (url)
  "Redirect to URL."
  (hunchentoot:redirect url))

;;; Simple HTML template rendering

(defun render-template (name &rest args)
  "Load and render a simple HTML template with variable substitution."
  (let* ((path (merge-pathnames name *template-directory*))
         (content (alexandria:read-file-into-string path)))
    ;; Simple variable substitution: {{var}} -> value
    (loop for (key val) on args by #'cddr
          do (setf content 
                   (cl-ppcre:regex-replace-all 
                    (format nil "\\{\\{~A\\}\\}" (string-downcase (symbol-name key)))
                    content
                    (princ-to-string (or val "")))))
    content))

;;; HTML Generation helpers using cl-who

(defmacro with-html (&body body)
  "Generate HTML string."
  `(cl-who:with-html-output-to-string (s nil :prologue t)
     ,@body))

(defun render-nav ()
  "Render navigation bar."
  (let* ((user (get-current-user))
         (pending-qc (if user (get-pending-qc-count-for-user user) 0)))
    (cl-who:with-html-output-to-string (s)
      (:nav :class "navbar"
        (:div :class "nav-logos"
          (:img :src "/static/img/company_logo.png" :alt "Versar" :class "logo-company")
          (:img :src "/static/img/TFS_Logo.png" :alt "Task Force SAFE" :class "logo-tfs"))
        (:div :class "nav-brand"
          (:a :href "/" "TF SAFE CMMS"))
        (:ul :class "nav-links"
          (:li (:a :href "/" "Dashboard"))
          (:li (:a :href "/work-orders" "Work Orders"))
          (:li (:a :href "/inspection-reports" "Inspection Reports"
                   (when (and user (user-is-qc-p user) (> pending-qc 0))
                     (cl-who:htm
                      (:span :class "notification-badge" (cl-who:str pending-qc))))))
          (:li (:a :href "/sites" "Sites"))
          (:li (:a :href "/inventory" "Inventory"))
          (:li (:a :href "/mrf" "MRF"))
          (:li (:a :href "/rr" "R&R"))
          (:li (:a :href "/dar" "DAR"))
          (:li (:a :href "/irp" "IRP"))
          (when (and user (user-can-generate-sar-p user))
            (cl-who:htm
             (:li (:a :href "/sar" "SAR"))))
          (when (and user (user-can-access-master-tracker-p user))
            (cl-who:htm
             (:li (:a :href "/master-tracker" "Master Tracker"))))
          (:li (:a :href "/reports" "Reports"))
          (when (and user (user-is-admin-p user))
            (cl-who:htm
             (:li :class "nav-admin"
               (:a :href "/admin/users" "Users"))
             (:li :class "nav-admin"
               (:a :href "/admin/settings" "Settings"))))
          (when (and user (or (user-is-admin-p user)
                              (string= (string-downcase (or (getf user :|role|) "")) "qc_manager")))
            (cl-who:htm
             (:li :class "nav-admin"
               (:a :href "/admin/reports" "Reports History")))))
        (when user
          (cl-who:htm
           (:div :class "nav-user"
             (:span :class "user-name" (cl-who:str (getf user :|full_name|)))
             (:span :class "user-role" (cl-who:str (format nil "(~A)" (getf user :|role|))))
             (:a :href "/logout" :class "btn-logout" "Logout"))))))))

(defun render-page (title content)
  "Wrap content in page layout."
  (with-html
    (:html
     (:head
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title (cl-who:str (format nil "TF SAFE CMMS - ~A" title)))
      (:link :rel "icon" :type "image/x-icon" :href "/static/favicon.ico")
      (:link :rel "stylesheet" :href "/static/css/style.css"))
     (:body
      (cl-who:str (render-nav))
      (:main :class "container"
        (cl-who:str content))
      (:footer :class "footer"
        (:p "Task Force SAFE - Computerized Maintenance Management System"))
      (:script :src "/static/js/app.js")))))

;;; Authentication Handlers

(defun handle-login ()
  "Login page."
  (let ((user (get-current-user)))
    (if user
        (redirect-to "/")
        (html-response
         (cl-who:with-html-output-to-string (s nil :prologue t)
           (:html
            (:head
             (:meta :charset "UTF-8")
             (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
             (:title "Login - TF SAFE CMMS")
             (:link :rel "stylesheet" :href "/static/css/style.css"))
            (:body :class "login-page"
             (:div :class "login-container"
               (:div :class "login-header"
                 (:img :src "/static/img/TFS_Logo.png" :alt "Task Force SAFE" :class "login-logo")
                 (:h1 "TF SAFE CMMS")
                 (:p "Computerized Maintenance Management System"))
               (:form :method "post" :action "/api/login" :class "login-form"
                 (:div :class "form-group"
                   (:label "Username")
                   (:input :type "text" :name "username" :required t :autofocus t))
                 (:div :class "form-group"
                   (:label "Password")
                   (:input :type "password" :name "password" :required t))
                 (:button :type "submit" :class "btn btn-primary btn-block" "Login"))
               (let ((error-msg (get-param "error")))
                 (when error-msg
                   (cl-who:htm
                    (:div :class "alert alert-danger" (cl-who:str error-msg)))))))))))))

(defun handle-api-login ()
  "Process login form."
  (let* ((username (get-param "username"))
         (password (get-param "password"))
         (user (authenticate-user username password)))
    (if user
        (let ((token (create-session (getf user :|id|)))
              (must-change (getf user :|must_change_password|)))
          (hunchentoot:set-cookie "session" :value token :path "/" :http-only t)
          ;; Check if user must change password
          (if (and must-change (= must-change 1))
              (redirect-to "/change-password?required=1")
              (redirect-to "/")))
        (redirect-to "/login?error=Invalid%20username%20or%20password"))))

(defun handle-logout ()
  "Logout and clear session."
  (let ((token (hunchentoot:cookie-in "session")))
    (when token
      (delete-session token))
    (hunchentoot:set-cookie "session" :value "" :path "/" :max-age 0)
    (redirect-to "/login")))

(defun handle-unauthorized ()
  "Unauthorized access page."
  (html-response
   (render-page "Unauthorized"
     (cl-who:with-html-output-to-string (s)
       (:div :class "empty-state"
         (:h1 "Access Denied")
         (:p "You do not have permission to access this page.")
         (:a :href "/" :class "btn" "Go Home"))))))

(defun handle-change-password ()
  "Password change page."
  (let ((user (get-current-user))
        (required (get-param "required"))
        (success (get-param "success"))
        (error-msg (get-param "error")))
    (if user
        (html-response
         (render-page "Change Password"
           (cl-who:with-html-output-to-string (s)
             (when required
               (cl-who:htm
                (:div :class "alert alert-warning"
                  (:strong "Password Change Required: ")
                  "You must change your password before continuing.")))
             (when success
               (cl-who:htm
                (:div :class "alert alert-success" "Password changed successfully.")))
             (when error-msg
               (cl-who:htm
                (:div :class "alert alert-danger" (cl-who:str error-msg))))
             (:div :class "card" :style "max-width: 500px; margin: 2rem auto;"
               (:h2 "Change Password")
               (:form :method "post" :action "/api/change-password"
                 (:div :class "form-group"
                   (:label "Current Password")
                   (:input :type "password" :name "current_password" :required t))
                 (:div :class "form-group"
                   (:label "New Password")
                   (:input :type "password" :name "new_password" :required t :minlength "6"))
                 (:div :class "form-group"
                   (:label "Confirm New Password")
                   (:input :type "password" :name "confirm_password" :required t))
                 (:div :class "form-actions"
                   (:button :type "submit" :class "btn btn-primary" "Change Password")
                   (unless required
                     (cl-who:htm
                      (:a :href "/" :class "btn" "Cancel")))))))))
        (redirect-to "/login"))))

(defun handle-api-change-password ()
  "Process password change."
  (let ((user (get-current-user)))
    (if user
        (let* ((current-password (get-param "current_password"))
               (new-password (get-param "new_password"))
               (confirm-password (get-param "confirm_password"))
               (user-id (getf user :|id|))
               (full-user (fetch-one "SELECT * FROM users WHERE id = ?" user-id)))
          (cond
            ((not (verify-password current-password (getf full-user :|password_hash|)))
             (redirect-to "/change-password?error=Current%20password%20is%20incorrect"))
            ((not (string= new-password confirm-password))
             (redirect-to "/change-password?error=New%20passwords%20do%20not%20match"))
            ((< (length new-password) 6)
             (redirect-to "/change-password?error=Password%20must%20be%20at%20least%206%20characters"))
            (t
             (execute-sql "UPDATE users SET password_hash = ?, must_change_password = 0 WHERE id = ?"
                          (hash-password new-password) user-id)
             (redirect-to "/?msg=Password%20changed%20successfully"))))
        (redirect-to "/login"))))

;;; Admin Panel Handlers

(defun handle-admin-reports ()
  "Admin reports page showing rejection history."
  (let ((user (get-current-user)))
    (if (and user (or (user-is-admin-p user) 
                      (string= (string-downcase (or (getf user :|role|) "")) "qc_manager")))
        (let ((rejected-reports (fetch-all 
                                 "SELECT r.*, s.name as site_name, s.code as site_code
                                  FROM inspection_reports r 
                                  JOIN sites s ON r.site_id = s.id 
                                  WHERE r.rejection_count > 0 OR r.status = 'QC Rejected'
                                  ORDER BY r.rejection_count DESC, r.updated_at DESC")))
          (html-response
           (render-page "Admin Reports"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "Report Statistics"))
               (:section :class "card"
                 (:h2 "Rejected Reports History")
                 (if rejected-reports
                     (cl-who:htm
                      (:table :class "data-table"
                        (:thead
                          (:tr (:th "Report #") (:th "Site") (:th "Building") 
                               (:th "Team") (:th "Rejections") (:th "Status")
                               (:th "QC Reviewer") (:th "QC Comments") (:th "Action")))
                        (:tbody
                          (dolist (rpt rejected-reports)
                            (cl-who:htm
                             (:tr
                               (:td (cl-who:str (getf rpt :|report_number|)))
                               (:td (cl-who:str (format nil "~A (~A)" 
                                                        (getf rpt :|site_name|)
                                                        (getf rpt :|site_code|))))
                               (:td (cl-who:str (getf rpt :|building_number|)))
                               (:td (cl-who:str (getf rpt :|team_number|)))
                               (:td 
                               (let ((rej-count (or (getf rpt :|rejection_count|) 0)))
                                 (if (> rej-count 0)
                                     (cl-who:htm
                                      (:a :href (format nil "/inspection-reports/~A/history" (getf rpt :|id|))
                                          :class "badge badge-danger" :title "Click to view rejection history"
                                          (cl-who:str rej-count)))
                                     (cl-who:htm
                                      (:span :class "badge" "0")))))
                               (:td (:span :class (format nil "badge badge-~A" 
                                                          (string-downcase 
                                                           (substitute #\- #\Space (getf rpt :|status|))))
                                           (cl-who:str (getf rpt :|status|))))
                               (:td (cl-who:str (or (getf rpt :|qc_name|) "-")))
                               (:td (cl-who:str (or (getf rpt :|last_rejection_comments|) 
                                                       (getf rpt :|qc_comments|) "-")))
                               (:td (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                                        :class "btn btn-sm" "View"))))))))
                     (cl-who:htm
                      (:p :class "empty-state" "No rejected reports found."))))
               (:div :class "page-actions"
                 (:a :href "/admin/users" :class "btn" "Manage Users")
                 (:a :href "/" :class "btn" "Dashboard"))))))
        (redirect-to "/unauthorized"))))

(defun handle-admin-users ()
  "User management page - Admin only."
  (let ((user (get-current-user)))
    (if (and user (user-is-admin-p user))
        (let ((users (list-users)))
          (html-response
           (render-page "User Management"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "User Management")
                 (:a :href "/admin/users/new" :class "btn btn-primary" "+ New User"))
               (:section :class "card"
                 (:table :class "data-table user-table"
                   (:thead
                     (:tr (:th "Username") (:th "Full Name") (:th "Email") 
                          (:th "Role") (:th "Status") (:th "Last Login") (:th "Actions")))
                   (:tbody
                     (dolist (u users)
                       (cl-who:htm
                        (:tr
                          (:td (cl-who:str (getf u :|username|)))
                          (:td (cl-who:str (getf u :|full_name|)))
                          (:td (cl-who:str (or (getf u :|email|) "-")))
                          (:td (:span :class (format nil "role-badge ~A" 
                                                     (string-downcase (getf u :|role|)))
                                      (cl-who:str (getf u :|role|))))
                          (:td (:span :class (if (= 1 (getf u :|active|)) 
                                                 "status-active" "status-inactive")
                                      (cl-who:str (if (= 1 (getf u :|active|)) "Active" "Inactive"))))
                          (:td (cl-who:str (or (getf u :|last_login|) "Never")))
                          (:td 
                            (:a :href (format nil "/admin/users/~A/edit" (getf u :|id|))
                                :class "btn btn-sm" "Edit"))))))))))))
        (redirect-to "/unauthorized"))))

(defun handle-admin-user-new ()
  "New user form - Admin only."
  (let ((user (get-current-user)))
    (if (and user (user-is-admin-p user))
        (html-response
         (render-page "New User"
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 "Create New User"))
             (:form :method "post" :action "/api/admin/users/create" :class "form-card"
               (:div :class "form-row"
                 (:div :class "form-group required"
                   (:label "Username")
                   (:input :type "text" :name "username" :required t 
                           :placeholder "e.g., jsmith"))
                 (:div :class "form-group required"
                   (:label "Full Name")
                   (:input :type "text" :name "full_name" :required t
                           :placeholder "e.g., John Smith")))
               (:div :class "form-row"
                 (:div :class "form-group"
                   (:label "Email")
                   (:input :type "email" :name "email" :placeholder "john.smith@example.com"))
                 (:div :class "form-group required"
                   (:label "Role")
                   (:select :name "role" :required t
                     (dolist (r *user-roles*)
                       (cl-who:htm (:option :value r (cl-who:str r)))))))
               (:div :class "form-row"
                 (:div :class "form-group required"
                   (:label "Password")
                   (:input :type "password" :name "password" :required t :minlength "6"))
                 (:div :class "form-group required"
                   (:label "Confirm Password")
                   (:input :type "password" :name "password_confirm" :required t)))
               (:div :class "form-actions"
                 (:button :type "submit" :class "btn btn-primary" "Create User")
                 (:a :href "/admin/users" :class "btn" "Cancel"))))))
        (redirect-to "/unauthorized"))))

(defun handle-admin-user-edit (user-id-str)
  "Edit user form - Admin only."
  (let ((current-user (get-current-user))
        (user-id (parse-int user-id-str)))
    (if (and current-user (user-is-admin-p current-user))
        (let ((edit-user (get-user user-id)))
          (if edit-user
              (html-response
               (render-page "Edit User"
                 (cl-who:with-html-output-to-string (s)
                   (:div :class "page-header"
                     (:h1 (cl-who:fmt "Edit User: ~A" (getf edit-user :|username|))))
                   (:form :method "post" :action (format nil "/api/admin/users/~A/update" user-id)
                          :class "form-card"
                     (:div :class "form-row"
                       (:div :class "form-group"
                         (:label "Username")
                         (:input :type "text" :value (getf edit-user :|username|) :disabled t))
                       (:div :class "form-group required"
                         (:label "Full Name")
                         (:input :type "text" :name "full_name" :required t
                                 :value (getf edit-user :|full_name|))))
                     (:div :class "form-row"
                      (:div :class "form-group"
                        (:label "Email")
                        (:input :type "email" :name "email" 
                                :value (or (getf edit-user :|email|) "")))
                      (:div :class "form-group required"
                        (:label "Role")
                        (:select :name "role" :required t
                          (dolist (r *user-roles*)
                            (cl-who:htm 
                             (:option :value r 
                                      :selected (string= r (getf edit-user :|role|))
                                      (cl-who:str r)))))))
                    (:div :class "form-row"
                      (:div :class "form-group"
                        (:label "Electrician Type")
                        (:select :name "electrician_type"
                          (:option :value "" :selected (null (getf edit-user :|electrician_type|)) "N/A")
                          (:option :value "Master" :selected (equal "Master" (getf edit-user :|electrician_type|)) "Master Electrician")
                          (:option :value "Journeyman" :selected (equal "Journeyman" (getf edit-user :|electrician_type|)) "Journeyman")))
                      (:div :class "form-group"
                        (:label "Team Number")
                        (:input :type "text" :name "team_number" :placeholder "e.g., 230"
                                :value (or (getf edit-user :|team_number|) ""))))
                    (:div :class "form-row"
                      (:div :class "form-group"
                        (:label "Hire Date")
                        (:input :type "date" :name "hire_date"
                                :value (or (getf edit-user :|hire_date|) "")))
                      (:div :class "form-group"
                        (:label "BOG Date (Boots on Ground)")
                        (:input :type "date" :name "bog_date"
                                :value (or (getf edit-user :|bog_date|) ""))))
                    (:div :class "form-group"
                      (:label "Status")
                      (:select :name "active"
                        (:option :value "1" :selected (= 1 (getf edit-user :|active|)) "Active")
                        (:option :value "0" :selected (= 0 (getf edit-user :|active|)) "Inactive")))
                    (:div :class "form-actions"
                      (:button :type "submit" :class "btn btn-primary" "Save Changes")
                      (:a :href "/admin/users" :class "btn" "Cancel")))
                   
                   ;; Password change requirement status
                   (:section :class "card" :style "margin-top: 1rem;"
                     (:h2 "Password Settings")
                     (:p :class "text-muted" 
                         (if (and (getf edit-user :|must_change_password|) 
                                  (= 1 (getf edit-user :|must_change_password|)))
                             (cl-who:htm (:span :style "color: orange;" "⚠ User must change password on next login"))
                             (cl-who:htm (:span :style "color: green;" "✓ No password change required"))))
                     (when (and (getf edit-user :|must_change_password|) 
                                (= 1 (getf edit-user :|must_change_password|)))
                       (cl-who:htm
                        (:form :method "post" :action (format nil "/api/admin/users/~A/clear-password-flag" user-id)
                               :style "margin-top: 0.5rem;"
                          (:button :type "submit" :class "btn btn-sm" "Clear Password Change Requirement")))))
                   
                   ;; Password reset section
                   (:section :class "card" :style "margin-top: 1rem;"
                     (:h2 "Reset Password")
                     (:form :method "post" :action (format nil "/api/admin/users/~A/reset-password" user-id)
                       (:div :class "form-row"
                         (:div :class "form-group required"
                           (:label "New Password")
                           (:input :type "password" :name "new_password" :required t :minlength "6"))
                         (:div :class "form-group required"
                           (:label "Confirm Password")
                           (:input :type "password" :name "confirm_password" :required t)))
                       (:div :class "form-group"
                         (:label
                           (:input :type "checkbox" :name "require_change" :value "1" :checked t)
                           " Require user to change password on next login"))
                       (:button :type "submit" :class "btn btn-danger" "Reset Password"))))))
              (redirect-to "/admin/users")))
        (redirect-to "/unauthorized"))))

(defun handle-api-admin-user-create ()
  "Create a new user - Admin only."
  (let ((user (get-current-user)))
    (if (and user (user-is-admin-p user))
        (let ((username (get-param "username"))
              (full-name (get-param "full_name"))
              (email (get-param "email"))
              (role (get-param "role"))
              (password (get-param "password"))
              (password-confirm (get-param "password_confirm")))
          (if (string= password password-confirm)
              (let ((new-id (create-user username password full-name role email)))
                (if new-id
                    (redirect-to "/admin/users")
                    (redirect-to "/admin/users/new?error=Username%20already%20exists")))
              (redirect-to "/admin/users/new?error=Passwords%20do%20not%20match")))
        (redirect-to "/unauthorized"))))

(defun handle-api-admin-user-update (user-id-str)
  "Update a user - Admin only."
  (let ((current-user (get-current-user))
        (user-id (parse-int user-id-str)))
    (if (and current-user (user-is-admin-p current-user))
        (let ((full-name (get-param "full_name"))
              (email (get-param "email"))
              (role (get-param "role"))
              (electrician-type (get-param "electrician_type"))
              (team-number (get-param "team_number"))
              (hire-date (get-param "hire_date"))
              (bog-date (get-param "bog_date"))
              (active (parse-int (get-param "active"))))
          (update-user user-id 
                       :full-name full-name 
                       :email email 
                       :role role 
                       :electrician-type (when (and electrician-type (not (string= electrician-type ""))) electrician-type)
                       :team-number (when (and team-number (not (string= team-number ""))) team-number)
                       :hire-date (when (and hire-date (not (string= hire-date ""))) hire-date)
                       :bog-date (when (and bog-date (not (string= bog-date ""))) bog-date)
                       :active (= active 1))
          (redirect-to "/admin/users"))
        (redirect-to "/unauthorized"))))

(defun handle-api-admin-reset-password (user-id-str)
  "Reset user password - Admin only."
  (let ((current-user (get-current-user))
        (user-id (parse-int user-id-str)))
    (if (and current-user (user-is-admin-p current-user))
        (let ((new-password (get-param "new_password"))
              (confirm-password (get-param "confirm_password"))
              (require-change (get-param "require_change")))
          (if (string= new-password confirm-password)
              (progn
                (change-password user-id new-password)
                ;; Set must_change_password flag if checkbox was checked
                (when require-change
                  (execute-sql "UPDATE users SET must_change_password = 1 WHERE id = ?" user-id))
                (redirect-to (format nil "/admin/users/~A/edit?success=Password%20reset" user-id)))
              (redirect-to (format nil "/admin/users/~A/edit?error=Passwords%20do%20not%20match" user-id))))
        (redirect-to "/unauthorized"))))

(defun handle-api-admin-clear-password-flag (user-id-str)
  "Clear must_change_password flag - Admin only."
  (let ((current-user (get-current-user))
        (user-id (parse-int user-id-str)))
    (if (and current-user (user-is-admin-p current-user))
        (progn
          (execute-sql "UPDATE users SET must_change_password = 0 WHERE id = ?" user-id)
          (redirect-to (format nil "/admin/users/~A/edit?success=Password%20change%20requirement%20cleared" user-id)))
        (redirect-to "/unauthorized"))))

(defun handle-admin-settings ()
  "Handle admin settings page."
  (let ((user (get-current-user)))
    (if (and user (user-is-admin-p user))
        (let ((settings (get-all-system-settings))
              (success (hunchentoot:parameter "success"))
              (error-msg (hunchentoot:parameter "error")))
          (html-response
           (render-page "System Settings"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "System Settings")
                 (:a :href "/admin/users" :class "btn btn-secondary" "Back to Users"))
               
               (when success
                 (cl-who:htm
                  (:div :class "alert alert-success" (cl-who:str success))))
               (when error-msg
                 (cl-who:htm
                  (:div :class "alert alert-danger" (cl-who:str error-msg))))
               
               (:section :class "card"
                 (:h2 "Contract Settings")
                 (:form :method "post" :action "/api/admin/settings/update"
                   (:div :class "form-group"
                     (:label "Contract Number")
                     (:input :type "text" :name "contract_number" 
                             :value (or (get-system-setting "contract_number") "")
                             :placeholder "e.g., W912DY24R0043"))
                   (:p :class "text-muted" "This contract number will be used on all new MRF forms.")
                   (:button :type "submit" :class "btn btn-primary" "Save Settings")))
               
               (:section :class "card" :style "margin-top: 1rem;"
                 (:h2 "All Settings")
                 (:table :class "data-table"
                   (:thead
                     (:tr
                       (:th "Setting")
                       (:th "Value")
                       (:th "Description")
                       (:th "Last Updated")))
                   (:tbody
                     (if settings
                         (dolist (setting settings)
                           (cl-who:htm
                            (:tr
                              (:td (:code (cl-who:str (getf setting :|setting_key|))))
                              (:td (cl-who:str (or (getf setting :|setting_value|) "")))
                              (:td (cl-who:str (or (getf setting :|description|) "")))
                              (:td (cl-who:str (let ((dt (or (getf setting :|updated_at|) "")))
                                        (if (>= (length dt) 10) (format-date-display (subseq dt 0 10)) dt)))))))
                         (cl-who:htm
                          (:tr (:td :colspan "4" :class "text-center" "No settings configured")))))))))))
        (redirect-to "/unauthorized"))))

(defun handle-api-admin-settings-update ()
  "Handle admin settings update."
  (let ((user (get-current-user)))
    (if (and user (user-is-admin-p user))
        (let ((contract-number (hunchentoot:parameter "contract_number")))
          (when contract-number
            (set-system-setting "contract_number" contract-number "Current contract number for MRF forms"))
          (redirect-to "/admin/settings?success=Settings%20updated"))
        (redirect-to "/unauthorized"))))

;;; Route handlers

(defun handle-index ()
  "Dashboard showing pending items based on user role."
  (let ((user (get-current-user)))
    (if user
        (html-response
         (render-page "Dashboard"
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 (cl-who:fmt "Welcome, ~A" (getf user :|full_name|))))
             
             ;; Role-specific dashboard content
             (let* ((role (getf user :|role|))
                   (role-lower (string-downcase (or role ""))))
               (cond
                 ;; QC Dashboard (QC, qc_manager)
                 ((or (string= role-lower "qc") (string= role-lower "qc_manager"))
                  (let* ((is-qc-manager (string= role-lower "qc_manager"))
                         (user-id (or (getf user :|id|) (getf user :|user_id|)))
                         (qc-users (get-qc-users))
                         (pending-reports (if is-qc-manager
                                              ;; QC Manager sees all pending reports
                                              (fetch-all 
                                               "SELECT r.*, s.name as site_name, u.full_name as assigned_to_name
                                                FROM inspection_reports r 
                                                JOIN sites s ON r.site_id = s.id 
                                                LEFT JOIN users u ON r.assigned_qc_id = u.id
                                                WHERE r.status = 'Pending QC' 
                                                ORDER BY r.assigned_qc_id IS NULL DESC, r.created_at DESC")
                                              ;; QC Specialists see only their assigned reports
                                              (fetch-all 
                                               "SELECT r.*, s.name as site_name 
                                                FROM inspection_reports r 
                                                JOIN sites s ON r.site_id = s.id 
                                                WHERE r.status = 'Pending QC' 
                                                  AND r.assigned_qc_id = ?
                                                ORDER BY r.created_at DESC"
                                               user-id))))
                    (cl-who:htm
                     (:section :class "card"
                       (:h2 (cl-who:str (if is-qc-manager "All Reports Awaiting QC" "My Assigned Reports"))
                            (:span :class "notification-badge" 
                                   (cl-who:str (length pending-reports))))
                       (if pending-reports
                           (cl-who:htm
                            (:table :class "data-table"
                              (:thead
                                (:tr (:th "Report #") (:th "Site") (:th "Building") 
                                     (:th "Inspector") (:th "Submitted") 
                                     (when is-qc-manager
                                       (cl-who:htm (:th "Assigned To")))
                                     (:th "Action")))
                              (:tbody
                                (dolist (rpt pending-reports)
                                  (cl-who:htm
                                   (:tr
                                     (:td (cl-who:str (getf rpt :|report_number|)))
                                     (:td (cl-who:str (getf rpt :|site_name|)))
                                     (:td (cl-who:str (getf rpt :|building_number|)))
                                     (:td (cl-who:str (getf rpt :|inspector1_name|)))
                                     (:td (cl-who:str (getf rpt :|inspector1_signed_at|)))
                                     (when is-qc-manager
                                       (cl-who:htm
                                        (:td 
                                         (:form :method "post" 
                                                :action (format nil "/api/inspection-reports/~A/assign" (getf rpt :|id|))
                                                :class "assign-form"
                                           (:select :name "assigned_qc_id" :onchange "this.form.submit()"
                                             (:option :value "" 
                                                      :selected (not (getf rpt :|assigned_qc_id|))
                                                      "-- Unassigned --")
                                             (dolist (qc-user qc-users)
                                               (cl-who:htm
                                                (:option :value (getf qc-user :|id|)
                                                         :selected (eql (getf rpt :|assigned_qc_id|) 
                                                                        (getf qc-user :|id|))
                                                         (cl-who:str (getf qc-user :|full_name|))))))))))
                                     (:td 
                                      (cond
                                        ;; QC Specialist always sees "Process"
                                        ((not is-qc-manager)
                                         (cl-who:htm
                                          (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                                              :class "btn btn-success btn-sm" "Process")))
                                        ;; QC Manager: assigned = Review, unassigned = Assign
                                        ((getf rpt :|assigned_qc_id|)
                                         (cl-who:htm 
                                          (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                                              :class "btn btn-success btn-sm" "Review")))
                                        (t
                                         (cl-who:htm
                                          (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                                              :class "btn btn-primary btn-sm" "Assign")))))))))))
                           (cl-who:htm
                            (:p :class "empty-state" 
                                (if is-qc-manager 
                                    "No reports pending QC review."
                                    "No reports assigned to you."))))))))
                 
                 ;; Inspector Dashboard
                 ((string= role "Inspector")
                  (let* ((user-name (getf user :|full_name|))
                         (rejected-reports (fetch-all 
                                            "SELECT r.*, s.name as site_name 
                                             FROM inspection_reports r 
                                             JOIN sites s ON r.site_id = s.id 
                                             WHERE r.status = 'QC Rejected' 
                                               AND (r.inspector1_name = ? OR r.inspector2_name = ?)
                                             ORDER BY r.updated_at DESC"
                                            user-name user-name))
                         (draft-reports (fetch-all 
                                         "SELECT r.*, s.name as site_name 
                                          FROM inspection_reports r 
                                          JOIN sites s ON r.site_id = s.id 
                                          WHERE r.status = 'Draft' 
                                            AND (r.inspector1_name = ? OR r.inspector2_name = ? 
                                                 OR r.inspector1_name IS NULL)
                                          ORDER BY r.created_at DESC LIMIT 10"
                                         user-name user-name)))
                    (cl-who:htm
                     (when rejected-reports
                       (cl-who:htm
                        (:section :class "card alert-card"
                          (:h2 "Reports Requiring Corrections"
                               (:span :class "notification-badge badge-danger" 
                                      (cl-who:str (length rejected-reports))))
                          (:table :class "data-table"
                            (:thead
                              (:tr (:th "Report #") (:th "Site") (:th "QC Comments") (:th "Action")))
                            (:tbody
                              (dolist (rpt rejected-reports)
                                (cl-who:htm
                                 (:tr
                                   (:td (cl-who:str (getf rpt :|report_number|)))
                                   (:td (cl-who:str (getf rpt :|site_name|)))
                                   (:td (cl-who:str (or (getf rpt :|qc_comments|) "-")))
                                   (:td (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                                            :class "btn btn-danger btn-sm" "Fix Issues"))))))))))
                     (:section :class "card"
                       (:h2 "Draft Reports")
                       (if draft-reports
                           (cl-who:htm
                            (:table :class "data-table"
                              (:thead
                                (:tr (:th "Report #") (:th "Site") (:th "Building") (:th "Created") (:th "Action")))
                              (:tbody
                                (dolist (rpt draft-reports)
                                  (cl-who:htm
                                   (:tr
                                     (:td (cl-who:str (getf rpt :|report_number|)))
                                     (:td (cl-who:str (getf rpt :|site_name|)))
                                     (:td (cl-who:str (getf rpt :|building_number|)))
                                     (:td (cl-who:str (getf rpt :|created_at|)))
                                     (:td (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                                              :class "btn btn-sm" "Continue"))))))))
                           (cl-who:htm
                            (:p :class "empty-state" "No draft reports."))))
                     ;; My Submitted Reports section
                     (let ((my-reports (fetch-all 
                                        "SELECT r.*, s.name as site_name 
                                         FROM inspection_reports r 
                                         JOIN sites s ON r.site_id = s.id 
                                         WHERE (r.inspector1_name = ? OR r.inspector2_name = ?)
                                           AND r.status NOT IN ('Draft', 'QC Rejected')
                                         ORDER BY r.updated_at DESC LIMIT 10"
                                        user-name user-name)))
                       (cl-who:htm
                        (:section :class "card"
                          (:h2 "My Submitted Reports")
                          (if my-reports
                              (cl-who:htm
                               (:table :class "data-table"
                                 (:thead
                                   (:tr (:th "Report #") (:th "Site") (:th "Building") (:th "Status") (:th "Action")))
                                 (:tbody
                                   (dolist (rpt my-reports)
                                     (cl-who:htm
                                      (:tr
                                        (:td (cl-who:str (getf rpt :|report_number|)))
                                        (:td (cl-who:str (getf rpt :|site_name|)))
                                        (:td (cl-who:str (getf rpt :|building_number|)))
                                        (:td (:span :class (format nil "badge badge-~A" 
                                                                   (string-downcase 
                                                                    (substitute #\- #\Space (getf rpt :|status|))))
                                                    (cl-who:str (getf rpt :|status|))))
                                        (:td (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                                                 :class "btn btn-sm" "View"))))))))
                              (cl-who:htm
                               (:p :class "empty-state" "No submitted reports yet."))))))))) ;; closes: if, :section, cl-who:htm, let, cl-who:htm, let*, cond-clause
                 
                 ;; Admin Dashboard
                 ((string= role "Admin")
                  (let ((pending-qc (get-pending-qc-count))
                        (total-users (getf (fetch-one "SELECT COUNT(*) as c FROM users") :|c|))
                        (total-reports (getf (fetch-one "SELECT COUNT(*) as c FROM inspection_reports") :|c|)))
                    (cl-who:htm
                     (:div :class "dashboard-stats"
                       (:div :class "stat-card"
                         (:h3 "Pending QC")
                         (:p :class "stat-number" (cl-who:str pending-qc)))
                       (:div :class "stat-card"
                         (:h3 "Total Users")
                         (:p :class "stat-number" (cl-who:str total-users)))
                       (:div :class "stat-card"
                         (:h3 "Total Reports")
                         (:p :class "stat-number" (cl-who:str total-reports))))
                     (:section :class "card"
                       (:h2 "Quick Actions")
                       (:div :class "quick-actions"
                         (:a :href "/admin/users" :class "btn btn-primary" "Manage Users")
                         (:a :href "/inspection-reports" :class "btn" "View All Reports")
                         (:a :href "/work-orders" :class "btn" "View Work Orders"))))))))
             
             ;; Notifications for all users
             (let ((notifications (get-user-notifications user)))
               (when notifications
                 (cl-who:htm
                  (:section :class "card"
                    (:h2 "Notifications" 
                         (:span :class "notification-badge" (cl-who:str (length notifications))))
                    (:div :class "notification-list"
                      (dolist (notif notifications)
                        (cl-who:htm
                         (:div :class (format nil "notification-item ~A" 
                                              (if (= 0 (getf notif :|read|)) "unread" ""))
                           (:div :class "notification-title" 
                                 (cl-who:str (getf notif :|title|)))
                           (:div :class "notification-message"
                                 (cl-who:str (getf notif :|message|)))
                           (:div :class "notification-actions"
                             (when (getf notif :|link|)
                               (cl-who:htm
                                (:a :href (getf notif :|link|) :class "btn btn-sm" "View Report")))
                             (:form :method "post" :action (format nil "/api/notifications/~A/read" (getf notif :|id|))
                                    :style "display:inline"
                               (:button :type "submit" :class "btn btn-sm" "Dismiss")))))))))))
             
             ;; Quick links for all users
             (:section :class "card"
               (:h2 "Quick Links")
               (:div :class "quick-actions"
                 (:a :href "/work-orders" :class "btn" "Work Orders")
                 (:a :href "/inspection-reports" :class "btn" "Inspection Reports")
                 (:a :href "/master-tracker" :class "btn btn-primary" "Master Tracker")
                 (:a :href "/sites" :class "btn" "Sites"))))))
        (redirect-to "/login"))))

(defun handle-work-orders ()
  "Work orders list page."
  (let* ((site-id (parse-int (get-param "site")))
         (status-param (get-param "status"))
         (status (when (and status-param (> (length status-param) 0)) status-param))
         (date-from (get-param "date_from"))
         (date-to (get-param "date_to"))
         (search-term (get-param "search"))
         (work-orders (if (and search-term (> (length search-term) 0))
                          (search-work-orders search-term :site-id site-id :limit 200)
                          (list-work-orders :site-id site-id :status status 
                                            :date-from date-from :date-to date-to :limit 200)))
         (sites (list-sites)))
    (html-response
     (render-page "Work Orders"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Work Orders")
           (:a :href "/work-orders/new" :class "btn btn-primary" "+ New Work Order"))
         (:div :class "filters"
           (:form :method "get" :action "/work-orders" :class "filter-form"
             (:label "Search:"
               (:input :type "text" :name "search" :placeholder "WO#, Building#, Location..." 
                       :value (or search-term "") :style "width: 200px;"))
             (:label "Site:"
               (:select :name "site"
                 (:option :value "" "All Sites")
                 (dolist (site sites)
                   (cl-who:htm
                    (:option :value (princ-to-string (getf site :|id|))
                             :selected (eql (getf site :|id|) site-id)
                             (cl-who:str (format nil "~A - ~A" 
                                                 (getf site :|code|) 
                                                 (getf site :|name|))))))))
             (:label "Status:"
               (:select :name "status"
                 (:option :value "" "All Statuses")
                 (dolist (st *wo-statuses*)
                   (cl-who:htm
                    (:option :value st :selected (equal st status) (cl-who:str st))))))
             (:label "From:"
               (:input :type "date" :name "date_from" :value (or date-from "")))
             (:label "To:"
               (:input :type "date" :name "date_to" :value (or date-to "")))
             (:button :type "submit" :class "btn" "Filter")
             (:a :href "/work-orders" :class "btn btn-secondary" "Clear")))
         (:table :class "data-table"
           (:thead
             (:tr
               (:th "WO Number")
               (:th "Site")
               (:th "Facility")
               (:th "Priority")
               (:th "Status")
               (:th "Progress")
               (:th "Created")
               (:th "Actions")))
           (:tbody
             (if work-orders
                 (dolist (wo work-orders)
                   (cl-who:htm
                    (:tr
                      (:td (:a :href (format nil "/work-orders/~A" (getf wo :|id|))
                               (cl-who:str (getf wo :|wo_number|))))
                      (:td (cl-who:str (or (getf wo :|site_code|) "-")))
                      (:td (cl-who:str (or (getf wo :|facility_code|) "-")))
                      (:td (:span :class (format nil "badge badge-~A" 
                                                 (string-downcase (or (getf wo :|priority|) "")))
                                  (cl-who:str (or (getf wo :|priority|) "-"))))
                      (:td (:span :class "status-badge" 
                                  (cl-who:str (or (getf wo :|status|) "-"))))
                      (:td (cl-who:str (format nil "~A%" (or (getf wo :|progress_pct|) 0))))
                      (:td (cl-who:str (subseq (or (getf wo :|created_at|) "") 0 
                                               (min 10 (length (or (getf wo :|created_at|) ""))))))
                      (:td (:a :href (format nil "/work-orders/~A" (getf wo :|id|))
                               :class "btn btn-sm" "View")))))
                 (cl-who:htm
                  (:tr (:td :colspan "8" :class "empty-state" 
                            "No work orders found.")))))))))))

(defun handle-work-order-detail (id)
  "Work order detail page."
  (let* ((wo-id (parse-int id))
         (wo (get-work-order wo-id))
         (activities (when wo (get-wo-activities wo-id))))
    (if wo
        (html-response
         (render-page (format nil "WO ~A" (getf wo :|wo_number|))
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 (cl-who:fmt "Work Order: ~A" (getf wo :|wo_number|)))
               (:span :class (format nil "badge badge-~A" 
                                     (string-downcase (or (getf wo :|priority|) "")))
                      (cl-who:str (or (getf wo :|priority|) ""))))
             (:div :class "wo-detail-grid"
               (:section :class "card"
                 (:h2 "Status")
                 (:dl :class "detail-list"
                   (:dt "Current Status") (:dd (:span :class "status-badge" (cl-who:str (or (getf wo :|status|) "-"))))
                   (:dt "Scheduled Date") (:dd (cl-who:str (or (getf wo :|target_start|) "Not scheduled")))
                   (:dt "Assigned To") (:dd (cl-who:str (or (getf wo :|assigned_to|) "Unassigned")))
                   (:dt "Created") (:dd (cl-who:str (or (getf wo :|created_at|) "-")))))
               (:section :class "card"
                 (:h2 "Update Status")
                 (:form :method "post" :action (format nil "/api/work-orders/~A/update" (getf wo :|id|))
                   (:div :class "form-group"
                     (:label "New Status")
                     (:select :name "status"
                       (dolist (st *wo-statuses*)
                         (cl-who:htm
                          (:option :value st :selected (equal st (getf wo :|status|))
                                   (cl-who:str st))))))
                   (:div :class "form-group"
                     (:label "Completion Notes")
                     (:textarea :name "notes" :rows "2" :placeholder "Optional notes..."))
                   (:button :type "submit" :class "btn btn-primary" "Update Status")))
               (:section :class "card"
                 (:h2 "Location")
                 (:dl :class "detail-list"
                   (:dt "Site") (:dd (cl-who:fmt "~A" (or (getf wo :|site_name|) "-")))
                   (:dt "Site Code") (:dd (cl-who:str (or (getf wo :|site_code|) "-")))
                   (:dt "Building") (:dd (cl-who:str (or (getf wo :|location_details|) "-")))))
               (:section :class "card"
                 (:h2 "Inspection Details")
                 (:dl :class "detail-list"
                   (:dt "Work Type") (:dd (cl-who:str (or (getf wo :|work_type|) "-")))
                   (:dt "Priority") (:dd (cl-who:str (or (getf wo :|priority|) "-")))
                   (:dt "Notes") (:dd (cl-who:str (or (getf wo :|work_instructions|) "-"))))))
             (:section :class "card activity-section"
               (:h2 "Activity History")
               (:p :class "help-text" "Track re-inspections, call-outs, and follow-up actions.")
               (:form :method "post" :action (format nil "/api/work-orders/~A/activity" wo-id)
                      :class "activity-form"
                 (:div :class "form-row"
                   (:div :class "form-group required"
                     (:label "Activity Type")
                     (:select :name "activity_type" :required t
                       (dolist (at *activity-types*)
                         (cl-who:htm (:option :value at (cl-who:str at))))))
                   (:div :class "form-group required"
                     (:label "Date")
                     (:input :type "date" :name "activity_date" :required t))
                   (:div :class "form-group"
                     (:label "Performed By")
                     (:input :type "text" :name "performed_by" :placeholder "Team/Person")))
                 (:div :class "form-group"
                   (:label "Description/Notes")
                   (:textarea :name "description" :rows "2" :placeholder "Details of the activity..."))
                 (:button :type "submit" :class "btn btn-primary" "Add Activity"))
               (if activities
                   (cl-who:htm
                    (:table :class "data-table activity-table"
                      (:thead
                        (:tr
                          (:th "Date")
                          (:th "Type")
                          (:th "Performed By")
                          (:th "Description")
                          (:th "Actions")))
                      (:tbody
                        (dolist (act activities)
                          (cl-who:htm
                           (:tr
                             (:td (cl-who:str (format-date-display (getf act :|activity_date|))))
                             (:td (:span :class "badge" (cl-who:str (or (getf act :|activity_type|) "-"))))
                             (:td (cl-who:str (or (getf act :|performed_by|) "-")))
                             (:td (cl-who:str (or (getf act :|description|) "-")))
                             (:td 
                               (:form :method "post" 
                                      :action (format nil "/api/activity/~A/delete" (getf act :|id|))
                                      :style "display:inline;"
                                      :onsubmit "return confirm('Delete this activity?');"
                                 (:input :type "hidden" :name "wo_id" :value (princ-to-string wo-id))
                                 (:button :type "submit" :class "btn btn-sm btn-danger" "Delete")))))))))
                   (cl-who:htm
                    (:p :class "empty-state" "No activities recorded yet."))))
             ;; Inspection Reports Section
             (let ((reports (get-reports-for-work-order wo-id)))
               (cl-who:htm
                (:section :class "card"
                  (:h2 "Inspection Reports")
                  (:div :class "page-actions" :style "margin-bottom: 1rem;"
                    (:a :href (format nil "/inspection-reports/new?wo_id=~A" wo-id) 
                        :class "btn btn-primary" "Create Inspection Report"))
                  (if reports
                      (cl-who:htm
                       (:table :class "data-table"
                         (:thead
                           (:tr
                             (:th "Report #")
                             (:th "Phase")
                             (:th "Date")
                             (:th "Status")
                             (:th "Actions")))
                         (:tbody
                           (dolist (rpt reports)
                             (cl-who:htm
                              (:tr
                                (:td (cl-who:str (getf rpt :|report_number|)))
                                (:td (cl-who:str (getf rpt :|inspection_phase|)))
                                (:td (cl-who:str (format-date-display (getf rpt :|inspection_date|))))
                                (:td (:span :class "status-badge" (cl-who:str (getf rpt :|status|))))
                                (:td 
                                  (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                                      :class "btn btn-sm" "View/Edit"))))))))
                      (cl-who:htm
                       (:p :class "empty-state" "No inspection reports yet."))))))
             (:div :class "page-actions"
               (:a :href "/work-orders" :class "btn" "Back to List")))))
        (html-response
         (render-page "Not Found"
           (cl-who:with-html-output-to-string (s)
             (:div :class "empty-state"
               (:h1 "Work Order Not Found")
               (:a :href "/work-orders" :class "btn" "Back to List"))))))))

(defun handle-work-order-new ()
  "New work order form."
  (let ((sites (list-sites)))
    (html-response
     (render-page "New Work Order"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Create New Work Order"))
         (:form :method "post" :action "/api/work-orders/create" :class "form-card"
           (:div :class "form-section"
             (:h2 "Location")
             (:div :class "form-row"
               (:div :class "form-group required"
                 (:label "Site")
                 (:select :name "site_id" :required t
                   (:option :value "" "Select Site...")
                   (dolist (site sites)
                     (cl-who:htm
                      (:option :value (princ-to-string (getf site :|id|))
                               (cl-who:fmt "~A - ~A" (getf site :|code|) (getf site :|name|))))))))
             (:div :class "form-row"
               (:div :class "form-group"
                 (:label "Building Number/Name")
                 (:input :type "text" :name "building_name" :placeholder "e.g., B-101, HQ Building"))
               (:div :class "form-group"
                 (:label "Building Type")
                 (:select :name "building_type"
                   (:option :value "" "Select Type...")
                   (dolist (bt *building-types*)
                     (cl-who:htm (:option :value bt (cl-who:str bt))))))))
           (:div :class "form-section"
             (:h2 "Work Classification")
             (:div :class "form-row"
               (:div :class "form-group required"
                 (:label "Work Type")
                 (:select :name "work_type" :required t
                   (dolist (wt *work-types*)
                     (cl-who:htm (:option :value wt (cl-who:str wt))))))
               (:div :class "form-group required"
                 (:label "Priority")
                 (:select :name "priority" :required t
                   (dolist (p *priorities*)
                     (cl-who:htm (:option :value p (cl-who:str p))))))))
           (:div :class "form-section"
             (:h2 "Scheduling")
             (:div :class "form-row"
               (:div :class "form-group"
                 (:label "Scheduled Date")
                 (:input :type "date" :name "target_start"))
               (:div :class "form-group"
                 (:label "Assigned Team")
                 (:input :type "text" :name "assigned_to" :placeholder "Team name or lead")))
             (:div :class "form-group"
               (:label "Reference / Notes")
               (:textarea :name "work_instructions" :rows "3" 
                          :placeholder "Any reference notes for this inspection...")))
           (:div :class "form-actions"
             (:a :href "/work-orders" :class "btn" "Cancel")
             (:button :type "submit" :class "btn btn-primary" "Create Work Order"))))))))

(defun handle-sites ()
  "Sites management page."
  (let ((sites (list-sites)))
    (html-response
     (render-page "Sites"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Sites"))
         (:div :class "card"
           (:h2 "Add New Site")
           (:form :method "post" :action "/api/sites/create" :class "inline-form"
             (:div :class "form-group"
               (:label "Code")
               (:input :type "text" :name "code" :required t :placeholder "AJ" :maxlength "10"))
             (:div :class "form-group"
               (:label "Name")
               (:input :type "text" :name "name" :required t :placeholder "Al Jaber Air Base"))
             (:button :type "submit" :class "btn btn-primary" "Add Site")))
         (:table :class "data-table"
           (:thead
             (:tr (:th "Code") (:th "Name") (:th "Created") (:th "Actions")))
           (:tbody
             (if sites
                 (dolist (site sites)
                   (cl-who:htm
                    (:tr
                      (:td (cl-who:str (getf site :|code|)))
                      (:td (cl-who:str (getf site :|name|)))
                      (:td (cl-who:str (subseq (or (getf site :|created_at|) "") 0 
                                               (min 10 (length (or (getf site :|created_at|) ""))))))
                      (:td 
                        (:a :href (format nil "/work-orders?site=~A" (getf site :|id|))
                            :class "btn btn-sm" "View WOs")
                        (:a :href (format nil "/reports?site=~A" (getf site :|id|))
                            :class "btn btn-success btn-sm" "Report")))))
                 (cl-who:htm
                  (:tr (:td :colspan "4" :class "empty-state" 
                            "No sites configured yet.")))))))))))

(defun handle-reports ()
  "Reports page with date range and status selection."
  (let* ((site-id (parse-int (get-param "site")))
         (date-from (get-param "date_from"))
         (date-to (get-param "date_to"))
         (status-filter (get-param "status"))
         (sites (list-sites)))
    (if (and site-id date-from date-to)
        ;; Generate detailed report with date range and optional status filter
        (let* ((site (get-site site-id))
               (work-orders (list-work-orders :site-id site-id 
                                              :date-from date-from 
                                              :date-to date-to
                                              :status (when (and status-filter 
                                                                 (> (length status-filter) 0))
                                                        status-filter)
                                              :limit 500))
               (total-count (length work-orders))
               (completed-count (count-if (lambda (wo) 
                                            (member (getf wo :|status|) 
                                                    '("Complete" "Closed") :test #'equal))
                                          work-orders))
               (initial-count (count-if (lambda (wo) 
                                          (equal (getf wo :|work_type|) "Initial Inspection"))
                                        work-orders))
               (reinspect-count (count-if (lambda (wo) 
                                            (equal (getf wo :|work_type|) "Re-inspection/Repair"))
                                          work-orders)))
          (html-response
           (render-page (format nil "Report - ~A" (getf site :|code|))
             (cl-who:with-html-output-to-string (s)
               (:style "
                 @page {
                   size: A4 portrait;
                   margin: 15mm 10mm 15mm 10mm;
                 }
                 @media print {
                   * { -webkit-print-color-adjust: exact !important; print-color-adjust: exact !important; }
                   body { font-size: 9pt; line-height: 1.3; }
                   .no-print { display: none !important; }
                   .navbar { display: none !important; }
                   .container { max-width: 100%; padding: 0; margin: 0; }
                   .card { box-shadow: none; border: none; padding: 0; margin-bottom: 10px; }
                   .report-logos { 
                     display: flex !important; 
                     position: running(header);
                     border-bottom: 2px solid #1a365d;
                     padding-bottom: 8px;
                     margin-bottom: 10px;
                   }
                   .report-logos img { max-height: 45px; }
                   .summary-grid { display: flex; gap: 15px; margin-bottom: 15px; }
                   .summary-card { 
                     border: 1px solid #ddd; 
                     padding: 8px 12px; 
                     text-align: center;
                     background: #f8f9fa;
                   }
                   .summary-value { font-size: 18pt; font-weight: bold; display: block; }
                   .summary-label { font-size: 8pt; color: #666; }
                   .report-table { 
                     font-size: 8pt; 
                     width: 100%; 
                     border-collapse: collapse;
                     page-break-inside: auto;
                   }
                   .report-table thead { 
                     display: table-header-group;
                   }
                   .report-table tr { 
                     page-break-inside: avoid; 
                     page-break-after: auto;
                   }
                   .report-table th { 
                     background: #1a365d !important; 
                     color: white !important; 
                     padding: 6px 4px;
                     text-align: left;
                     font-weight: 600;
                     border: 1px solid #1a365d;
                   }
                   .report-table td { 
                     padding: 4px; 
                     border: 1px solid #ddd;
                     vertical-align: top;
                   }
                   .report-table tbody tr:nth-child(even) { background: #f8f9fa; }
                   .report-section { margin-bottom: 15px; }
                   .report-section h2 { 
                     font-size: 11pt; 
                     color: #1a365d; 
                     border-bottom: 1px solid #1a365d;
                     padding-bottom: 3px;
                     margin-bottom: 8px;
                   }
                   .report-meta p { margin: 2px 0; font-size: 9pt; }
                   .page-actions { display: none; }
                 }
                 .report-logos {
                   display: flex;
                   justify-content: space-between;
                   align-items: center;
                   padding: 1rem;
                   border-bottom: 2px solid #1a365d;
                   margin-bottom: 1rem;
                   background: white;
                 }
                 .report-logos img {
                   max-height: 60px;
                   width: auto;
                 }
                 .report-title-center {
                   text-align: center;
                   flex: 1;
                 }
                 .report-title-center h1 {
                   margin: 0;
                   color: #1a365d;
                   font-size: 1.5rem;
                 }
                 .report-title-center p {
                   margin: 0.25rem 0 0 0;
                   color: #666;
                   font-size: 0.875rem;
                 }
               ")
               (:div :class "report-logos"
                 (:img :src "/static/img/company_logo.png" :alt "Versar Global Solutions")
                 (:div :class "report-title-center"
                   (:h1 "Inspection Schedule Report")
                   (:p "Task Force SAFE - CENTCOM AOR"))
                 (:img :src "/static/img/TFS_Logo.png" :alt "Task Force SAFE"))
               (:div :class "no-print" :style "margin-bottom: 1rem;"
                 (:button :onclick "window.print()" :class "btn btn-primary" "Print / Save as PDF"))
               (:div :class "report-header card"
                 (:div :class "report-meta"
                   (:p (:strong "Site: ") (cl-who:fmt "~A (~A)" 
                                                     (getf site :|name|)
                                                     (getf site :|code|)))
                   (:p (:strong "Report Period: ") (cl-who:fmt "~A to ~A" date-from date-to))
                   (when (and status-filter (> (length status-filter) 0))
                     (cl-who:htm
                      (:p (:strong "Status Filter: ") (cl-who:str status-filter))))
                   (:p (:strong "Generated: ") (cl-who:str (local-time:format-timestring 
                                                            nil (local-time:now) 
                                                            :format '(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))))))
               (:section :class "report-section"
                 (:h2 "Summary")
                 (:div :class "summary-grid"
                   (:div :class "summary-card"
                     (:span :class "summary-value" (cl-who:str total-count))
                     (:span :class "summary-label" "Total Inspections"))
                   (:div :class "summary-card"
                     (:span :class "summary-value" (cl-who:str completed-count))
                     (:span :class "summary-label" "Completed"))
                   (:div :class "summary-card"
                     (:span :class "summary-value" (cl-who:str initial-count))
                     (:span :class "summary-label" "Initial Inspections"))
                   (:div :class "summary-card"
                     (:span :class "summary-value" (cl-who:str reinspect-count))
                     (:span :class "summary-label" "Re-inspections"))))
               (:section :class "report-section"
                 (:h2 "Detailed Inspection Schedule")
                 (:table :class "data-table report-table"
                   (:thead
                     (:tr
                       (:th "WO Number")
                       (:th "Date")
                       (:th "Building")
                       (:th "Work Type")
                       (:th "Team")
                       (:th "Priority")
                       (:th "Status")))
                   (:tbody
                     (if work-orders
                         (dolist (wo work-orders)
                           (cl-who:htm
                            (:tr
                              (:td (cl-who:str (getf wo :|wo_number|)))
                              (:td (cl-who:str (subseq (or (getf wo :|created_at|) "") 0 
                                                       (min 10 (length (or (getf wo :|created_at|) ""))))))
                              (:td (cl-who:str (or (getf wo :|location_details|) "-")))
                              (:td (cl-who:str (or (getf wo :|work_type|) "-")))
                              (:td (cl-who:str (or (getf wo :|assigned_to|) "-")))
                              (:td (cl-who:str (or (getf wo :|priority|) "-")))
                              (:td (cl-who:str (or (getf wo :|status|) "-"))))))
                         (cl-who:htm
                          (:tr (:td :colspan "7" :class "empty-state" 
                                    "No inspections found for this date range.")))))))
               (:div :class "page-actions no-print"
                 (:a :href "/reports" :class "btn" "Back to Reports"))))))
        ;; Show report selection form
        (html-response
         (render-page "Reports"
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 "Inspection Reports"))
             (:div :class "card"
               (:h2 "Generate Inspection Report")
               (:p "Select a site and date range to generate a detailed inspection report.")
               (:form :method "get" :action "/reports" :class "report-form"
                 (:div :class "form-row"
                   (:div :class "form-group required"
                     (:label "Site")
                     (:select :name "site" :required t
                       (:option :value "" "Select Site...")
                       (dolist (site sites)
                         (cl-who:htm
                          (:option :value (princ-to-string (getf site :|id|))
                                   :selected (eql (getf site :|id|) site-id)
                                   (cl-who:fmt "~A - ~A" (getf site :|code|) (getf site :|name|))))))))
                 (:div :class "form-row"
                   (:div :class "form-group required"
                     (:label "From Date")
                     (:input :type "date" :name "date_from" :required t))
                   (:div :class "form-group required"
                     (:label "To Date")
                     (:input :type "date" :name "date_to" :required t))
                   (:div :class "form-group"
                     (:label "Status (optional)")
                     (:select :name "status"
                       (:option :value "" "All Statuses")
                       (dolist (st *wo-statuses*)
                         (cl-who:htm
                          (:option :value st (cl-who:str st)))))))
                 (:div :class "form-actions"
                   (:button :type "submit" :class "btn btn-primary" "Generate Report"))))))))))

;;; API handlers

(defun handle-api-sites-create ()
  "Create a new site."
  (let ((code (get-param "code"))
        (name (get-param "name")))
    (if (and code name (> (length code) 0) (> (length name) 0))
        (let ((id (create-site code name)))
          (redirect-to "/sites"))
        (json-response (list :error "Code and name are required")))))

(defun handle-api-work-orders-create ()
  "Create a new work order."
  (let ((site-id (parse-int (get-param "site_id")))
        (work-type (get-param "work_type"))
        (priority (get-param "priority"))
        (building-name (get-param "building_name"))
        (building-type (get-param "building_type")))
    (if (and site-id work-type priority)
        (multiple-value-bind (wo-id wo-number)
            (create-work-order site-id work-type priority
                               :work-instructions (get-param "work_instructions")
                               :target-start (get-param "target_start")
                               :assigned-to (get-param "assigned_to")
                               :location-details (format nil "~@[~A~]~@[ (~A)~]" building-name building-type))
          (redirect-to (format nil "/work-orders/~A" wo-id)))
        (json-response (list :error "Site, work type, and priority are required")))))

(defun handle-api-work-orders-update (id)
  "Update a work order status."
  (let ((wo-id (parse-int id))
        (status (get-param "status"))
        (notes (get-param "notes")))
    (when (and wo-id status)
      (update-work-order wo-id :status status :next-action notes))
    (redirect-to (format nil "/work-orders/~A" wo-id))))

(defun handle-api-work-orders-activity (id)
  "Add an activity to a work order."
  (let ((wo-id (parse-int id))
        (activity-type (get-param "activity_type"))
        (activity-date (get-param "activity_date"))
        (performed-by (get-param "performed_by"))
        (description (get-param "description")))
    (when (and wo-id activity-type activity-date)
      (add-wo-activity wo-id activity-type activity-date 
                       :performed-by performed-by 
                       :description description))
    (redirect-to (format nil "/work-orders/~A" wo-id))))

(defun handle-api-activity-delete (activity-id wo-id)
  "Delete an activity from a work order."
  (let ((act-id (parse-int activity-id))
        (work-order-id (parse-int wo-id)))
    (when act-id
      (delete-wo-activity act-id))
    (redirect-to (format nil "/work-orders/~A" work-order-id))))

;;; Inspection Report Handlers

(defun handle-inspection-reports-list ()
  "List all inspection reports with optional status filter."
  (let* ((user (get-current-user))
         (status-filter (get-param "status"))
         (show-assigned (and user (or (user-is-admin-p user)
                                      (string= (string-downcase (or (getf user :|role|) "")) "qc_manager"))))
         (base-query "SELECT r.*, s.name as site_name, s.code as site_code, u.full_name as assigned_to_name
                      FROM inspection_reports r
                      JOIN sites s ON r.site_id = s.id
                      LEFT JOIN users u ON r.assigned_qc_id = u.id")
         (reports (cond
                    ((and status-filter (string= status-filter "rejected"))
                     (fetch-all (concatenate 'string base-query 
                                " WHERE r.rejection_count > 0 ORDER BY r.rejection_count DESC, r.created_at DESC LIMIT 100")))
                    ((and status-filter (> (length status-filter) 0))
                     (fetch-all (concatenate 'string base-query 
                                " WHERE r.status = ? ORDER BY r.created_at DESC LIMIT 100") status-filter))
                    (t (fetch-all (concatenate 'string base-query 
                                  " ORDER BY r.created_at DESC LIMIT 100"))))))
    (html-response
     (render-page "Inspection Reports"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Inspection Reports"))
         ;; Status filter form
         (:div :class "filter-bar"
           (:form :method "get" :action "/inspection-reports" :class "filter-form"
             (:label :for "status" "Filter by Status:")
             (:select :name "status" :id "status" :onchange "this.form.submit()"
               (:option :value "" (cl-who:str (if (or (null status-filter) (string= status-filter "")) "All Statuses ▼" "All Statuses")))
               (:option :value "Draft" :selected (when (and status-filter (string= status-filter "Draft")) "selected") "Draft")
               (:option :value "Pending QC" :selected (when (and status-filter (string= status-filter "Pending QC")) "selected") "Pending QC")
               (:option :value "QC Rejected" :selected (when (and status-filter (string= status-filter "QC Rejected")) "selected") "QC Rejected")
               (:option :value "QC Approved" :selected (when (and status-filter (string= status-filter "QC Approved")) "selected") "QC Approved")
               (:option :value "Complete" :selected (when (and status-filter (string= status-filter "Complete")) "selected") "Complete")
               (:option :value "rejected" :selected (when (and status-filter (string= status-filter "rejected")) "selected") "⚠ Ever Rejected (Return Rate)"))))
         (:table :class "data-table"
           (:thead
             (:tr
               (:th "Report #")
               (:th "Site")
               (:th "Building")
               (:th "Phase")
               (:th "Date")
               (:th "Status")
               (:th "Rejections")
               (when show-assigned
                 (cl-who:htm (:th "Assigned To")))
               (:th "Actions")))
           (:tbody
             (if reports
                 (dolist (rpt reports)
                   (cl-who:htm
                    (:tr
                      (:td (cl-who:str (getf rpt :|report_number|)))
                      (:td (cl-who:str (format nil "~A (~A)" 
                                               (or (getf rpt :|site_name|) "Unknown")
                                               (or (getf rpt :|site_code|) "-"))))
                      (:td (cl-who:str (getf rpt :|building_number|)))
                      (:td (cl-who:str (getf rpt :|inspection_phase|)))
                      (:td (cl-who:str (format-date-display (getf rpt :|inspection_date|))))
                      (:td (:span :class "status-badge" (cl-who:str (getf rpt :|status|))))
                      (:td 
                        (let ((rej-count (or (getf rpt :|rejection_count|) 0)))
                          (if (> rej-count 0)
                              (cl-who:htm (:span :class "rejection-badge" (cl-who:str rej-count)))
                              (cl-who:htm (cl-who:str "-")))))
                      (when show-assigned
                        (cl-who:htm 
                         (:td (cl-who:str (or (getf rpt :|assigned_to_name|) "-")))))
                      (:td 
                        (:a :href (format nil "/inspection-reports/~A" (getf rpt :|id|))
                            :class "btn btn-sm" "View")))))
                 (cl-who:htm
                  (:tr (:td :colspan (if show-assigned "9" "8") :class "empty-state" "No inspection reports found.")))))))))))

(defun handle-inspection-report-new ()
  "New inspection report form."
  (let* ((wo-id (parse-int (get-param "wo_id")))
         (wo (when wo-id (get-work-order wo-id)))
         (sites (list-sites)))
    (html-response
     (render-page "New Inspection Report"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Create Inspection Report")
           (when wo
             (cl-who:htm
              (:p "For Work Order: " (:strong (cl-who:str (getf wo :|wo_number|)))))))
         (:form :method "post" :action "/api/inspection-reports/create" :class "form-card"
           (when wo-id
             (cl-who:htm
              (:input :type "hidden" :name "wo_id" :value (princ-to-string wo-id))))
           (:div :class "form-row"
             (:div :class "form-group required"
               (:label "Site")
               (:select :name "site_id" :id "site-select" :required t
                 (:option :value "" "-- Select Site --")
                 (dolist (site sites)
                   (let ((selected (and wo (= (getf site :|id|) (getf wo :|site_id|)))))
                     (cl-who:htm
                      (:option :value (princ-to-string (getf site :|id|))
                               :selected selected
                               (cl-who:str (format nil "~A (~A)" 
                                                   (getf site :|name|) 
                                                   (getf site :|code|)))))))))
             (:div :class "form-group required"
               (:label "Building Number")
               (:input :type "text" :name "building_number" :required t
                       :placeholder "e.g., 11200, GEN-1")))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "Building Type")
               (:select :name "building_type"
                 (:option :value "" "-- Select --")
                 (dolist (bt *building-types*)
                   (cl-who:htm (:option :value bt (cl-who:str bt))))))
             (:div :class "form-group required"
               (:label "System Voltage")
               (:select :name "system_voltage" :id "voltage-select" :required t
                 (dolist (v *system-voltages*)
                   (cl-who:htm 
                    (:option :value (car v) 
                             (cl-who:str (format nil "~A (~A)" (car v) (cdr v)))))))))
           (:div :class "form-row"
             (:div :class "form-group required"
               (:label "Inspection Phase")
               (:select :name "inspection_phase" :required t
                 (dolist (phase *inspection-phases*)
                   (cl-who:htm (:option :value phase (cl-who:str phase))))))
             (:div :class "form-group required"
               (:label "Team Number")
               (:input :type "text" :name "team_number" :required t
                       :placeholder "e.g., 102"
                       :value (or (when wo (getf wo :|assigned_to|)) ""))))
           (:div :class "form-row"
             (:div :class "form-group required"
               (:label "Inspection Date")
               (:input :type "date" :name "inspection_date" :required t))
             (:div :class "form-group"
               (:label "Previous Report # (if Re-inspection)")
               (:input :type "text" :name "previous_report" :placeholder "Leave blank for initial")))
           (:div :class "form-group"
             (:label "Physical Building Location Description")
             (:textarea :name "location_description" :rows "2" 
                        :placeholder "e.g., Mayor cell building, near main gate..."))
           (:div :class "form-actions"
             (:button :type "submit" :class "btn btn-primary" "Create Report")
             (:a :href (if wo-id (format nil "/work-orders/~A" wo-id) "/work-orders")
                 :class "btn" "Cancel"))))))))

(defun handle-inspection-report-history (id)
  "View rejection history for a report."
  (let* ((report-id (parse-int id))
         (report (get-inspection-report report-id))
         (history (fetch-all 
                   "SELECT * FROM rejection_history WHERE report_id = ? ORDER BY rejection_number DESC"
                   report-id)))
    (if report
        (html-response
         (render-page (format nil "Rejection History: ~A" (getf report :|report_number|))
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 "Rejection History")
               (:p "Report: " (:strong (cl-who:str (getf report :|report_number|)))))
             (:section :class "card"
               (:h2 (cl-who:str (format nil "~A Rejection~:P" (or (getf report :|rejection_count|) 0))))
               (if history
                   (cl-who:htm
                    (:table :class "data-table"
                      (:thead
                        (:tr (:th "#") (:th "Team") (:th "Submitted") 
                             (:th "Rejected") (:th "QC Reviewer") (:th "Comments")))
                      (:tbody
                        (dolist (h history)
                          (cl-who:htm
                           (:tr
                             (:td (cl-who:str (getf h :|rejection_number|)))
                             (:td (cl-who:str (or (getf h :|team_number|) "-")))
                             (:td (cl-who:str (or (getf h :|submitted_at|) "-")))
                             (:td (cl-who:str (or (getf h :|rejected_at|) "-")))
                             (:td (cl-who:str (or (getf h :|qc_name|) "-")))
                             (:td (cl-who:str (or (getf h :|qc_comments|) "-")))))))))
                   (cl-who:htm
                    (:p :class "empty-state" "No rejection history recorded yet.")
                    (:p :class "text-muted" "Note: History is only recorded for rejections after this feature was added."))))
             (:div :class "page-actions"
               (:a :href (format nil "/inspection-reports/~A" report-id) :class "btn" "Back to Report")
               (:a :href "/admin/reports" :class "btn" "Reports History")))))
        (redirect-to "/inspection-reports"))))

(defun handle-inspection-report-detail (id)
  "View/edit an inspection report."
  (let* ((report-id (parse-int id))
         (report (get-inspection-report report-id))
         (deficiencies (get-report-deficiencies report-id))
         (voltage (getf report :|system_voltage|))
         (code-source (get-code-source voltage))
         (code-refs (if (string= code-source "NEC") *nec-code-references* *bs7671-code-references*)))
    (if report
        (html-response
         (render-page (format nil "Report: ~A" (getf report :|report_number|))
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 (cl-who:str (getf report :|report_number|)))
               (:span :class (format nil "badge badge-~A" 
                                     (string-downcase (substitute #\- #\Space (getf report :|status|))))
                      (cl-who:str (getf report :|status|))))
             
             ;; Report Details
             (:div :class "wo-detail-grid"
               (:section :class "card"
                 (:h2 "Report Details")
                 (:dl :class "detail-list"
                   (:dt "TAG-ID") (:dd (cl-who:str (getf report :|tag_id|)))
                   (:dt "Work Order") (:dd (cl-who:str (getf report :|work_order_number|)))
                   (:dt "Site") (:dd (cl-who:str (format nil "~A (~A)" 
                                                         (getf report :|site_name|)
                                                         (getf report :|site_code|))))
                   (:dt "Building") (:dd (cl-who:str (getf report :|building_number|)))
                   (:dt "Building Type") (:dd (cl-who:str (or (getf report :|building_type|) "-")))
                   (:dt "System Voltage") (:dd (cl-who:str (getf report :|system_voltage|)))
                   (:dt "Code Standard") (:dd (cl-who:str code-source))))
               
               (:section :class "card"
                 (:h2 "Inspection Info")
                 (:dl :class "detail-list"
                   (:dt "Phase") (:dd (cl-who:str (getf report :|inspection_phase|)))
                   (:dt "Date") (:dd (cl-who:str (format-date-display (getf report :|inspection_date|))))
                   (:dt "Team") (:dd (cl-who:str (getf report :|team_number|)))
                   (:dt "Overall Rating") (:dd (cl-who:str (or (getf report :|overall_rating|) "Not set")))
                   (:dt "Location") (:dd (cl-who:str (or (getf report :|location_description|) "-"))))))
             
             ;; Building Image Section
             (:section :class "card"
               (:h2 "Building Picture")
               (if (getf report :|building_image_path|)
                   (cl-who:htm
                    (:div :class "building-image-container"
                      (:a :href (format nil "/uploads/~A" (getf report :|building_image_path|))
                          :target "_blank"
                          (:img :src (format nil "/uploads/~A" (getf report :|building_image_path|))
                                :class "building-image"
                                :alt "Building photo"))))
                   (cl-who:htm
                    (:p :class "text-muted" "No building image uploaded yet.")))
               (when (member (getf report :|status|) '("Draft" "QC Rejected") :test #'string=)
                 (cl-who:htm
                  (:form :method "post" :action (format nil "/api/inspection-reports/~A/building-image" report-id)
                         :enctype "multipart/form-data" :style "margin-top: 1rem;"
                    (:div :class "form-group"
                      (:label "Upload Building Image")
                      (:input :type "file" :name "building_image" :accept "image/*"))
                    (:button :type "submit" :class "btn" "Upload Image")))))
             
             ;; Update Report Form (if Draft or QC Rejected)
             (when (member (getf report :|status|) '("Draft" "QC Rejected") :test #'string=)
               (cl-who:htm
                (:section :class "card"
                  (:h2 "Update Report")
                  (:form :method "post" :action (format nil "/api/inspection-reports/~A/update" report-id)
                    (:div :class "form-row"
                      (:div :class "form-group"
                        (:label "Inspection Date")
                        (:input :type "date" :name "inspection_date" 
                                :value (or (getf report :|inspection_date|) "")))
                      (:div :class "form-group"
                        (:label "Team Number")
                        (:input :type "text" :name "team_number" 
                                :value (or (getf report :|team_number|) "")
                                :placeholder "e.g., Team 102")))
                    (:div :class "form-row"
                      (:div :class "form-group"
                        (:label "Building Type")
                        (:select :name "building_type"
                          (:option :value "" "-- Select --")
                          (dolist (bt '("Administrative" "Barracks" "Dining Facility" "Maintenance" 
                                        "Storage" "Medical" "Recreation" "Communications" "Power Generation" "Other"))
                            (cl-who:htm
                             (:option :value bt
                                      :selected (string= bt (getf report :|building_type|))
                                      (cl-who:str bt))))))
                      (:div :class "form-group"
                        (:label "Building Number")
                        (:input :type "text" :name "building_number"
                                :value (or (getf report :|building_number|) ""))))
                    (:div :class "form-row"
                      (:div :class "form-group"
                        (:label "Overall Rating")
                        (:select :name "overall_rating"
                          (:option :value "" "-- Select --")
                          (dolist (r *overall-ratings*)
                            (cl-who:htm 
                             (:option :value r 
                                      :selected (string= r (getf report :|overall_rating|))
                                      (cl-who:str r))))))
                      (:div :class "form-group"
                        (:label "MRF Needed?")
                        (:select :name "mrf_needed"
                          (:option :value "No" :selected (string= "No" (getf report :|mrf_needed|)) "No")
                          (:option :value "Yes" :selected (string= "Yes" (getf report :|mrf_needed|)) "Yes"))))
                    (:div :class "form-group"
                      (:label "Location Description")
                      (:textarea :name "location_description" :rows "2"
                                 (cl-who:str (or (getf report :|location_description|) ""))))
                    (:div :class "form-group"
                      (:label "Summary of Findings")
                      (:textarea :name "summary_of_findings" :rows "3"
                                 (cl-who:str (or (getf report :|summary_of_findings|) ""))))
                    (:button :type "submit" :class "btn btn-primary" "Save Changes")))))
             
             ;; Deficiencies Section
             (:section :class "card"
               (:h2 (cl-who:str (format nil "Deficiencies (~A)" (length deficiencies))))
               (when (member (getf report :|status|) '("Draft" "QC Rejected") :test #'string=)
                 (cl-who:htm
                  (:div :class "page-actions" :style "margin-bottom: 1rem;"
                    (:a :href (format nil "/inspection-reports/~A/deficiency/new" report-id)
                        :class "btn btn-primary" "Add Deficiency"))))
               (if deficiencies
                   (cl-who:htm
                    (:table :class "data-table"
                      (:thead
                        (:tr
                          (:th "#")
                          (:th "RAC")
                          (:th "Location")
                          (:th "Category")
                          (:th "Status")
                          (:th "Actions")))
                      (:tbody
                        (dolist (def deficiencies)
                          (cl-who:htm
                           (:tr
                             (:td (cl-who:str (getf def :|deficiency_number|)))
                             (:td (:span :class (format nil "badge rac-~A" (getf def :|rac_score|))
                                         (cl-who:str (format nil "RAC ~A" (getf def :|rac_score|)))))
                             (:td 
                               (cl-who:str (or (getf def :|location_description|) "-"))
                               (when (getf def :|image_path|)
                                 (cl-who:htm
                                  (:br)
                                  (:a :href (format nil "/uploads/~A" (getf def :|image_path|))
                                      :target "_blank"
                                      (:img :src (format nil "/uploads/~A" (getf def :|image_path|))
                                            :class "deficiency-thumbnail"
                                            :alt "Deficiency photo")))))
                             (:td (cl-who:str (or (getf def :|deficiency_category|) "-")))
                             (:td (cl-who:str (or (getf def :|deficiency_status|) "-")))
                             (:td
                               (when (member (getf report :|status|) '("Draft" "QC Rejected") :test #'string=)
                                 (cl-who:htm
                                  (:a :href (format nil "/deficiency/~A/edit" (getf def :|id|))
                                      :class "btn btn-sm" "Edit")
                                  " "
                                  (:form :method "post" 
                                         :action (format nil "/api/deficiency/~A/delete" (getf def :|id|))
                                         :style "display:inline;"
                                         :onsubmit "return confirm('Delete this deficiency?');"
                                    (:input :type "hidden" :name "report_id" :value (princ-to-string report-id))
                                    (:button :type "submit" :class "btn btn-sm btn-danger" "Delete")))))))))))
                   (cl-who:htm
                    (:p :class "empty-state" "No deficiencies recorded yet."))))
             
             ;; MRF Section (if MRF Required = Yes)
             (when (string= "Yes" (getf report :|mrf_needed|))
               (let ((mrf (get-mrf-by-report report-id)))
                 (cl-who:htm
                  (:section :class "card"
                    (:h2 "Material Request Form (MRF)")
                    (if mrf
                        (cl-who:htm
                         (:dl :class "detail-list"
                           (:dt "MRF Number") (:dd (cl-who:str (getf mrf :|mrf_number|)))
                           (:dt "Status") (:dd (:span :class (format nil "badge badge-~A" 
                                                                      (string-downcase (getf mrf :|status|)))
                                                      (cl-who:str (getf mrf :|status|))))
                           (:dt "Items") (:dd (cl-who:str (format nil "~A item(s)" 
                                                                   (or (getf mrf :|item_count|) 0)))))
                         (:div :class "page-actions" :style "margin-top: 1rem;"
                           (:a :href (format nil "/mrf/~A" (getf mrf :|id|))
                               :class "btn btn-primary" "View/Edit MRF")
                           (:a :href (format nil "/mrf/~A/print" (getf mrf :|id|))
                               :class "btn" :target "_blank" "Print MRF")))
                        (cl-who:htm
                         (:p :class "text-muted" "MRF not yet generated.")
                         (when (member (getf report :|status|) '("Draft" "QC Rejected") :test #'string=)
                           (cl-who:htm
                            (:form :method "post" :action (format nil "/api/inspection-reports/~A/generate-mrf" report-id)
                              (:button :type "submit" :class "btn btn-primary" "Generate MRF"))))))))))
             
             ;; QC Section
             (:section :class "card"
               (:h2 "Signatures & QC")
               (cond
                 ;; Draft - can submit for QC
                 ((string= (getf report :|status|) "Draft")
                  (cl-who:htm
                   (:form :method "post" :action (format nil "/api/inspection-reports/~A/submit" report-id)
                     (:div :class "form-row"
                       (:div :class "form-group required"
                         (:label "Inspector 1 Name")
                         (:input :type "text" :name "inspector1_name" :required t))
                       (:div :class "form-group"
                         (:label "Inspector 2 Name")
                         (:input :type "text" :name "inspector2_name")))
                     (:button :type "submit" :class "btn btn-primary" "Submit for QC Review"))))
                 
                 ;; Pending QC - QC can approve/reject
                 ((string= (getf report :|status|) "Pending QC")
                  (cl-who:htm
                   (:p "Inspector 1: " (:strong (cl-who:str (getf report :|inspector1_name|)))
                       " (signed " (cl-who:str (getf report :|inspector1_signed_at|)) ")")
                   (when (getf report :|inspector2_name|)
                     (cl-who:htm
                      (:p "Inspector 2: " (:strong (cl-who:str (getf report :|inspector2_name|)))
                          " (signed " (cl-who:str (getf report :|inspector2_signed_at|)) ")")))
                   (:hr)
                   (:form :method "post" :id "qc-form"
                     (:div :class "form-group required"
                       (:label "QC Name")
                       (:input :type "text" :name "qc_name" :required t
                               :value (getf (get-current-user) :|full_name|)))
                     (:div :class "form-group"
                       (:label "Comments")
                       (:textarea :name "qc_comments" :rows "2"))
                     (:div :class "form-actions"
                       (:button :type "submit" :class "btn btn-primary" 
                                :formaction (format nil "/api/inspection-reports/~A/qc-approve" report-id)
                                "Approve")
                       (:button :type "submit" :class "btn btn-danger" :style "margin-left: 0.5rem;"
                                :formaction (format nil "/api/inspection-reports/~A/qc-reject" report-id)
                                "Reject")))))
                 
                 ;; QC Rejected - show comments and allow resubmit
                 ((string= (getf report :|status|) "QC Rejected")
                  (let ((rejection-count (or (getf report :|rejection_count|) 0)))
                    (cl-who:htm
                     (:div :class "alert alert-danger"
                       (:strong "QC Rejected by: ") (cl-who:str (getf report :|qc_name|))
                       (when (> rejection-count 0)
                         (cl-who:htm
                          (:span :class "rejection-count" 
                                 (cl-who:str (format nil " (Rejection #~A)" rejection-count)))))
                       (:p "Comments: " (cl-who:str (or (getf report :|qc_comments|) "None."))))
                     (:p "Please address the issues above and resubmit.")
                     (:hr)
                     (:form :method "post" :action (format nil "/api/inspection-reports/~A/submit" report-id)
                       (:div :class "form-row"
                         (:div :class "form-group required"
                           (:label "Inspector 1 Name")
                           (:input :type "text" :name "inspector1_name" :required t
                                   :value (or (getf report :|inspector1_name|) "")))
                         (:div :class "form-group"
                           (:label "Inspector 2 Name")
                           (:input :type "text" :name "inspector2_name"
                                   :value (or (getf report :|inspector2_name|) ""))))
                       (:button :type "submit" :class "btn btn-primary" "Resubmit for QC Review")))))
                 
                 ;; QC Approved or Complete
                 (t
                  (cl-who:htm
                   (:p "Inspector 1: " (:strong (cl-who:str (getf report :|inspector1_name|))))
                   (when (getf report :|inspector2_name|)
                     (cl-who:htm
                      (:p "Inspector 2: " (:strong (cl-who:str (getf report :|inspector2_name|))))))
                   (:p "QC: " (:strong (cl-who:str (getf report :|qc_name|)))
                       " (approved " (cl-who:str (getf report :|qc_signed_at|)) ")")))))
             
             (:div :class "page-actions"
               (when (string= (getf report :|status|) "Complete")
                 (cl-who:htm
                  (:a :href (format nil "/api/inspection-reports/~A/generate-pdf" report-id)
                      :target "_blank"
                      :class "btn btn-primary" "Download PDF")
                  ;; Show Re-inspection button for completed Initial Inspections
                  (when (string= (getf report :|inspection_phase|) "Initial Inspection")
                    (cl-who:htm
                     (:a :href (format nil "/inspection-reports/~A/reinspection" report-id)
                         :class "btn btn-secondary" "Create Re-inspection")))))
               (:a :href (format nil "/work-orders/~A" (getf report :|wo_id|)) :class "btn" "Back to Work Order")
               (:a :href "/inspection-reports" :class "btn" "All Reports")))))
        (html-response
         (render-page "Not Found"
           (cl-who:with-html-output-to-string (s)
             (:div :class "empty-state"
               (:h1 "Report Not Found")
               (:a :href "/inspection-reports" :class "btn" "Back to Reports"))))))))

(defun handle-reinspection-form (original-report-id-str)
  "Form to create a re-inspection report from a completed initial inspection."
  (let* ((original-id (parse-int original-report-id-str))
         (original (get-inspection-report original-id))
         (today (multiple-value-bind (sec min hour day month year)
                    (decode-universal-time (get-universal-time))
                  (declare (ignore sec min hour))
                  (format nil "~4,'0D-~2,'0D-~2,'0D" year month day))))
    (if (and original (string= (getf original :|status|) "Complete"))
        (html-response
         (render-page "Create Re-inspection Report"
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 "Create Re-inspection Report"))
             (:section :class "card"
               (:h2 "Original Report")
               (:p "Report: " (:strong (cl-who:str (getf original :|report_number|))))
               (:p "Building: " (:strong (cl-who:str (getf original :|building_number|))))
               (:p "Site: " (:strong (cl-who:str (getf original :|site_name|))))
               (:p "Deficiencies: " (:strong (cl-who:str 
                                              (length (get-report-deficiencies original-id))))))
             (:section :class "card"
               (:h2 "Re-inspection Details")
               (:form :method "post" 
                      :action (format nil "/api/inspection-reports/~A/create-reinspection" original-id)
                 (:div :class "form-row"
                   (:div :class "form-group required"
                     (:label "Re-inspection Date")
                     (:input :type "date" :name "inspection_date" :required t :value today))
                   (:div :class "form-group required"
                     (:label "Team Number")
                     (:input :type "text" :name "team_number" :required t
                             :value (getf original :|team_number|)
                             :placeholder "e.g., 104")))
                 (:div :class "form-actions"
                   (:button :type "submit" :class "btn btn-primary" "Create Re-inspection Report")
                   (:a :href (format nil "/inspection-reports/~A" original-id) 
                       :class "btn" "Cancel")))))))
        (redirect-to "/inspection-reports"))))

;;; Master Tracker Handlers

(defun user-can-access-master-tracker-p (user)
  "Check if user can access Master Tracker (Admin, QC Manager, Program Manager, QC, AO Lead)."
  (when user
    (let ((role (string-downcase (or (getf user :|role|) ""))))
      (or (user-is-admin-p user)
          (string= role "qc_manager")
          (string= role "program_manager")
          (string= role "qc")
          (string= role "ao_lead")))))

(defun handle-master-tracker ()
  "Master Tracker dashboard with filtering by country and camp."
  (let ((user (get-current-user)))
    (unless (user-can-access-master-tracker-p user)
      (return-from handle-master-tracker (redirect-to "/unauthorized"))))
  (let* ((country-id (parse-int (get-param "country")))
         (camp-id (parse-int (get-param "camp")))
         (status-filter (get-param "status"))
         (date-from (get-param "date_from"))
         (date-to (get-param "date_to"))
         (countries (fetch-all "SELECT id, name FROM countries WHERE name != 'Unknown' ORDER BY name"))
         (camps (when country-id
                  (fetch-all "SELECT id, name FROM camps WHERE country_id = ? ORDER BY name" country-id)))
         ;; Build query based on filters
         (base-query "SELECT md.*, ca.name as camp_name, co.name as country_name
                      FROM master_deficiencies md
                      JOIN camps ca ON md.camp_id = ca.id
                      JOIN countries co ON ca.country_id = co.id
                      WHERE 1=1")
         (params nil))
    ;; Add filters
    (when country-id
      (setf base-query (concatenate 'string base-query " AND co.id = ?"))
      (push country-id params))
    (when camp-id
      (setf base-query (concatenate 'string base-query " AND ca.id = ?"))
      (push camp-id params))
    (when (and status-filter (> (length status-filter) 0))
      (setf base-query (concatenate 'string base-query " AND md.deficiency_status = ?"))
      (push status-filter params))
    (when (and date-from (> (length date-from) 0))
      (setf base-query (concatenate 'string base-query " AND md.inspection_date >= ?"))
      (push date-from params))
    (when (and date-to (> (length date-to) 0))
      (setf base-query (concatenate 'string base-query " AND md.inspection_date <= ?"))
      (push date-to params))
    ;; Add ordering and limit
    (setf base-query (concatenate 'string base-query " ORDER BY md.inspection_date DESC LIMIT 500"))
    ;; Get summary stats
    (let* ((stats-query "SELECT 
                          COUNT(*) as total,
                          SUM(CASE WHEN deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
                          SUM(CASE WHEN deficiency_status LIKE '%Closed%' OR deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_count,
                          COUNT(DISTINCT building_number) as buildings
                         FROM master_deficiencies md
                         JOIN camps ca ON md.camp_id = ca.id
                         JOIN countries co ON ca.country_id = co.id
                         WHERE 1=1")
           (stats-params nil))
      (when country-id
        (setf stats-query (concatenate 'string stats-query " AND co.id = ?"))
        (push country-id stats-params))
      (when camp-id
        (setf stats-query (concatenate 'string stats-query " AND ca.id = ?"))
        (push camp-id stats-params))
      (let* ((stats (apply #'fetch-one stats-query (reverse stats-params)))
             (deficiencies (apply #'fetch-all base-query (reverse params))))
        (html-response
         (render-page "Master Tracker"
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 "Master Tracker")
               (:a :href "/master-tracker/weekly-report" :class "btn btn-primary" "Generate Weekly Report"))
             
             ;; Filter form
             (:section :class "card"
               (:h2 "Filters")
               (:form :method "get" :action "/master-tracker" :class "filter-form"
                 (:div :class "form-row"
                   (:div :class "form-group"
                     (:label "Country")
                     (:select :name "country" :id "country-select" 
                              :onchange "this.form.submit()"
                       (:option :value "" "-- All Countries --")
                       (dolist (c countries)
                         (cl-who:htm
                          (:option :value (getf c :|id|)
                                   :selected (eql country-id (getf c :|id|))
                                   (cl-who:str (getf c :|name|)))))))
                   (:div :class "form-group"
                     (:label "Camp")
                     (:select :name "camp" :onchange "this.form.submit()"
                       (:option :value "" "-- All Camps --")
                       (dolist (ca camps)
                         (cl-who:htm
                          (:option :value (getf ca :|id|)
                                   :selected (eql camp-id (getf ca :|id|))
                                   (cl-who:str (getf ca :|name|)))))))
                   (:div :class "form-group"
                     (:label "Status")
                     (:select :name "status" :onchange "this.form.submit()"
                       (:option :value "" "-- All Statuses --")
                       (:option :value "Open" :selected (string= status-filter "Open") "Open")
                       (:option :value "Closed / Repaired" :selected (string= status-filter "Closed / Repaired") "Closed / Repaired"))))
                 (:div :class "form-row"
                   (:div :class "form-group"
                     (:label "Date From")
                     (:input :type "date" :name "date_from" :value (or date-from "")))
                   (:div :class "form-group"
                     (:label "Date To")
                     (:input :type "date" :name "date_to" :value (or date-to "")))
                   (:div :class "form-group" :style "align-self: flex-end;"
                     (:button :type "submit" :class "btn" "Apply Filters")))))
             
             ;; Stats summary
             (:section :class "card"
               (:h2 "Summary")
               (:div :class "dashboard-stats"
                 (:div :class "stat-card"
                   (:h3 "Total Deficiencies")
                   (:p :class "stat-number" (cl-who:str (or (getf stats :|total|) 0))))
                 (:div :class "stat-card"
                   (:h3 "Open")
                   (:p :class "stat-number" :style "color: #dc3545;" 
                       (cl-who:str (or (getf stats :|open_count|) 0))))
                 (:div :class "stat-card"
                   (:h3 "Closed/Repaired")
                   (:p :class "stat-number" :style "color: #28a745;"
                       (cl-who:str (or (getf stats :|closed_count|) 0))))
                 (:div :class "stat-card"
                   (:h3 "Buildings")
                   (:p :class "stat-number" (cl-who:str (or (getf stats :|buildings|) 0))))))
             
             ;; Data table
             (:section :class "card"
               (:h2 (cl-who:fmt "Deficiencies (~A shown, max 500)" (length deficiencies)))
               (if deficiencies
                   (cl-who:htm
                    (:div :style "overflow-x: auto;"
                      (:table :class "data-table"
                        (:thead
                          (:tr (:th "Date") (:th "Country") (:th "Camp") (:th "Building") 
                               (:th "Def #") (:th "Category") (:th "Status") (:th "Repair By")))
                        (:tbody
                          (dolist (d deficiencies)
                            (cl-who:htm
                             (:tr
                               (:td (cl-who:str (format-date-display (getf d :|inspection_date|))))
                               (:td (cl-who:str (getf d :|country_name|)))
                               (:td (cl-who:str (getf d :|camp_name|)))
                               (:td (cl-who:str (or (getf d :|building_number|) "-")))
                               (:td (cl-who:str (or (getf d :|deficiency_number|) "-")))
                               (:td (cl-who:str (or (getf d :|def_category|) "-")))
                               (:td (:span :class (if (string= (getf d :|deficiency_status|) "Open")
                                                      "badge badge-danger"
                                                      "badge badge-success")
                                           (cl-who:str (or (getf d :|deficiency_status|) "-"))))
                               (:td (cl-who:str (or (getf d :|repair_by|) "-"))))))))))
                   (cl-who:htm
                    (:p :class "empty-state" "No deficiencies found. Try adjusting filters.")))))))))))

(defun handle-weekly-report ()
  "Generate weekly report for client updates (Mon-Sat)."
  (let ((user (get-current-user)))
    (unless (user-can-access-master-tracker-p user)
      (return-from handle-weekly-report (redirect-to "/unauthorized"))))
  (let* ((week-start (get-param "week_start"))
         (week-end (get-param "week_end"))
         (generate-p (get-param "generate"))
         ;; Default to last week (Mon-Sat)
         (today (get-universal-time))
         (default-end (multiple-value-bind (sec min hour day month year dow)
                          (decode-universal-time today)
                        (declare (ignore sec min hour dow))
                        (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))
         (default-start (multiple-value-bind (sec min hour day month year dow)
                            (decode-universal-time (- today (* 6 24 60 60)))
                          (declare (ignore sec min hour dow))
                          (format nil "~4,'0D-~2,'0D-~2,'0D" year month day))))
    (let* ((start-date (or week-start default-start))
           (end-date (or week-end default-end))
           (report-dir (format nil "week_~A_to_~A" start-date end-date))
           (report-path (merge-pathnames (make-pathname :directory `(:relative "reports" ,report-dir))
                                         *base-directory*))
           (images-exist (probe-file (merge-pathnames "01_operational_updates.png" report-path)))
           (generated-now nil))
      ;; Generate report if requested
      (when generate-p
        (let ((python-path "/home/glenn/Notes/org/TFS/CMMS/.venv/bin/python")
              (script-path (namestring (merge-pathnames "scripts/generate_weekly_report.py" *base-directory*))))
          (uiop:run-program (list python-path script-path "--start" start-date "--end" end-date)
                            :output t :error-output t)
          (setf generated-now t)
          (setf images-exist (probe-file (merge-pathnames "01_operational_updates.png" report-path)))))
      
      (html-response
       (render-page "Weekly Report"
         (cl-who:with-html-output-to-string (s)
           (:div :class "page-header"
             (:h1 "Weekly Report Generator")
             (:a :href "/master-tracker" :class "btn" "Back to Master Tracker"))
           
           ;; Date range selector
           (:section :class "card"
             (:h2 "Report Period")
             (:form :method "get" :action "/master-tracker/weekly-report"
               (:div :class "form-row"
                 (:div :class "form-group"
                   (:label "Week Start (Monday)")
                   (:input :type "date" :name "week_start" :value start-date))
                 (:div :class "form-group"
                   (:label "Week End (Saturday)")
                   (:input :type "date" :name "week_end" :value end-date))
                 (:div :class "form-group" :style "align-self: flex-end;"
                   (:input :type "hidden" :name "generate" :value "1")
                   (:button :type "submit" :class "btn btn-primary" "Generate Report Slides")))))
           
           ;; Show generated images if they exist
           (when images-exist
             (cl-who:htm
              (:div :class "alert alert-success" :style "margin: 1rem 0; padding: 1rem; background: #d4edda; border-radius: 4px;"
                "✓ Report slides available for download.")
              (:section :class "card"
                (:h2 "Generated Slides")
                (:p "Click on any image to download it for PowerPoint.")
                (:div :class "form-actions" :style "margin-bottom: 1rem;"
                  (:a :href (format nil "/reports/~A/01_operational_updates.png" report-dir)
                      :download "01_operational_updates.png" :class "btn" "Download Slide 1")
                  (:a :href (format nil "/reports/~A/02_weekly_by_country.png" report-dir)
                      :download "02_weekly_by_country.png" :class "btn" "Download Slide 2")
                  (:a :href (format nil "/reports/~A/03_deficiency_types.png" report-dir)
                      :download "03_deficiency_types.png" :class "btn" "Download Slide 3")
                  (:a :href (format nil "/reports/~A/04_cumulative_stats.png" report-dir)
                      :download "04_cumulative_stats.png" :class "btn" "Download Slide 4")
                  (:a :href (format nil "/reports/~A/05_90_day_stats.png" report-dir)
                      :download "05_90_day_stats.png" :class "btn" "Download Slide 5")))
              (:section :class "card"
                (:h3 "Slide 1: Operational Updates")
                (:img :src (format nil "/reports/~A/01_operational_updates.png" report-dir)
                      :style "max-width: 100%; border: 1px solid #ccc;"))
              (:section :class "card"
                (:h3 "Slide 2: Weekly by Country")
                (:img :src (format nil "/reports/~A/02_weekly_by_country.png" report-dir)
                      :style "max-width: 100%; border: 1px solid #ccc;"))
              (:section :class "card"
                (:h3 "Slide 3: Deficiency Types")
                (:img :src (format nil "/reports/~A/03_deficiency_types.png" report-dir)
                      :style "max-width: 100%; border: 1px solid #ccc;"))
              (:section :class "card"
                (:h3 "Slide 4: Cumulative Stats")
                (:img :src (format nil "/reports/~A/04_cumulative_stats.png" report-dir)
                      :style "max-width: 100%; border: 1px solid #ccc;"))
              (:section :class "card"
                (:h3 "Slide 5: 90-Day Stats")
                (:img :src (format nil "/reports/~A/05_90_day_stats.png" report-dir)
                      :style "max-width: 100%; border: 1px solid #ccc;"))))
           ;; Show message if no images yet
           (unless images-exist
             (cl-who:htm
              (:section :class "card"
                (:p :class "empty-state" 
                    "No report slides generated yet. Click 'Generate Report Slides' to create them."))))))))))

(defun handle-api-camps-by-country ()
  "API endpoint to get camps for a country (for dynamic dropdowns)."
  (let* ((country-id (parse-int (get-param "country_id")))
         (camps (fetch-all "SELECT id, name FROM camps WHERE country_id = ? ORDER BY name" country-id)))
    (setf (hunchentoot:content-type*) "application/json")
    (cl-json:encode-json-to-string
     (mapcar (lambda (c) 
               `(("id" . ,(getf c :|id|)) ("name" . ,(getf c :|name|))))
             camps))))

(defun handle-deficiency-new (report-id-str)
  "Add deficiency form."
  (let* ((report-id (parse-int report-id-str))
         (report (get-inspection-report report-id))
         (voltage (getf report :|system_voltage|))
         (code-source (get-code-source voltage))
         (code-refs (if (string= code-source "NEC") *nec-code-references* *bs7671-code-references*)))
    (html-response
     (render-page "Add Deficiency"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Add Deficiency")
           (:p "Report: " (:strong (cl-who:str (getf report :|report_number|)))))
         (:form :method "post" :action (format nil "/api/inspection-reports/~A/deficiency" report-id)
                :class "form-card" :enctype "multipart/form-data"
           (:div :class "form-row"
             (:div :class "form-group required"
               (:label "Location Description")
               (:input :type "text" :name "location_description" :required t
                       :placeholder "e.g., Rear wall, AC unit 1-9"))
             (:div :class "form-group"
               (:label "Number of Occurrences")
               (:input :type "number" :name "num_occurrences" :value "1" :min "1")))
           (:div :class "form-row"
             (:div :class "form-group required"
               (:label "Deficiency Category")
               (:select :name "deficiency_category" :required t
                 (:option :value "" "-- Select --")
                 (dolist (cat *deficiency-categories*)
                   (cl-who:htm (:option :value cat (cl-who:str cat))))))
             (:div :class "form-group"
               (:label "Equipment Category")
               (:select :name "equipment_category"
                 (:option :value "" "-- Select --")
                 (dolist (eq *equipment-categories*)
                   (cl-who:htm (:option :value eq (cl-who:str eq)))))))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "Imminent Danger?")
               (:select :name "imminent_danger"
                 (:option :value "No" "No")
                 (:option :value "Yes" "Yes")))
             (:div :class "form-group"
               (:label "If Yes, Action Taken")
               (:input :type "text" :name "action_taken" :placeholder "Describe action...")))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "SOR Issued To")
               (:select :name "sor_issued_to"
                 (:option :value "" "-- Select --")
                 (dolist (sor *sor-issued-to*)
                   (cl-who:htm (:option :value sor (cl-who:str sor))))))
             (:div :class "form-group"
               (:label "Deficiency Status")
               (:select :name "deficiency_status"
                 (dolist (st *deficiency-statuses*)
                   (cl-who:htm (:option :value st (cl-who:str st)))))))
           (:div :class "form-group required"
             (:label "Description of Deficiency")
             (:textarea :name "description" :rows "3" :required t
                        :placeholder "Describe the deficiency in detail..."))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label (cl-who:str (format nil "Code Source: ~A" code-source)))
               (:input :type "hidden" :name "code_source" :value code-source))
             (:div :class "form-group required"
               (:label "Code Reference")
               (:select :name "code_reference" :required t
                 (:option :value "" "-- Select --")
                 (dolist (ref code-refs)
                   (cl-who:htm (:option :value ref (cl-who:str ref)))))))
           (:div :class "form-group"
             (:label "Deficiency Photo")
             (:input :type "file" :name "deficiency_image" :accept "image/*")
             (:p :class "help-text" "Upload a photo of the deficiency (optional)"))
           (:div :class "form-actions"
             (:button :type "submit" :class "btn btn-primary" "Add Deficiency")
             (:a :href (format nil "/inspection-reports/~A" report-id) :class "btn" "Cancel"))))))))

(defun handle-deficiency-edit (deficiency-id-str)
  "Edit deficiency form."
  (let* ((deficiency-id (parse-int deficiency-id-str))
         (def (get-deficiency deficiency-id))
         (report-id (getf def :|report_id|))
         (report (get-inspection-report report-id))
         (voltage (getf report :|system_voltage|))
         (code-source (get-code-source voltage))
         (code-refs (if (string= code-source "NEC") *nec-code-references* *bs7671-code-references*)))
    (html-response
     (render-page "Edit Deficiency"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 (cl-who:fmt "Edit Deficiency #~A" (getf def :|deficiency_number|)))
           (:p "Report: " (:strong (cl-who:str (getf report :|report_number|)))))
         (:form :method "post" :action (format nil "/api/deficiency/~A/update" deficiency-id)
                :class "form-card" :enctype "multipart/form-data"
           (:input :type "hidden" :name "report_id" :value (princ-to-string report-id))
           (:div :class "form-row"
             (:div :class "form-group required"
               (:label "Location Description")
               (:input :type "text" :name "location_description" :required t
                       :value (or (getf def :|location_description|) "")))
             (:div :class "form-group"
               (:label "Number of Occurrences")
               (:input :type "number" :name "num_occurrences" :min "1"
                       :value (or (getf def :|num_occurrences|) 1))))
           (:div :class "form-row"
             (:div :class "form-group required"
               (:label "Deficiency Category")
               (:select :name "deficiency_category" :required t
                 (:option :value "" "-- Select --")
                 (dolist (cat *deficiency-categories*)
                   (cl-who:htm 
                    (:option :value cat 
                             :selected (string= cat (getf def :|deficiency_category|))
                             (cl-who:str cat))))))
             (:div :class "form-group"
               (:label "Equipment Category")
               (:select :name "equipment_category"
                 (:option :value "" "-- Select --")
                 (dolist (eq *equipment-categories*)
                   (cl-who:htm 
                    (:option :value eq
                             :selected (string= eq (getf def :|equipment_category|))
                             (cl-who:str eq)))))))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "Imminent Danger?")
               (:select :name "imminent_danger"
                 (:option :value "No" :selected (string= "No" (getf def :|imminent_danger|)) "No")
                 (:option :value "Yes" :selected (string= "Yes" (getf def :|imminent_danger|)) "Yes")))
             (:div :class "form-group"
               (:label "If Yes, Action Taken")
               (:input :type "text" :name "action_taken" 
                       :value (or (getf def :|action_taken|) ""))))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "SOR Issued To")
               (:select :name "sor_issued_to"
                 (:option :value "" "-- Select --")
                 (dolist (sor *sor-issued-to*)
                   (cl-who:htm 
                    (:option :value sor
                             :selected (string= sor (getf def :|sor_issued_to|))
                             (cl-who:str sor))))))
             (:div :class "form-group"
               (:label "Deficiency Status")
               (:select :name "deficiency_status"
                 (dolist (st *deficiency-statuses*)
                   (cl-who:htm 
                    (:option :value st
                             :selected (string= st (getf def :|deficiency_status|))
                             (cl-who:str st)))))))
           (:div :class "form-group required"
             (:label "Description of Deficiency")
             (:textarea :name "description" :rows "3" :required t
                        (cl-who:str (or (getf def :|description|) ""))))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label (cl-who:str (format nil "Code Source: ~A" code-source)))
               (:input :type "hidden" :name "code_source" :value code-source))
             (:div :class "form-group required"
               (:label "Code Reference")
               (:select :name "code_reference" :required t
                 (:option :value "" "-- Select --")
                 (dolist (ref code-refs)
                   (cl-who:htm 
                    (:option :value ref
                             :selected (string= ref (getf def :|code_reference|))
                             (cl-who:str ref)))))))
           (:div :class "form-group"
             (:label "Deficiency Photo")
             (when (getf def :|image_path|)
               (cl-who:htm
                (:div :style "margin-bottom: 0.5rem;"
                  (:img :src (format nil "/uploads/~A" (getf def :|image_path|))
                        :class "deficiency-thumbnail" :alt "Current photo")
                  (:p :class "help-text" "Current photo (upload new to replace)"))))
             (:input :type "file" :name "deficiency_image" :accept "image/*"))
           (:div :class "form-actions"
             (:button :type "submit" :class "btn btn-primary" "Save Changes")
             (:a :href (format nil "/inspection-reports/~A" report-id) :class "btn" "Cancel"))))))))

;;; Inspection Report API Handlers

(defun handle-api-inspection-report-create ()
  "Create a new inspection report."
  (let ((wo-id (parse-int (get-param "wo_id")))
        (site-id (parse-int (get-param "site_id")))
        (building-number (get-param "building_number"))
        (building-type (get-param "building_type"))
        (system-voltage (get-param "system_voltage"))
        (inspection-phase (get-param "inspection_phase"))
        (team-number (get-param "team_number"))
        (inspection-date (get-param "inspection_date"))
        (location-description (get-param "location_description")))
    (let ((report-id (create-inspection-report 
                      wo-id site-id building-number building-type system-voltage
                      inspection-phase team-number inspection-date
                      :location-description location-description)))
      (redirect-to (format nil "/inspection-reports/~A" report-id)))))

(defun handle-api-inspection-report-update (id)
  "Update an inspection report."
  (let ((report-id (parse-int id))
        (inspection-date (get-param "inspection_date"))
        (team-number (get-param "team_number"))
        (building-type (get-param "building_type"))
        (building-number (get-param "building_number"))
        (location-description (get-param "location_description"))
        (overall-rating (get-param "overall_rating"))
        (mrf-needed (get-param "mrf_needed"))
        (summary-of-findings (get-param "summary_of_findings")))
    (update-inspection-report report-id
                              :inspection-date inspection-date
                              :team-number team-number
                              :building-type building-type
                              :building-number building-number
                              :location-description location-description
                              :overall-rating overall-rating
                              :mrf-needed mrf-needed
                              :summary-of-findings summary-of-findings)
    ;; Auto-generate MRF if MRF Required = Yes and no MRF exists yet
    (when (and mrf-needed (string= mrf-needed "Yes"))
      (let ((existing-mrf (get-mrf-by-report report-id)))
        (unless existing-mrf
          (create-mrf-from-inspection report-id))))
    (redirect-to (format nil "/inspection-reports/~A" report-id))))

(defun handle-api-inspection-report-building-image (id)
  "Upload building image for a report."
  (let* ((report-id (parse-int id))
         (image-post-param (hunchentoot:post-parameter "building_image"))
         (image-path (when image-post-param
                       (save-uploaded-file image-post-param "buildings" 
                                           (format nil "bldg-~A" report-id)))))
    (when image-path
      (execute-sql "UPDATE inspection_reports SET building_image_path = ? WHERE id = ?"
                   image-path report-id))
    (redirect-to (format nil "/inspection-reports/~A" report-id))))

(defun handle-api-inspection-report-submit (id)
  "Submit report for QC review."
  (let* ((report-id (parse-int id))
         (user (get-current-user))
         (user-id (getf user :|id|))
         (inspector1-name (get-param "inspector1_name"))
         (inspector2-name (get-param "inspector2_name"))
         (report (get-inspection-report report-id)))
    (submit-report-for-qc report-id inspector1-name inspector2-name)
    ;; Record who submitted the report for notifications
    (execute-sql "UPDATE inspection_reports SET submitted_by_user_id = ? WHERE id = ?" user-id report-id)
    ;; Notify QC team
    (notify-qc-team report-id (getf report :|report_number|) inspector1-name)
    (redirect-to (format nil "/inspection-reports/~A" report-id))))

(defun handle-api-inspection-report-qc-approve (id)
  "QC approve a report."
  (let* ((report-id (parse-int id))
         (report (get-inspection-report report-id))
         (qc-name (get-param "qc_name"))
         (qc-comments (or (get-param "qc_comments") ""))
         (submitted-by (getf report :|submitted_by_user_id|)))
    (qc-approve-report report-id qc-name qc-comments)
    (finalize-report report-id)
    ;; Notify submitting inspector
    (when submitted-by
      (execute-sql 
       "INSERT INTO notifications (user_id, title, message, link) VALUES (?, ?, ?, ?)"
       submitted-by
       "Report Approved - Signature Required"
       (format nil "Report ~A has been approved by QC. Please sign and finalize." 
               (getf report :|report_number|))
       (format nil "/inspection-reports/~A" report-id)))
    (redirect-to (format nil "/inspection-reports/~A" report-id))))

(defun handle-api-create-reinspection (original-id-str)
  "Create a re-inspection report from a completed initial inspection."
  (let* ((original-id (parse-int original-id-str))
         (team-number (get-param "team_number"))
         (inspection-date (get-param "inspection_date"))
         (new-report-id (create-reinspection-report original-id 
                                                     :team-number team-number
                                                     :inspection-date inspection-date)))
    (redirect-to (format nil "/inspection-reports/~A" new-report-id))))

(defun handle-api-inspection-report-qc-reject (id)
  "QC reject a report."
  (let* ((report-id (parse-int id))
         (report (get-inspection-report report-id))
         (qc-name (get-param "qc_name"))
         (qc-comments (or (get-param "qc_comments") ""))
         (submitted-by (getf report :|submitted_by_user_id|))
         (current-rejection-count (or (getf report :|rejection_count|) 0))
         (new-rejection-number (1+ current-rejection-count)))
    (qc-reject-report report-id qc-name qc-comments)
    ;; Increment rejection counter
    (execute-sql "UPDATE inspection_reports SET rejection_count = ? WHERE id = ?" 
                 new-rejection-number report-id)
    ;; Record rejection history
    (execute-sql 
     "INSERT INTO rejection_history (report_id, rejection_number, team_number, qc_name, qc_comments, submitted_at)
      VALUES (?, ?, ?, ?, ?, ?)"
     report-id new-rejection-number 
     (getf report :|team_number|) qc-name qc-comments
     (getf report :|inspector1_signed_at|))
    ;; Notify submitting inspector
    (when submitted-by
      (execute-sql 
       "INSERT INTO notifications (user_id, title, message, link) VALUES (?, ?, ?, ?)"
       submitted-by
       "Report Rejected - Corrections Needed"
       (format nil "Report ~A was rejected by ~A. Comments: ~A" 
               (getf report :|report_number|) qc-name qc-comments)
       (format nil "/inspection-reports/~A" report-id)))
    (redirect-to (format nil "/inspection-reports/~A" report-id))))

(defun handle-api-inspection-report-assign (id)
  "Assign a report to a QC reviewer. QC Manager only."
  (let* ((user (get-current-user))
         (report-id (parse-int id))
         (assigned-qc-id-str (get-param "assigned_qc_id"))
         (assigned-qc-id (if (and assigned-qc-id-str (> (length assigned-qc-id-str) 0))
                             (parse-int assigned-qc-id-str)
                             nil)))
    (if (and user (user-is-qc-manager-p user))
        (progn
          (execute-sql "UPDATE inspection_reports SET assigned_qc_id = ? WHERE id = ?" 
                       assigned-qc-id report-id)
          (redirect-to "/"))
        (redirect-to "/unauthorized"))))

(defun handle-api-deficiency-create (report-id-str)
  "Add a deficiency to a report."
  (let* ((report-id (parse-int report-id-str))
         (location-description (get-param "location_description"))
         (num-occurrences (parse-int (get-param "num_occurrences")))
         (deficiency-category (get-param "deficiency_category"))
         (equipment-category (get-param "equipment_category"))
         (imminent-danger (get-param "imminent_danger"))
         (action-taken (get-param "action_taken"))
         (sor-issued-to (get-param "sor_issued_to"))
         (description (get-param "description"))
         (code-source (get-param "code_source"))
         (code-reference (get-param "code_reference"))
         (deficiency-status (get-param "deficiency_status"))
         ;; Handle image upload
         (image-post-param (hunchentoot:post-parameter "deficiency_image"))
         (image-path (when image-post-param
                       (save-uploaded-file image-post-param "deficiencies" 
                                           (format nil "def-~A" report-id)))))
    (add-deficiency report-id
                    :location-description location-description
                    :num-occurrences num-occurrences
                    :deficiency-category deficiency-category
                    :equipment-category equipment-category
                    :imminent-danger imminent-danger
                    :action-taken action-taken
                    :sor-issued-to sor-issued-to
                    :description description
                    :code-source code-source
                    :code-reference code-reference
                    :deficiency-status deficiency-status
                    :image-path image-path)
    (redirect-to (format nil "/inspection-reports/~A" report-id))))

(defun handle-api-deficiency-delete (deficiency-id-str)
  "Delete a deficiency."
  (let ((deficiency-id (parse-int deficiency-id-str))
        (report-id (get-param "report_id")))
    (delete-deficiency deficiency-id)
    (redirect-to (format nil "/inspection-reports/~A" report-id))))

(defun handle-api-deficiency-update (deficiency-id-str)
  "Update a deficiency."
  (let* ((deficiency-id (parse-int deficiency-id-str))
         (report-id (get-param "report_id"))
         (location-description (get-param "location_description"))
         (num-occurrences (parse-int (get-param "num_occurrences")))
         (deficiency-category (get-param "deficiency_category"))
         (equipment-category (get-param "equipment_category"))
         (imminent-danger (get-param "imminent_danger"))
         (action-taken (get-param "action_taken"))
         (sor-issued-to (get-param "sor_issued_to"))
         (description (get-param "description"))
         (code-source (get-param "code_source"))
         (code-reference (get-param "code_reference"))
         (deficiency-status (get-param "deficiency_status"))
         ;; Handle image upload (only if new file provided)
         (image-post-param (hunchentoot:post-parameter "deficiency_image"))
         (new-image-path (when (and image-post-param (listp image-post-param) (first image-post-param))
                           (save-uploaded-file image-post-param "deficiencies" 
                                               (format nil "def-~A" deficiency-id)))))
    (update-deficiency deficiency-id
                       :location-description location-description
                       :num-occurrences num-occurrences
                       :deficiency-category deficiency-category
                       :equipment-category equipment-category
                       :imminent-danger imminent-danger
                       :action-taken action-taken
                       :sor-issued-to sor-issued-to
                       :description description
                       :code-source code-source
                       :code-reference code-reference
                       :deficiency-status deficiency-status
                       :image-path new-image-path)
    (redirect-to (format nil "/inspection-reports/~A" report-id))))

(defun handle-api-generate-pdf (report-id-str)
  "Generate PDF for a report and redirect to download."
  (let* ((report-id (parse-int report-id-str))
         (pdf-filename (generate-report-pdf report-id)))
    (if pdf-filename
        (redirect-to (format nil "/reports/pdf/~A" pdf-filename))
        (progn
          (setf (hunchentoot:return-code*) 500)
          (html-response
           (render-page "PDF Generation Error"
             (cl-who:with-html-output-to-string (s)
               (:div :class "empty-state"
                 (:h1 "PDF Generation Failed")
                 (:p "Could not generate PDF. Please try again.")
                 (:a :href (format nil "/inspection-reports/~A" report-id) 
                     :class "btn" "Back to Report")))))))))

;;; Static file handler

(defun handle-static ()
  "Serve static files."
  (let* ((uri (hunchentoot:request-uri*))
         (path (subseq uri 8)) ; Remove "/static/"
         (file-path (merge-pathnames path *static-directory*)))
    (if (probe-file file-path)
        (hunchentoot:handle-static-file file-path)
        (setf (hunchentoot:return-code*) 404))))

(defun handle-uploads ()
  "Serve uploaded files."
  (let* ((uri (hunchentoot:request-uri*))
         (path (subseq uri 9)) ; Remove "/uploads/"
         (file-path (merge-pathnames path *uploads-directory*)))
    (if (probe-file file-path)
        (hunchentoot:handle-static-file file-path)
        (progn
          (setf (hunchentoot:return-code*) 404)
          "Not found"))))

(defun handle-reports-files ()
  "Serve generated report files (PNG images)."
  (let* ((uri (hunchentoot:request-uri*))
         (path (subseq uri 9)) ; Remove "/reports/"
         (file-path (merge-pathnames path (merge-pathnames "reports/" *base-directory*))))
    (if (probe-file file-path)
        (hunchentoot:handle-static-file file-path)
        (progn
          (setf (hunchentoot:return-code*) 404)
          "Not found"))))

(defun handle-pdf-download ()
  "Serve or generate PDF reports."
  (let* ((uri (hunchentoot:request-uri*))
         (path (hunchentoot:url-decode (subseq uri 13))) ; Remove "/reports/pdf/" and decode
         (file-path (merge-pathnames path *reports-directory*)))
    (if (probe-file file-path)
        (progn
          (setf (hunchentoot:content-type*) "application/pdf")
          (hunchentoot:handle-static-file file-path))
        (progn
          (setf (hunchentoot:return-code*) 404)
          (html-response
           (render-page "PDF Not Found"
             (cl-who:with-html-output-to-string (s)
               (:div :class "empty-state"
                 (:h1 "PDF Not Found")
                 (:p "The requested PDF file does not exist. Try generating it again.")
                 (:a :href "/inspection-reports" :class "btn" "Back to Reports")))))))))

;;; Router

(defun dispatch-request ()
  "Main request dispatcher."
  (let* ((full-uri (hunchentoot:request-uri*))
         (uri (first (cl-ppcre:split "\\?" full-uri))) ; Strip query string
         (method (hunchentoot:request-method*)))
    (cond
      ;; Static files
      ((cl-ppcre:scan "^/static/" uri)
       (handle-static))
      
      ;; Authentication
      ((string= uri "/login")
       (handle-login))
      ((string= uri "/logout")
       (handle-logout))
      ((string= uri "/change-password")
       (handle-change-password))
      ((string= uri "/api/change-password")
       (handle-api-change-password))
      ((string= uri "/unauthorized")
       (handle-unauthorized))
      ((and (eq method :post) (string= uri "/api/login"))
       (handle-api-login))
      
      ;; Admin Panel
      ((string= uri "/admin/reports")
       (handle-admin-reports))
      ((string= uri "/admin/users")
       (handle-admin-users))
      ((string= uri "/admin/users/new")
       (handle-admin-user-new))
      ((cl-ppcre:scan "^/admin/users/(\\d+)/edit$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/admin/users/(\\d+)/edit$" uri)
         (declare (ignore match))
         (handle-admin-user-edit (aref groups 0))))
      ((and (eq method :post) (string= uri "/api/admin/users/create"))
       (handle-api-admin-user-create))
      ((and (eq method :post) (cl-ppcre:scan "^/api/admin/users/(\\d+)/update$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/admin/users/(\\d+)/update$" uri)
         (declare (ignore match))
         (handle-api-admin-user-update (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/admin/users/(\\d+)/reset-password$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/admin/users/(\\d+)/reset-password$" uri)
         (declare (ignore match))
         (handle-api-admin-reset-password (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/admin/users/(\\d+)/clear-password-flag$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/admin/users/(\\d+)/clear-password-flag$" uri)
         (declare (ignore match))
         (handle-api-admin-clear-password-flag (aref groups 0))))
      ((string= uri "/admin/settings")
       (handle-admin-settings))
      ((and (eq method :post) (string= uri "/api/admin/settings/update"))
       (handle-api-admin-settings-update))
      
      ;; Pages
      ((string= uri "/")
       (handle-index))
      ((string= uri "/work-orders")
       (handle-work-orders))
      ((string= uri "/work-orders/new")
       (handle-work-order-new))
      ((cl-ppcre:scan "^/work-orders/(\\d+)$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/work-orders/(\\d+)$" uri)
         (declare (ignore match))
         (handle-work-order-detail (aref groups 0))))
      ((string= uri "/sites")
       (handle-sites))
      ((string= uri "/inventory")
       (handle-inventory))
      ((string= uri "/inventory/locations")
       (handle-inventory-locations))
      ((cl-ppcre:scan "^/inventory/item/(\\d+)$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/inventory/item/(\\d+)$" uri)
         (declare (ignore match))
         (handle-inventory-item-detail (aref groups 0))))
      ((string= uri "/inventory/audit/new")
       (handle-inventory-audit-new))
      ((cl-ppcre:scan "^/inventory/audit/(\\d+)$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/inventory/audit/(\\d+)$" uri)
         (declare (ignore match))
         (handle-inventory-audit-detail (aref groups 0))))
      
      ;; MRF Routes
      ((string= uri "/mrf")
       (handle-mrf-list))
      ((string= uri "/mrf/new")
       (handle-mrf-new))
      ((cl-ppcre:scan "^/mrf/(\\d+)/print$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/mrf/(\\d+)/print$" uri)
         (declare (ignore match))
         (handle-mrf-print (aref groups 0))))
      ((cl-ppcre:scan "^/mrf/(\\d+)$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/mrf/(\\d+)$" uri)
         (declare (ignore match))
         (handle-mrf-detail (aref groups 0))))
      
      ;; R&R Routes
      ((string= uri "/rr")
       (handle-rr-dashboard))
      ((string= uri "/rr/approve")
       (handle-rr-approval-queue))
      ((string= uri "/rr/calendar")
       (handle-rr-calendar))
      ((string= uri "/rr/calendar/print")
       (handle-rr-calendar-print))
      ((string= uri "/rr/annual")
       (handle-rr-annual-gantt))
      ((cl-ppcre:scan "^/rr/(\\d+)/edit$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/rr/(\\d+)/edit$" uri)
         (declare (ignore match))
         (handle-rr-edit (aref groups 0))))
      
      ((string= uri "/reports")
       (handle-reports))
      
      ;; Uploaded files
      ((cl-ppcre:scan "^/uploads/" uri)
       (handle-uploads))
      
      ;; PDF reports (must come before general /reports/)
      ((cl-ppcre:scan "^/reports/pdf/" uri)
       (handle-pdf-download))
      
      ;; Generated report images (weekly report PNGs)
      ((cl-ppcre:scan "^/reports/week_" uri)
       (handle-reports-files))
      
      ;; Inspection Reports
      ((string= uri "/inspection-reports")
       (handle-inspection-reports-list))
      ((string= uri "/inspection-reports/new")
       (handle-inspection-report-new))
      ((cl-ppcre:scan "^/inspection-reports/(\\d+)$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/inspection-reports/(\\d+)$" uri)
         (declare (ignore match))
         (handle-inspection-report-detail (aref groups 0))))
      ((cl-ppcre:scan "^/inspection-reports/(\\d+)/deficiency/new$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/inspection-reports/(\\d+)/deficiency/new$" uri)
         (declare (ignore match))
         (handle-deficiency-new (aref groups 0))))
      ((cl-ppcre:scan "^/deficiency/(\\d+)/edit$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/deficiency/(\\d+)/edit$" uri)
         (declare (ignore match))
         (handle-deficiency-edit (aref groups 0))))
      ((cl-ppcre:scan "^/inspection-reports/(\\d+)/history$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/inspection-reports/(\\d+)/history$" uri)
         (declare (ignore match))
         (handle-inspection-report-history (aref groups 0))))
      ((cl-ppcre:scan "^/inspection-reports/(\\d+)/reinspection$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/inspection-reports/(\\d+)/reinspection$" uri)
         (declare (ignore match))
         (handle-reinspection-form (aref groups 0))))
      
      ;; Master Tracker routes
      ((string= uri "/master-tracker")
       (handle-master-tracker))
      ((string= uri "/master-tracker/weekly-report")
       (handle-weekly-report))
      ((string= uri "/api/master-tracker/camps")
       (handle-api-camps-by-country))
      
      ;; DAR (Daily Activity Report) routes
      ((string= uri "/dar")
       (handle-dar-list))
      ((string= uri "/dar/new")
       (handle-dar-new))
      ((cl-ppcre:scan "^/dar/(\\d+)$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/dar/(\\d+)$" uri)
         (declare (ignore match))
         (handle-dar-detail (aref groups 0))))
      ((cl-ppcre:scan "^/dar/(\\d+)/pdf$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/dar/(\\d+)/pdf$" uri)
         (declare (ignore match))
         (handle-dar-pdf (aref groups 0))))
      ((and (eq method :post) (string= uri "/api/dar/create"))
       (handle-api-dar-create))
      
      ;; IRP (Immediate Repair Package) routes
      ((string= uri "/irp")
       (handle-irp-list))
      ((string= uri "/irp/new")
       (handle-irp-new))
      ((cl-ppcre:scan "^/irp/(\\d+)$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/irp/(\\d+)$" uri)
         (declare (ignore match))
         (handle-irp-detail (aref groups 0))))
      ((and (eq method :post) (string= uri "/api/irp/create"))
       (handle-api-irp-create))
      
      ;; SAR (Site Activity Report) routes
      ((string= uri "/sar")
       (handle-sar-form))
      ((string= uri "/sar/generate")
       (handle-sar-generate))
      
      ;; API endpoints
      ((and (eq method :post) (string= uri "/api/sites/create"))
       (handle-api-sites-create))
      ((and (eq method :post) (string= uri "/api/work-orders/create"))
       (handle-api-work-orders-create))
      ((and (eq method :post) (cl-ppcre:scan "^/api/work-orders/(\\d+)/update$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/work-orders/(\\d+)/update$" uri)
         (declare (ignore match))
         (handle-api-work-orders-update (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/work-orders/(\\d+)/activity$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/work-orders/(\\d+)/activity$" uri)
         (declare (ignore match))
         (handle-api-work-orders-activity (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/activity/(\\d+)/delete$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/activity/(\\d+)/delete$" uri)
         (declare (ignore match))
         (handle-api-activity-delete (aref groups 0) (get-param "wo_id"))))
      
      ;; Inspection Report API endpoints
      ((and (eq method :post) (string= uri "/api/inspection-reports/create"))
       (handle-api-inspection-report-create))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/create-reinspection$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/create-reinspection$" uri)
         (declare (ignore match))
         (handle-api-create-reinspection (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/update$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/update$" uri)
         (declare (ignore match))
         (handle-api-inspection-report-update (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/submit$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/submit$" uri)
         (declare (ignore match))
         (handle-api-inspection-report-submit (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/qc-approve$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/qc-approve$" uri)
         (declare (ignore match))
         (handle-api-inspection-report-qc-approve (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/qc-reject$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/qc-reject$" uri)
         (declare (ignore match))
         (handle-api-inspection-report-qc-reject (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/building-image$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/building-image$" uri)
         (declare (ignore match))
         (handle-api-inspection-report-building-image (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/assign$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/assign$" uri)
         (declare (ignore match))
         (handle-api-inspection-report-assign (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/deficiency$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/deficiency$" uri)
         (declare (ignore match))
         (handle-api-deficiency-create (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inspection-reports/(\\d+)/generate-mrf$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/generate-mrf$" uri)
         (declare (ignore match))
         (let ((report-id (parse-int (aref groups 0))))
           (create-mrf-from-inspection report-id)
           (redirect-to (format nil "/inspection-reports/~A" report-id)))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/deficiency/(\\d+)/delete$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/deficiency/(\\d+)/delete$" uri)
         (declare (ignore match))
         (handle-api-deficiency-delete (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/notifications/(\\d+)/read$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/notifications/(\\d+)/read$" uri)
         (declare (ignore match))
         (mark-notification-read (parse-int (aref groups 0)))
         (redirect-to "/")))
      ((and (eq method :post) (cl-ppcre:scan "^/api/deficiency/(\\d+)/update$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/deficiency/(\\d+)/update$" uri)
         (declare (ignore match))
         (handle-api-deficiency-update (aref groups 0))))
      ((cl-ppcre:scan "^/api/inspection-reports/(\\d+)/generate-pdf$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inspection-reports/(\\d+)/generate-pdf$" uri)
         (declare (ignore match))
         (handle-api-generate-pdf (aref groups 0))))
      
      ;; MRF API endpoints
      ((and (eq method :post) (string= uri "/api/mrf/create"))
       (handle-api-mrf-create))
      ((and (eq method :post) (cl-ppcre:scan "^/api/mrf/(\\d+)/item$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/mrf/(\\d+)/item$" uri)
         (declare (ignore match))
         (handle-api-mrf-add-item (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/mrf/(\\d+)/item/(\\d+)/delete$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/mrf/(\\d+)/item/(\\d+)/delete$" uri)
         (declare (ignore match))
         (handle-api-mrf-delete-item (aref groups 0) (aref groups 1))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/mrf/(\\d+)/submit$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/mrf/(\\d+)/submit$" uri)
         (declare (ignore match))
         (handle-api-mrf-submit (aref groups 0))))
      
      ;; R&R API endpoints
      ((and (eq method :post) (string= uri "/api/rr/request"))
       (handle-api-rr-request))
      ((and (eq method :post) (cl-ppcre:scan "^/api/rr/(\\d+)/approve$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/rr/(\\d+)/approve$" uri)
         (declare (ignore match))
         (handle-api-rr-approve (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/rr/(\\d+)/reject$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/rr/(\\d+)/reject$" uri)
         (declare (ignore match))
         (handle-api-rr-reject (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/rr/(\\d+)/status$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/rr/(\\d+)/status$" uri)
         (declare (ignore match))
         (handle-api-rr-status-update (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/rr/(\\d+)/update$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/rr/(\\d+)/update$" uri)
         (declare (ignore match))
         (handle-api-rr-update (aref groups 0))))
      ((and (eq method :get) (string= uri "/api/rr/check-dates"))
       (handle-api-rr-check-dates))
      
      ;; 404
      (t
       (setf (hunchentoot:return-code*) 404)
       (html-response
        (render-page "Not Found"
          (cl-who:with-html-output-to-string (s)
            (:div :class "empty-state"
              (:h1 "Page Not Found")
              (:a :href "/" :class "btn" "Go Home")))))))))

;;; Server control

(defun seed-default-sites ()
  "Seed the database with default sites if empty."
  (let ((count (getf (fetch-one "SELECT COUNT(*) as count FROM sites") :|count|)))
    (when (zerop count)
      (format t "~&Seeding ~A default sites...~%" (length *default-sites*))
      (dolist (site *default-sites*)
        (let* ((code (first site))
               (name (second site))
               (country (third site))
               (full-name (format nil "~A (~A)" name country)))
          (handler-case
              (create-site code full-name)
            (error (e) (format t "~&Warning: Could not create site ~A: ~A~%" code e)))))
      (format t "~&Sites seeded.~%"))))

(defun start-server (&key (port *server-port*))
  "Start the CMMS web server."
  (init-database)
  (seed-default-sites)
  (seed-admin-user)
  (cleanup-expired-sessions)
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))
  (setf (hunchentoot:acceptor-document-root *acceptor*) *static-directory*)
  (push (hunchentoot:create-prefix-dispatcher "/" 'dispatch-request)
        hunchentoot:*dispatch-table*)
  (hunchentoot:start *acceptor*)
  (format t "~&TFS CMMS server started on http://localhost:~A~%" port))

(defun stop-server ()
  "Stop the CMMS web server."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  (disconnect-db)
  (format t "~&TFS CMMS server stopped.~%"))
