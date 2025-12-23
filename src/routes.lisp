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

;;; Public Landing Page

(defun handle-landing-page ()
  "Public landing page for non-logged-in users."
  (let* ((stats (fetch-one "SELECT 
                             COUNT(DISTINCT building_number) as facilities_inspected,
                             COUNT(*) as deficiencies_identified,
                             COUNT(CASE WHEN deficiency_status IN ('Closed / Repaired', 'Closed/Repaired') THEN 1 END) as deficiencies_repaired
                           FROM master_deficiencies"))
         (facilities-count (or (getf stats :|facilities_inspected|) 0))
         (identified-count (or (getf stats :|deficiencies_identified|) 0))
         (repaired-count (or (getf stats :|deficiencies_repaired|) 0))
         (active-teams (or (getf (fetch-one "SELECT COUNT(DISTINCT name) as c FROM camps WHERE id IN (SELECT DISTINCT camp_id FROM master_deficiencies WHERE inspection_date >= date('now', '-90 days'))") :|c|) 12)))
    (html-response
     (cl-who:with-html-output-to-string (s nil :prologue t)
       (:html
        (:head
         (:meta :charset "UTF-8")
         (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
         (:title "Task Force SAFE - Electrical Safety Inspections")
         (:link :rel "stylesheet" :href "/static/css/style.css"))
        (:body :class "landing-page"
         ;; Header with login button
         (:header :class "landing-header"
           (:div :class "landing-header-logos"
             (:img :src "/static/img/company_logo.png" :alt "Versar" :class "logo-company")
             (:img :src "/static/img/TFS_Logo.png" :alt "Task Force SAFE" :class "logo-tfs"))
           (:a :href "/login" :class "btn btn-primary" "Staff Login"))
         
         ;; Hero section
         (:section :class "landing-hero"
           (:div :class "landing-hero-content"
             (:h1 "Task Force SAFE")
             (:p :class "landing-subtitle" "Electrical Safety Inspection Program")
             (:p :class "landing-tagline" "Safeguarding Personnel and Facilities Across CENTCOM")))
         
         ;; Mission section
         (:section :class "landing-section"
           (:div :class "landing-container"
             (:h2 "Our Mission")
             (:p "Task Force SAFE (TF SAFE) plays a vital role in safeguarding personnel and facilities, 
                  reducing risks, and ensuring operational readiness in challenging environments across 
                  the CENTCOM Area of Responsibility.")
             (:p "Our teams provide subject matter expertise delivering safe operating conditions 
                  throughout military installations in Syria, Iraq, Jordan, Kuwait, and surrounding regions.")))
         
         ;; Impact section
         (:section :class "landing-section landing-section-alt"
           (:div :class "landing-container"
             (:h2 "Program Impact")
             (:p :class "landing-stats-intro" "Since July 2020, Task Force SAFE has:")
             (:div :class "landing-stats"
               (:div :class "landing-stat"
                 (:span :class "landing-stat-number" (cl-who:fmt "~:D" facilities-count))
                 (:span :class "landing-stat-label" "Facilities Inspected"))
               (:div :class "landing-stat"
                 (:span :class "landing-stat-number" (cl-who:fmt "~:D" identified-count))
                 (:span :class "landing-stat-label" "Deficiencies Identified"))
               (:div :class "landing-stat"
                 (:span :class "landing-stat-number" (cl-who:fmt "~:D" repaired-count))
                 (:span :class "landing-stat-label" "Deficiencies Repaired"))
               (:div :class "landing-stat"
                 (:span :class "landing-stat-number" (cl-who:fmt "~D" active-teams))
                 (:span :class "landing-stat-label" "Active Teams")))
           (:p :class "landing-impact" "Through identification and repair of electrical deficiencies, 
                our field teams have prevented an immeasurable number of electrocutions, shocks, 
                and fires for facility occupants throughout the AOR.")))
       
       ;; Services section
       (:section :class "landing-section"
         (:div :class "landing-container"
           (:h2 "What We Do")
           (:div :class "landing-services"
             (:div :class "landing-service"
               (:h3 "Electrical Inspections")
               (:p "Comprehensive inspections from source to end users, identifying Life, Health, 
                    and Safety (LHS) electrical deficiencies."))
             (:div :class "landing-service"
               (:h3 "Deficiency Repair")
               (:p "Immediate mitigation and repair of unsafe conditions. No deficiency is ever 
                    left in an unsafe state."))
             (:div :class "landing-service"
               (:h3 "Quality Assurance")
               (:p "Rigorous QA/QC processes ensure accurate reporting and continuous improvement 
                    across all inspection activities."))
             (:div :class "landing-service"
               (:h3 "UFC Inspections")
               (:p "Multi-discipline facility assessments including structural, mechanical, 
                    fire protection, and environmental evaluations.")))))
       
         ;; Footer
         (:footer :class "landing-footer"
           (:p "Task Force SAFE / UFC - CENTCOM Area of Responsibility")
           (:p :class "landing-footer-sub" "Versar Global Solutions"))))))))

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
             (:a :href "/" :class "login-back-link" "← Back to Home")
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
    (redirect-to "/")))

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

;;; Admin Panel Handlers - moved to admin.lisp

;;; Route handlers

(defun handle-index ()
  "Dashboard showing pending items based on user role, or landing page for visitors."
  (let ((user (get-current-user)))
    (if user
        ;; Logged in - show dashboard
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
                                             ;; QC Manager sees all pending reports with MRF info
                                             (fetch-all 
                                              "SELECT r.*, s.name as site_name, u.full_name as assigned_to_name,
                                                      mr.id as mrf_id, mr.mrf_number, mr.status as mrf_status,
                                                      (SELECT COUNT(*) FROM material_request_items WHERE mrf_id = mr.id) as mrf_item_count
                                               FROM inspection_reports r 
                                               JOIN sites s ON r.site_id = s.id 
                                               LEFT JOIN users u ON r.assigned_qc_id = u.id
                                               LEFT JOIN material_requests mr ON mr.inspection_report_id = r.id
                                               WHERE r.status = 'Pending QC' 
                                               ORDER BY r.assigned_qc_id IS NULL DESC, r.created_at DESC")
                                             ;; QC Specialists see only their assigned reports with MRF info
                                             (fetch-all 
                                              "SELECT r.*, s.name as site_name,
                                                      mr.id as mrf_id, mr.mrf_number, mr.status as mrf_status,
                                                      (SELECT COUNT(*) FROM material_request_items WHERE mrf_id = mr.id) as mrf_item_count
                                               FROM inspection_reports r 
                                               JOIN sites s ON r.site_id = s.id 
                                               LEFT JOIN material_requests mr ON mr.inspection_report_id = r.id
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
                                     (:th "Inspector") (:th "Submitted") (:th "MRF")
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
                                     (:td (if (getf rpt :|mrf_id|)
                                              (cl-who:htm
                                               (:a :href (format nil "/mrf/~A" (getf rpt :|mrf_id|))
                                                   :class "badge badge-info"
                                                   :title (format nil "~A items" (or (getf rpt :|mrf_item_count|) 0))
                                                   "MRF"))
                                              (cl-who:htm (:span :class "text-muted" "-"))))
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
        ;; Not logged in - show public landing page
        (handle-landing-page))))

(defun handle-work-orders ()
  "Work orders list page."
  (let* ((site-id (parse-int (get-param "site")))
         (status-param (get-param "status"))
         (status (when (and status-param (> (length status-param) 0)) status-param))
         (date-from (get-param "date_from"))
         (date-to (get-param "date_to"))
         (search-term (get-param "search"))
         (contract-week (get-param "contract_week"))
         (current-period (or (get-system-setting "contract_current_period") "BY"))
         (available-weeks (get-available-contract-weeks current-period))
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
             (:label "Contract Week:"
               (:select :name "contract_week" :id "wo-contract-week" :onchange "updateWoDatesFromWeek()"
                 (:option :value "" "-- Select --")
                 (dolist (week available-weeks)
                   (cl-who:htm
                    (:option :value (format nil "~A-~A" current-period (getf week :week-number))
                             :data-start (getf week :week-start)
                             :data-end (getf week :week-end)
                             :selected (string= contract-week (format nil "~A-~A" current-period (getf week :week-number)))
                             (cl-who:fmt "~A Wk ~A" current-period (getf week :week-number)))))))
             (:label "From:"
               (:input :type "date" :name "date_from" :id "wo-date-from" :value (or date-from "")))
             (:label "To:"
               (:input :type "date" :name "date_to" :id "wo-date-to" :value (or date-to "")))
             (:button :type "submit" :class "btn" "Filter")
             (:a :href "/work-orders" :class "btn btn-secondary" "Clear"))
           (:script "
function updateWoDatesFromWeek() {
  var select = document.getElementById('wo-contract-week');
  var option = select.options[select.selectedIndex];
  if (option.value) {
    document.getElementById('wo-date-from').value = option.dataset.start;
    document.getElementById('wo-date-to').value = option.dataset.end;
  }
}
"))
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
         (contract-week (get-param "contract_week"))
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
                   (:p :style "font-size: 0.75rem; margin: 0; color: #666;" "TF SAFE CMMS - Report")
                   (:h1 (cl-who:str (if (and contract-week (> (length contract-week) 0))
                                       (format nil "Inspection Schedule Report - ~A" contract-week)
                                       "Inspection Schedule Report")))
                   (:p "Task Force SAFE - CENTCOM AOR"))
                 (:img :src "/static/img/TFS_Logo.png" :alt "Task Force SAFE"))
               (:div :class "no-print" :style "margin-bottom: 1rem;"
                 (:button :onclick "window.print()" :class "btn btn-primary" "Print / Save as PDF"))
               (:div :class "report-header card"
                 (:div :class "report-meta"
                   (:p (:strong "Site: ") (cl-who:fmt "~A (~A)" 
                                                     (getf site :|name|)
                                                     (getf site :|code|)))
                   (when (and contract-week (> (length contract-week) 0))
                     (cl-who:htm
                      (:p (:strong "Contract Week: ") (cl-who:str contract-week))))
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
        (let* ((current-period (or (get-system-setting "contract_current_period") "BY"))
               (available-weeks (get-available-contract-weeks current-period)))
          (html-response
           (render-page "Reports"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "Reports"))
               
               ;; Quick links to other reports
               (:div :class "card" :style "margin-bottom: 1rem;"
                 (:h2 "Report Types")
                 (:div :style "display: flex; gap: 1rem; flex-wrap: wrap;"
                   (:a :href "/reports" :class "btn btn-primary" "Inspection Reports")
                   (:a :href "/reports/inventory-issues" :class "btn btn-secondary" "Inventory Issues")))
               
               (:div :class "card"
                 (:h2 "Generate Inspection Report")
                 (:p "Select a site and either a contract week OR a custom date range.")
                 (:form :method "get" :action "/reports" :class "report-form" :id "report-form"
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
                   
                   ;; Contract Week Selection
                   (:div :class "form-row" :style "background: #f8f9fa; padding: 1rem; border-radius: 4px; margin-bottom: 1rem;"
                     (:div :class "form-group"
                       (:label (:strong "Option 1: Contract Week"))
                       (:select :name "contract_week" :id "contract-week-select" :onchange "updateDatesFromWeek()"
                         (:option :value "" "-- Select Week --")
                         (dolist (week available-weeks)
                           (cl-who:htm
                            (:option :value (format nil "~A-~A" current-period (getf week :week-number))
                                     :data-start (getf week :week-start)
                                     :data-end (getf week :week-end)
                                     (cl-who:fmt "~A Week ~A (~A to ~A)" 
                                                 current-period 
                                                 (getf week :week-number)
                                                 (getf week :week-start)
                                                 (getf week :week-end))))))))
                   
                   ;; Custom Date Range
                   (:div :class "form-row"
                     (:div :class "form-group"
                       (:label (:strong "Option 2: Custom Date Range")))
                     (:div :class "form-group"
                       (:label "From Date")
                       (:input :type "date" :name "date_from" :id "date-from"))
                     (:div :class "form-group"
                       (:label "To Date")
                       (:input :type "date" :name "date_to" :id "date-to"))
                     (:div :class "form-group"
                       (:label "Status (optional)")
                       (:select :name "status"
                         (:option :value "" "All Statuses")
                         (dolist (st *wo-statuses*)
                           (cl-who:htm
                            (:option :value st (cl-who:str st)))))))
                   (:div :class "form-actions"
                     (:button :type "submit" :class "btn btn-primary" "Generate Report"))))
               
               (:script "
function updateDatesFromWeek() {
  var select = document.getElementById('contract-week-select');
  var option = select.options[select.selectedIndex];
  if (option.value) {
    document.getElementById('date-from').value = option.dataset.start;
    document.getElementById('date-to').value = option.dataset.end;
  }
}
"))))))))

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


;;; Inspection Report Handlers - moved to inspection-reports.lisp

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
      ((string= uri "/inventory/new")
       (handle-inventory-new))
      ((string= uri "/inventory/locations")
       (handle-inventory-locations))
      ((cl-ppcre:scan "^/inventory/item/(\\d+)/edit$" uri)
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/inventory/item/(\\d+)/edit$" uri)
         (declare (ignore match))
         (handle-inventory-item-edit (aref groups 0))))
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
      ((string= uri "/reports/inventory-issues")
       (handle-inventory-issue-report))
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
      
      ;; Inventory API endpoints
      ((and (eq method :post) (string= uri "/api/inventory/item/create"))
       (handle-api-inventory-item-create))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inventory/item/(\\d+)/update$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inventory/item/(\\d+)/update$" uri)
         (declare (ignore match))
         (handle-api-inventory-item-update (aref groups 0))))
      ((and (eq method :post) (cl-ppcre:scan "^/api/inventory/item/(\\d+)/adjust$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/inventory/item/(\\d+)/adjust$" uri)
         (declare (ignore match))
         (handle-api-inventory-item-adjust (aref groups 0))))
      
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
