(in-package #:tfs-cmms)

;;; Admin Panel Handlers
;;; Handles user management, system settings, and admin reports

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
                   
                   (:hr)
                   (:h3 "Contract Period Settings")
                   (:p :class "text-muted" "Configure contract periods for week-based reporting. Reports can be generated by contract week number (e.g., BY-Week12).")
                   
                   (:div :class "form-row"
                     (:div :class "form-group"
                       (:label "Current Contract Period")
                       (:select :name "contract_current_period"
                         (:option :value "BY" :selected (string= "BY" (or (get-system-setting "contract_current_period") "BY")) "BY - Base Year")
                         (:option :value "OY" :selected (string= "OY" (get-system-setting "contract_current_period")) "OY - Option Year")
                         (:option :value "EX" :selected (string= "EX" (get-system-setting "contract_current_period")) "EX - Extension"))))
                   
                   (:div :class "form-row"
                     (:div :class "form-group"
                       (:label "Base Year Start Date")
                       (:input :type "date" :name "contract_base_year_start"
                               :value (or (get-system-setting "contract_base_year_start") "")))
                     (:div :class "form-group"
                       (:label "Base Year End Date")
                       (:input :type "date" :name "contract_base_year_end"
                               :value (or (get-system-setting "contract_base_year_end") ""))))
                   
                   (:div :class "form-row"
                     (:div :class "form-group"
                       (:label "Option Year Start Date")
                       (:input :type "date" :name "contract_option_year_start"
                               :value (or (get-system-setting "contract_option_year_start") "")))
                     (:div :class "form-group"
                       (:label "Option Year End Date (optional)")
                       (:input :type "date" :name "contract_option_year_end"
                               :value (or (get-system-setting "contract_option_year_end") ""))))
                   
                   (:p :class "text-muted" "Week 1 starts on the contract start date and ends on the following Saturday. Subsequent weeks run Sunday to Saturday.")
                   
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
        (let ((contract-number (hunchentoot:parameter "contract_number"))
              (current-period (hunchentoot:parameter "contract_current_period"))
              (by-start (hunchentoot:parameter "contract_base_year_start"))
              (by-end (hunchentoot:parameter "contract_base_year_end"))
              (oy-start (hunchentoot:parameter "contract_option_year_start"))
              (oy-end (hunchentoot:parameter "contract_option_year_end")))
          (when contract-number
            (set-system-setting "contract_number" contract-number "Current contract number for MRF forms"))
          (when current-period
            (set-system-setting "contract_current_period" current-period "Current contract period: BY (Base Year), OY (Option Year), EX (Extension)"))
          (when by-start
            (set-system-setting "contract_base_year_start" by-start "Base Year contract start date (YYYY-MM-DD)"))
          (when by-end
            (set-system-setting "contract_base_year_end" by-end "Base Year contract end date (YYYY-MM-DD)"))
          (when oy-start
            (set-system-setting "contract_option_year_start" oy-start "Option Year contract start date (YYYY-MM-DD)"))
          (when oy-end
            (set-system-setting "contract_option_year_end" oy-end "Option Year contract end date (YYYY-MM-DD)"))
          (redirect-to "/admin/settings?success=Settings%20updated"))
        (redirect-to "/unauthorized"))))
