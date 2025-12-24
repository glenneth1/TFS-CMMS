(in-package #:tfs-cmms)

;;; Daily Activity Report (DAR) and Immediate Repair Package (IRP) Module

;;; Permission Helpers

(defun user-can-view-dar-p (user dar)
  "Check if user can view a DAR."
  (when (and user dar)
    (let ((role (string-downcase (or (getf user :|role|) "")))
          (user-id (getf user :|id|))
          (creator-id (getf dar :|submitted_by|)))
      (or (= user-id creator-id)
          (user-is-admin-p user)
          (string= role "program_manager")
          (string= role "project_manager")
          (string= role "qc_manager")
          (string= role "ao_lead")
          (string= role "pmo")))))

(defun user-can-view-irp-p (user irp)
  "Check if user can view an IRP."
  (when (and user irp)
    (let ((role (string-downcase (or (getf user :|role|) "")))
          (user-id (getf user :|id|))
          (creator-id (getf irp :|submitted_by|)))
      (or (= user-id creator-id)
          (user-is-admin-p user)
          (string= role "program_manager")
          (string= role "project_manager")
          (string= role "qc_manager")
          (string= role "ao_lead")
          (string= role "pmo")
          (string= role "property_manager")
          (string= role "materials_supervisor")
          (string= role "materials_specialist")))))

(defun user-can-submit-dar-p (user)
  "Check if user can submit DARs (electricians and inspectors)."
  (when user
    (let ((role (string-downcase (or (getf user :|role|) ""))))
      (or (string= role "electrician")
          (string= role "master_electrician")
          (string= role "journeyman")
          (string= role "inspector")))))

;;; Database Functions

(defun get-dar-list (&key user-id team-number date-from date-to status limit)
  "Get list of DARs with optional filters."
  (let ((conditions '("1=1"))
        (params '()))
    (when user-id
      (push "submitted_by = ?" conditions)
      (push user-id params))
    (when team-number
      (push "(d.team_number = ? OR d.team_number = ?)" conditions)
      (push team-number params)
      (push (format nil "Team ~A" team-number) params))
    (when date-from
      (push "report_date >= ?" conditions)
      (push date-from params))
    (when date-to
      (push "report_date <= ?" conditions)
      (push date-to params))
    (when status
      (push "status = ?" conditions)
      (push status params))
    (let ((sql (format nil "SELECT d.*, u.full_name as submitter_name
                            FROM daily_activity_reports d
                            JOIN users u ON d.submitted_by = u.id
                            WHERE ~{~A~^ AND ~}
                            ORDER BY report_date DESC, created_at DESC
                            ~@[LIMIT ~A~]"
                       (reverse conditions) limit)))
      (apply #'fetch-all sql (reverse params)))))

(defun get-dar-by-id (id)
  "Get a single DAR by ID with submitter info."
  (fetch-one "SELECT d.*, u.full_name as submitter_name
              FROM daily_activity_reports d
              JOIN users u ON d.submitted_by = u.id
              WHERE d.id = ?" id))

(defun get-dar-activities (dar-id)
  "Get all activities for a DAR."
  (fetch-all "SELECT * FROM dar_activities WHERE dar_id = ? ORDER BY id" dar-id))

(defun get-dar-reports-submitted (dar-id)
  "Get all reports submitted entries for a DAR."
  (fetch-all "SELECT * FROM dar_reports_submitted WHERE dar_id = ? ORDER BY id" dar-id))

(defun create-dar (user-id report-date team-number team-member-name team-mate-name 
                   camp-location site-lead toolbox-topic attended-safety-brief hecp-submitted)
  "Create a new DAR and return its ID."
  (execute-sql "INSERT INTO daily_activity_reports 
                (report_date, team_number, team_member_name, team_mate_name, camp_location,
                 site_lead, toolbox_topic, attended_safety_brief, hecp_submitted, submitted_by, status)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'Draft')"
               report-date team-number team-member-name (or team-mate-name "")
               camp-location (or site-lead "") (or toolbox-topic "")
               (if attended-safety-brief 1 0) (if hecp-submitted 1 0) user-id)
  (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))

(defun add-dar-activity (dar-id activity-time activity-description notes)
  "Add an activity entry to a DAR."
  (execute-sql "INSERT INTO dar_activities (dar_id, activity_time, activity_description, notes)
                VALUES (?, ?, ?, ?)"
               dar-id (or activity-time "") (or activity-description "") (or notes "")))

(defun add-dar-report-submitted (dar-id report-tag-id submitted)
  "Add a report submitted entry to a DAR."
  (execute-sql "INSERT INTO dar_reports_submitted (dar_id, report_tag_id, submitted)
                VALUES (?, ?, ?)"
               dar-id (or report-tag-id "") (if submitted 1 0)))

(defun update-dar-status (dar-id status)
  "Update DAR status."
  (execute-sql "UPDATE daily_activity_reports SET status = ?, updated_at = datetime('now') WHERE id = ?"
               status dar-id))

;;; IRP Database Functions

(defun get-irp-standard-items ()
  "Get all active IRP standard items."
  (fetch-all "SELECT * FROM irp_standard_items WHERE active = 1 ORDER BY item_number"))

(defun get-irp-list (&key user-id team-number date-from date-to limit)
  "Get list of IRP submissions with optional filters."
  (let ((conditions '("1=1"))
        (params '()))
    (when user-id
      (push "submitted_by = ?" conditions)
      (push user-id params))
    (when team-number
      (push "(i.team_number = ? OR i.team_number = ?)" conditions)
      (push team-number params)
      (push (format nil "Team ~A" team-number) params))
    (when date-from
      (push "submission_date >= ?" conditions)
      (push date-from params))
    (when date-to
      (push "submission_date <= ?" conditions)
      (push date-to params))
    (let ((sql (format nil "SELECT i.*, u.full_name as submitter_name
                            FROM irp_submissions i
                            JOIN users u ON i.submitted_by = u.id
                            WHERE ~{~A~^ AND ~}
                            ORDER BY submission_date DESC, created_at DESC
                            ~@[LIMIT ~A~]"
                       (reverse conditions) limit)))
      (apply #'fetch-all sql (reverse params)))))

(defun get-irp-by-id (id)
  "Get a single IRP submission by ID."
  (fetch-one "SELECT i.*, u.full_name as submitter_name
              FROM irp_submissions i
              JOIN users u ON i.submitted_by = u.id
              WHERE i.id = ?" id))

(defun get-irp-submission-items (submission-id)
  "Get all items for an IRP submission with standard item details."
  (fetch-all "SELECT si.*, s.description, s.make, s.part_number, s.uom, s.std_issue_qty, s.item_number
              FROM irp_submission_items si
              JOIN irp_standard_items s ON si.irp_item_id = s.id
              WHERE si.submission_id = ?
              ORDER BY s.item_number" submission-id))

(defun create-irp-submission (user-id submission-date team-number camp-location notes)
  "Create a new IRP submission and return its ID."
  (execute-sql "INSERT INTO irp_submissions 
                (submission_date, team_number, camp_location, submitted_by, status, notes)
                VALUES (?, ?, ?, ?, 'Submitted', ?)"
               submission-date team-number camp-location user-id (or notes ""))
  (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))

(defun add-irp-submission-item (submission-id irp-item-id issued-qty current-qty)
  "Add an item to an IRP submission."
  (execute-sql "INSERT INTO irp_submission_items (submission_id, irp_item_id, issued_qty, current_qty)
                VALUES (?, ?, ?, ?)"
               submission-id irp-item-id issued-qty current-qty))

(defun get-irp-discrepancies (submission-id)
  "Get items where current qty differs from issued qty."
  (fetch-all "SELECT si.*, s.description, s.make, s.part_number, s.uom, s.std_issue_qty, s.item_number,
                     (si.issued_qty - si.current_qty) as difference
              FROM irp_submission_items si
              JOIN irp_standard_items s ON si.irp_item_id = s.id
              WHERE si.submission_id = ? AND si.issued_qty != si.current_qty
              ORDER BY s.item_number" submission-id))

;;; Route Handlers

(defun handle-dar-list ()
  "Display list of DARs."
  (let* ((user (get-current-user))
         (role (when user (string-downcase (or (getf user :|role|) ""))))
         (user-id (when user (getf user :|id|)))
         (user-team (when user (getf user :|team_number|)))
         (is-admin (and user (eql 1 (getf user :|is_admin|))))
         (team-filter (get-param "team"))
         (date-from (get-param "date_from"))
         (date-to (get-param "date_to"))
         (contract-week (get-param "contract_week"))
         (current-period (or (get-system-setting "contract_current_period") "BY"))
         (available-weeks (get-available-contract-weeks current-period)))
    (if user
        (let* ((is-electrician (or (string= role "electrician")
                                   (string= role "master_electrician")
                                   (string= role "journeyman")
                                   (string= role "inspector")))
               ;; Non-admin electricians see only their team's DARs
               (effective-team (if (and is-electrician (not is-admin))
                                   user-team
                                   (when (and team-filter (not (string= team-filter ""))) team-filter)))
               (dars (get-dar-list :team-number effective-team
                                   :date-from date-from :date-to date-to)))
          (html-response
           (render-page "Daily Activity Reports"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "Daily Activity Reports")
                 (when is-electrician
                   (cl-who:htm
                    (:a :href "/dar/new" :class "btn btn-primary" "+ New DAR"))))
               ;; Filter form
               (:section :class "card" :style "margin-bottom: 1rem;"
                 (:h3 "Filter by Date Range")
                 (:form :method "get" :action "/dar"
                   (:div :class "form-row"
                     (:div :class "form-group"
                       (:label "Contract Week")
                       (:select :name "contract_week" :id "dar-contract-week" :onchange "updateDarDatesFromWeek()"
                         (:option :value "" "-- Select Week --")
                         (dolist (week available-weeks)
                           (cl-who:htm
                            (:option :value (format nil "~A-~A" current-period (getf week :week-number))
                                     :data-start (getf week :week-start)
                                     :data-end (getf week :week-end)
                                     :selected (string= contract-week (format nil "~A-~A" current-period (getf week :week-number)))
                                     (cl-who:fmt "~A Week ~A (~A to ~A)" 
                                                 current-period 
                                                 (getf week :week-number)
                                                 (getf week :week-start)
                                                 (getf week :week-end)))))))
                     (:div :class "form-group"
                       (:label "From Date")
                       (:input :type "date" :name "date_from" :id "dar-date-from" :value (or date-from "")))
                     (:div :class "form-group"
                       (:label "To Date")
                       (:input :type "date" :name "date_to" :id "dar-date-to" :value (or date-to "")))
                     (:div :class "form-group" :style "align-self: flex-end;"
                       (:button :type "submit" :class "btn btn-primary" "Filter")
                       (:a :href "/dar" :class "btn" :style "margin-left: 0.5rem;" "Clear"))))
                 (:script "
function updateDarDatesFromWeek() {
  var select = document.getElementById('dar-contract-week');
  var option = select.options[select.selectedIndex];
  if (option.value) {
    document.getElementById('dar-date-from').value = option.dataset.start;
    document.getElementById('dar-date-to').value = option.dataset.end;
  }
}
"))
               (:section :class "card"
                 (:table :class "data-table"
                   (:thead
                     (:tr
                       (:th "Date")
                       (:th "Team")
                       (:th "Location")
                       (:th "Team Member")
                       (:th "Status")
                       (:th "Actions")))
                   (:tbody
                     (if dars
                         (dolist (dar dars)
                           (cl-who:htm
                            (:tr
                              (:td (cl-who:str (format-date-display (getf dar :|report_date|))))
                              (:td (cl-who:str (getf dar :|team_number|)))
                              (:td (cl-who:str (getf dar :|camp_location|)))
                              (:td (cl-who:str (getf dar :|team_member_name|)))
                              (:td (:span :class "badge" (cl-who:str (getf dar :|status|))))
                              (:td (:a :href (format nil "/dar/~A" (getf dar :|id|))
                                       :class "btn btn-sm" "View")))))
                         (cl-who:htm
                          (:tr (:td :colspan "6" :class "text-center" "No DARs found")))))))))))
        (redirect-to "/login"))))

(defun handle-dar-new ()
  "Display new DAR form."
  (let ((user (get-current-user)))
    (if (and user (user-can-submit-dar-p user))
        (let ((today (multiple-value-bind (sec min hour day month year)
                         (decode-universal-time (get-universal-time))
                       (declare (ignore sec min hour))
                       (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))
              (team-number (or (getf user :|team_number|) ""))
              (full-name (getf user :|full_name|))
              (camps (fetch-all "SELECT DISTINCT name FROM camps WHERE name != 'Unknown' ORDER BY name")))
          (html-response
           (render-page "New Daily Activity Report"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "New Daily Activity Report")
                 (:a :href "/dar" :class "btn" "Back to List"))
               (:form :method "post" :action "/api/dar/create" :class "form-card"
                 (:section :class "card"
                   (:h2 "Report Details")
                   (:div :class "form-row"
                     (:div :class "form-group required"
                       (:label "Date")
                       (:input :type "date" :name "report_date" :required t :value today))
                     (:div :class "form-group required"
                       (:label "Camp / Location")
                       (:select :name "camp_location" :required t
                         (:option :value "" "-- Select Location --")
                         (dolist (camp camps)
                           (cl-who:htm
                            (:option :value (getf camp :|name|) (cl-who:str (getf camp :|name|))))))))
                   (:div :class "form-row"
                     (:div :class "form-group required"
                       (:label "Team Member Name")
                       (:input :type "text" :name "team_member_name" :required t :value full-name))
                     (:div :class "form-group"
                       (:label "Team Mate Name")
                       (:input :type "text" :name "team_mate_name")))
                   (:div :class "form-row"
                     (:div :class "form-group required"
                       (:label "Team #")
                       (:input :type "text" :name "team_number" :required t :value team-number))
                     (:div :class "form-group"
                       (:label "Site Lead")
                       (:input :type "text" :name "site_lead"))))
                 (:section :class "card"
                   (:h2 "Safety")
                   (:div :class "form-row"
                     (:div :class "form-group"
                       (:label "Toolbox Topic")
                       (:input :type "text" :name "toolbox_topic"))
                     (:div :class "form-group"
                       (:label "Attended Safety Brief?")
                       (:select :name "attended_safety_brief"
                         (:option :value "1" "Yes")
                         (:option :value "0" "No"))))
                   (:div :class "form-group"
                     (:label
                       (:input :type "checkbox" :name "hecp_submitted" :value "1")
                       " HECP Package Submitted")))
                 (:section :class "card"
                   (:h2 "Activity Record")
                   (:p :class "text-muted" "Record your activities throughout the day. Click 'Add Activity' to add more rows.")
                   (:div :id "activities-container"
                     (:div :class "activity-row form-row"
                       (:div :class "form-group" :style "width: 100px;"
                         (:label "Time")
                         (:select :name "activity_time[]" :class "time-select"
                           (:option :value "" "-- Time --")
                           (loop for hour from 0 to 23
                                 do (loop for min in '(0 30)
                                          do (cl-who:htm
                                              (:option :value (format nil "~2,'0D:~2,'0D" hour min)
                                                       (cl-who:str (format nil "~2,'0D:~2,'0D" hour min))))))))
                       (:div :class "form-group" :style "flex: 2;"
                         (:label "Activity")
                         (:input :type "text" :name "activity_desc[]" :placeholder "e.g., TEAM MEETING"))
                       (:div :class "form-group" :style "flex: 1;"
                         (:label "Notes")
                         (:input :type "text" :name "activity_notes[]" :placeholder "Optional notes"))))
                   (:button :type "button" :class "btn" :onclick "addActivityRow()" "+ Add Activity"))
                 (:section :class "card"
                   (:h2 "Reports Submitted to QA/QC")
                   (:p :class "text-muted" "List any inspection reports submitted today.")
                   (:div :id "reports-container"
                     (:div :class "report-row form-row"
                       (:div :class "form-group" :style "flex: 3;"
                         (:label "Report TAG ID")
                         (:input :type "text" :name "report_tag[]" :placeholder "e.g., F797V-B123-001"))
                       (:div :class "form-group" :style "width: 120px;"
                         (:label "Submitted?")
                         (:select :name "report_submitted[]"
                           (:option :value "1" "Yes")
                           (:option :value "0" "No")))))
                   (:button :type "button" :class "btn" :onclick "addReportRow()" "+ Add Report"))
                 (:div :class "form-actions"
                   (:button :type "submit" :class "btn btn-primary" "Submit DAR")
                   (:a :href "/dar" :class "btn" "Cancel"))
                 (:script :type "text/javascript"
                   (cl-who:str "
var timeOptions = '<option value=\"\">-- Time --</option>';
for (var h = 0; h < 24; h++) {
  for (var m = 0; m < 60; m += 30) {
    var hh = h.toString().padStart(2, '0');
    var mm = m.toString().padStart(2, '0');
    timeOptions += '<option value=\"' + hh + ':' + mm + '\">' + hh + ':' + mm + '</option>';
  }
}
function addActivityRow() {
  var container = document.getElementById('activities-container');
  var row = document.createElement('div');
  row.className = 'activity-row form-row';
  row.innerHTML = '<div class=\"form-group\" style=\"width: 100px;\"><select name=\"activity_time[]\" class=\"time-select\">' + timeOptions + '</select></div>' +
                  '<div class=\"form-group\" style=\"flex: 2;\"><input type=\"text\" name=\"activity_desc[]\" placeholder=\"e.g., LUNCH\"></div>' +
                  '<div class=\"form-group\" style=\"flex: 1;\"><input type=\"text\" name=\"activity_notes[]\" placeholder=\"Optional\"></div>' +
                  '<button type=\"button\" class=\"btn btn-sm\" onclick=\"this.parentNode.remove()\" style=\"margin-top: 5px;\">Remove</button>';
  container.appendChild(row);
}
function addReportRow() {
  var container = document.getElementById('reports-container');
  var row = document.createElement('div');
  row.className = 'report-row form-row';
  row.innerHTML = '<div class=\"form-group\" style=\"flex: 3;\"><input type=\"text\" name=\"report_tag[]\" placeholder=\"e.g., F797V-B123-001\"></div>' +
                  '<div class=\"form-group\" style=\"width: 120px;\"><select name=\"report_submitted[]\"><option value=\"1\">Yes</option><option value=\"0\">No</option></select></div>' +
                  '<button type=\"button\" class=\"btn btn-sm\" onclick=\"this.parentNode.remove()\" style=\"margin-top: 5px;\">Remove</button>';
  container.appendChild(row);
}
")))))))
        (redirect-to "/unauthorized"))))

(defun handle-api-dar-create ()
  "Handle DAR creation."
  (let ((user (get-current-user)))
    (if (and user (user-can-submit-dar-p user))
        (let* ((user-id (getf user :|id|))
               (report-date (get-param "report_date"))
               (team-number (get-param "team_number"))
               (team-member-name (get-param "team_member_name"))
               (team-mate-name (get-param "team_mate_name"))
               (camp-location (get-param "camp_location"))
               (site-lead (get-param "site_lead"))
               (toolbox-topic (get-param "toolbox_topic"))
               (attended-safety-brief (string= (get-param "attended_safety_brief") "1"))
               (hecp-submitted (get-param "hecp_submitted"))
               ;; Get arrays of activities and reports
               (activity-times (hunchentoot:post-parameters* "activity_time[]"))
               (activity-descs (hunchentoot:post-parameters* "activity_desc[]"))
               (activity-notes (hunchentoot:post-parameters* "activity_notes[]"))
               (report-tags (hunchentoot:post-parameters* "report_tag[]"))
               (report-submitted-flags (hunchentoot:post-parameters* "report_submitted[]")))
          (let ((dar-id (create-dar user-id report-date team-number team-member-name team-mate-name
                                    camp-location site-lead toolbox-topic attended-safety-brief
                                    (and hecp-submitted (string= hecp-submitted "1")))))
            ;; Add activities
            (loop for i from 0 below (length activity-times)
                  for time = (nth i activity-times)
                  for desc = (nth i activity-descs)
                  for notes = (nth i activity-notes)
                  when (and desc (not (string= desc "")))
                  do (add-dar-activity dar-id time desc notes))
            ;; Add reports submitted
            (loop for i from 0 below (length report-tags)
                  for tag = (nth i report-tags)
                  for submitted = (nth i report-submitted-flags)
                  when (and tag (not (string= tag "")))
                  do (add-dar-report-submitted dar-id tag (string= submitted "1")))
            (update-dar-status dar-id "Submitted")
            (redirect-to (format nil "/dar/~A" dar-id))))
        (redirect-to "/unauthorized"))))

(defun handle-dar-detail (id-str)
  "Display DAR detail view."
  (let* ((user (get-current-user))
         (dar-id (parse-int id-str))
         (dar (when dar-id (get-dar-by-id dar-id))))
    (if (and user dar (user-can-view-dar-p user dar))
        (let ((activities (get-dar-activities dar-id))
              (reports-submitted (get-dar-reports-submitted dar-id)))
          (html-response
           (render-page "Daily Activity Report"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 (cl-who:fmt "DAR - ~A" (format-date-display (getf dar :|report_date|))))
                 (:div
                   (:a :href "/dar" :class "btn" "Back to List")
                   (:a :href (format nil "/dar/~A/pdf" dar-id) :class "btn btn-primary" "Download PDF")))
               (:section :class "card"
                 (:h2 "Report Details")
                 (:dl :class "detail-list"
                   (:dt "Date") (:dd (cl-who:str (format-date-display (getf dar :|report_date|))))
                   (:dt "Location") (:dd (cl-who:str (getf dar :|camp_location|)))
                   (:dt "Team #") (:dd (cl-who:str (getf dar :|team_number|)))
                   (:dt "Team Member") (:dd (cl-who:str (getf dar :|team_member_name|)))
                   (:dt "Team Mate") (:dd (cl-who:str (or (getf dar :|team_mate_name|) "-")))
                   (:dt "Status") (:dd (:span :class "badge" (cl-who:str (getf dar :|status|))))))
               (:section :class "card"
                 (:h2 "Safety")
                 (:dl :class "detail-list"
                   (:dt "Toolbox Topic") (:dd (cl-who:str (or (getf dar :|toolbox_topic|) "-")))
                   (:dt "Safety Brief") (:dd (cl-who:str (if (= 1 (getf dar :|attended_safety_brief|)) "Yes" "No")))
                   (:dt "HECP Submitted") (:dd (cl-who:str (if (= 1 (getf dar :|hecp_submitted|)) "Yes" "No")))))
               (when activities
                 (cl-who:htm
                  (:section :class "card"
                    (:h2 "Activities")
                    (:table :class "data-table"
                      (:thead (:tr (:th "Time") (:th "Activity") (:th "Notes")))
                      (:tbody
                        (dolist (act activities)
                          (cl-who:htm
                           (:tr
                             (:td (cl-who:str (or (getf act :|activity_time|) "-")))
                             (:td (cl-who:str (or (getf act :|activity_description|) "-")))
                             (:td (cl-who:str (or (getf act :|notes|) "-")))))))))))))))
        (redirect-to "/dar"))))

(defun handle-dar-pdf (id-str)
  "Generate and serve DAR PDF."
  (let* ((user (get-current-user))
         (dar-id (parse-int id-str))
         (dar (when dar-id (get-dar-by-id dar-id))))
    (if (and user dar (user-can-view-dar-p user dar))
        (let* ((python-path *python-path*)
               (script-path (namestring (merge-pathnames "scripts/generate_dar_pdf.py" *base-directory*)))
               (reports-dir (namestring (merge-pathnames "reports/dar/" *base-directory*))))
          ;; Generate PDF
          (uiop:run-program (list python-path script-path (write-to-string dar-id) reports-dir)
                            :output t :error-output t)
          ;; Build filename
          (let* ((date-str (getf dar :|report_date|))
                 (team (getf dar :|team_number|))
                 (location (getf dar :|camp_location|))
                 (site-code (cond ((search "ERBIL" (string-upcase (or location ""))) "EAB")
                                  ((search "GREEN VILLAGE" (string-upcase (or location ""))) "GV")
                                  ((search "AL ASAD" (string-upcase (or location ""))) "AAS")
                                  (t (subseq (string-upcase (or location "UNK")) 0 (min 3 (length (or location "UNK")))))))
                 (date-part (if (and date-str (>= (length date-str) 10))
                                (let* ((year (subseq date-str 2 4))
                                       (month-num (parse-integer (subseq date-str 5 7)))
                                       (day (subseq date-str 8 10))
                                       (month-abbrev (nth (1- month-num) '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" 
                                                                            "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))))
                                  (format nil "~A~A~A" day month-abbrev year))
                                "NODATE"))
                 (filename (format nil "DAR_~A_~A_~A.pdf" site-code date-part team))
                 (filepath (merge-pathnames filename (merge-pathnames "reports/dar/" *base-directory*))))
            (if (probe-file filepath)
                (progn
                  (setf (hunchentoot:content-type*) "application/pdf")
                  (setf (hunchentoot:header-out :content-disposition)
                        (format nil "attachment; filename=\"~A\"" filename))
                  (with-open-file (stream filepath :element-type '(unsigned-byte 8))
                    (let ((buffer (make-array (file-length stream) :element-type '(unsigned-byte 8))))
                      (read-sequence buffer stream)
                      buffer)))
                (redirect-to (format nil "/dar/~A" dar-id)))))
        (redirect-to "/dar"))))

;;; IRP Route Handlers

(defun handle-irp-list ()
  "Display list of IRP submissions."
  (let* ((user (get-current-user))
         (role (when user (string-downcase (or (getf user :|role|) ""))))
         (user-id (when user (getf user :|id|)))
         (user-team (when user (getf user :|team_number|)))
         (is-admin (and user (eql 1 (getf user :|is_admin|))))
         (team-filter (get-param "team")))
    (if user
        (let* ((is-electrician (or (string= role "electrician")
                                   (string= role "master_electrician")
                                   (string= role "journeyman")
                                   (string= role "inspector")))
               ;; Non-admin electricians see only their team's IRPs
               (effective-team (if (and is-electrician (not is-admin))
                                   user-team
                                   (when (and team-filter (not (string= team-filter ""))) team-filter)))
               (irps (get-irp-list :team-number effective-team)))
          (html-response
           (render-page "Immediate Repair Package (IRP)"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "Immediate Repair Package (IRP)")
                 (when is-electrician
                   (cl-who:htm
                    (:a :href "/irp/new" :class "btn btn-primary" "+ New IRP Report"))))
               (:section :class "card"
                 (:table :class "data-table"
                   (:thead
                     (:tr
                       (:th "Date")
                       (:th "Team")
                       (:th "Location")
                       (:th "Submitted By")
                       (:th "Discrepancies")
                       (:th "Actions")))
                   (:tbody
                     (if irps
                         (dolist (irp irps)
                           (let ((discrepancies (get-irp-discrepancies (getf irp :|id|))))
                             (cl-who:htm
                              (:tr
                                (:td (cl-who:str (format-date-display (getf irp :|submission_date|))))
                                (:td (cl-who:str (getf irp :|team_number|)))
                                (:td (cl-who:str (getf irp :|camp_location|)))
                                (:td (cl-who:str (getf irp :|submitter_name|)))
                                (:td (if discrepancies
                                         (cl-who:htm (:span :class "badge badge-warning" 
                                                            (cl-who:str (format nil "~A" (length discrepancies)))))
                                         (cl-who:htm (:span :class "badge badge-success" "None"))))
                                (:td (:a :href (format nil "/irp/~A" (getf irp :|id|))
                                         :class "btn btn-sm" "View"))))))
                         (cl-who:htm
                          (:tr (:td :colspan "6" :class "text-center" "No IRP submissions found")))))))))))
        (redirect-to "/login"))))

(defun handle-irp-new ()
  "Display new IRP form."
  (let ((user (get-current-user)))
    (if (and user (user-can-submit-dar-p user))
        (let ((today (multiple-value-bind (sec min hour day month year)
                         (decode-universal-time (get-universal-time))
                       (declare (ignore sec min hour))
                       (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))
              (team-number (or (getf user :|team_number|) ""))
              (camps (fetch-all "SELECT DISTINCT name FROM camps WHERE name != 'Unknown' ORDER BY name"))
              (standard-items (get-irp-standard-items)))
          (html-response
           (render-page "New IRP Report"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "Immediate Repair Package - Inventory")
                 (:a :href "/irp" :class "btn" "Back to List"))
               (:form :method "post" :action "/api/irp/create" :class "form-card"
                 (:section :class "card"
                   (:h2 "Report Details")
                   (:div :class "form-row"
                     (:div :class "form-group required"
                       (:label "Inventory Date")
                       (:input :type "date" :name "submission_date" :required t :value today))
                     (:div :class "form-group required"
                       (:label "Site")
                       (:select :name "camp_location" :required t
                         (:option :value "" "-- Select Site --")
                         (dolist (camp camps)
                           (cl-who:htm
                            (:option :value (getf camp :|name|) (cl-who:str (getf camp :|name|))))))))
                   (:div :class "form-row"
                     (:div :class "form-group required"
                       (:label "Inventory By (Team)")
                       (:input :type "text" :name "team_number" :required t :value team-number))
                     (:div :class "form-group"
                       (:label "Notes")
                       (:input :type "text" :name "notes"))))
                 (:section :class "card"
                   (:h2 "Inventory Items")
                   (:p :class "text-muted" "Update Current Qty for items that differ from standard.")
                   (:table :class "data-table"
                     (:thead
                       (:tr (:th "#") (:th "Description") (:th "UOM") (:th "Std") (:th "Issued") (:th "Current")))
                     (:tbody
                       (dolist (item standard-items)
                         (let ((item-id (getf item :|id|))
                               (std-qty (getf item :|std_issue_qty|)))
                           (cl-who:htm
                            (:tr
                              (:td (cl-who:str (getf item :|item_number|)))
                              (:td (cl-who:str (getf item :|description|)))
                              (:td (cl-who:str (getf item :|uom|)))
                              (:td (cl-who:str std-qty))
                              (:td (:input :type "number" :name (format nil "issued_~A" item-id) 
                                           :value std-qty :min "0" :style "width:60px;"))
                              (:td (:input :type "number" :name (format nil "current_~A" item-id)
                                           :value std-qty :min "0" :style "width:60px;")))))))))
                 (:div :class "form-actions"
                   (:button :type "submit" :class "btn btn-primary" "Submit IRP Report")
                   (:a :href "/irp" :class "btn" "Cancel")))))))
        (redirect-to "/unauthorized"))))

(defun handle-api-irp-create ()
  "Handle IRP submission creation."
  (let ((user (get-current-user)))
    (if (and user (user-can-submit-dar-p user))
        (let* ((user-id (getf user :|id|))
               (submission-date (get-param "submission_date"))
               (team-number (get-param "team_number"))
               (camp-location (get-param "camp_location"))
               (notes (get-param "notes"))
               (standard-items (get-irp-standard-items)))
          (let ((submission-id (create-irp-submission user-id submission-date team-number camp-location notes)))
            (dolist (item standard-items)
              (let* ((item-id (getf item :|id|))
                     (issued-str (get-param (format nil "issued_~A" item-id)))
                     (current-str (get-param (format nil "current_~A" item-id)))
                     (issued-qty (if issued-str (parse-int issued-str) (getf item :|std_issue_qty|)))
                     (current-qty (if current-str (parse-int current-str) (getf item :|std_issue_qty|))))
                (add-irp-submission-item submission-id item-id issued-qty current-qty)))
            (redirect-to (format nil "/irp/~A" submission-id))))
        (redirect-to "/unauthorized"))))

(defun handle-irp-detail (id-str)
  "Display IRP submission detail view."
  (let* ((user (get-current-user))
         (irp-id (parse-int id-str))
         (irp (when irp-id (get-irp-by-id irp-id))))
    (if (and user irp (user-can-view-irp-p user irp))
        (let ((items (get-irp-submission-items irp-id))
              (discrepancies (get-irp-discrepancies irp-id)))
          (html-response
           (render-page "IRP Report"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 (cl-who:fmt "IRP - ~A" (format-date-display (getf irp :|submission_date|))))
                 (:a :href "/irp" :class "btn" "Back to List"))
               (:section :class "card"
                 (:h2 "Report Details")
                 (:dl :class "detail-list"
                   (:dt "Site") (:dd (cl-who:str (getf irp :|camp_location|)))
                   (:dt "Team") (:dd (cl-who:str (getf irp :|team_number|)))
                   (:dt "Date") (:dd (cl-who:str (format-date-display (getf irp :|submission_date|))))
                   (:dt "Submitted By") (:dd (cl-who:str (getf irp :|submitter_name|)))))
               (when discrepancies
                 (cl-who:htm
                  (:section :class "card" :style "border-left: 4px solid #ffc107;"
                    (:h2 "Quantity Discrepancies")
                    (:table :class "data-table"
                      (:thead (:tr (:th "#") (:th "Description") (:th "Std") (:th "Issued") (:th "Current") (:th "Diff")))
                      (:tbody
                        (dolist (d discrepancies)
                          (cl-who:htm
                           (:tr :style "background-color: #fff3cd;"
                             (:td (cl-who:str (getf d :|item_number|)))
                             (:td (cl-who:str (getf d :|description|)))
                             (:td (cl-who:str (getf d :|std_issue_qty|)))
                             (:td (cl-who:str (getf d :|issued_qty|)))
                             (:td (cl-who:str (getf d :|current_qty|)))
                             (:td (:strong (cl-who:str (getf d :|difference|))))))))))))
               (:section :class "card"
                 (:h2 "Full Inventory")
                 (:table :class "data-table"
                   (:thead (:tr (:th "#") (:th "Description") (:th "UOM") (:th "Std") (:th "Issued") (:th "Current")))
                   (:tbody
                     (dolist (item items)
                       (let ((has-diff (/= (getf item :|issued_qty|) (getf item :|current_qty|))))
                         (cl-who:htm
                          (:tr :style (when has-diff "background-color: #fff3cd;")
                            (:td (cl-who:str (getf item :|item_number|)))
                            (:td (cl-who:str (getf item :|description|)))
                            (:td (cl-who:str (getf item :|uom|)))
                            (:td (cl-who:str (getf item :|std_issue_qty|)))
                            (:td (cl-who:str (getf item :|issued_qty|)))
                            (:td (cl-who:str (getf item :|current_qty|))))))))))))))
        (redirect-to "/irp"))))

;;; Site Activity Report (SAR) Handlers

(defun user-can-generate-sar-p (user)
  "Check if user can generate SAR (Admin, AO Lead, PMO)."
  (when user
    (let ((role (string-downcase (or (getf user :|role|) ""))))
      (or (user-is-admin-p user)
          (string= role "ao_lead")
          (string= role "pmo")
          (string= role "program_manager")
          (string= role "project_manager")))))

(defun handle-sar-form ()
  "Display SAR generation form."
  (let ((user (get-current-user)))
    (if (and user (user-can-generate-sar-p user))
        (let ((today (multiple-value-bind (sec min hour day month year)
                         (decode-universal-time (get-universal-time))
                       (declare (ignore sec min hour))
                       (format nil "~4,'0D-~2,'0D-~2,'0D" year month day))))
          (html-response
           (render-page "Site Activity Report"
             (cl-who:with-html-output-to-string (s)
               (:div :class "page-header"
                 (:h1 "Site Activity Report (SAR)")
                 (:a :href "/" :class "btn" "Back to Dashboard"))
               (:section :class "card"
                 (:h2 "Generate Consolidated Report")
                 (:p "Generate a multi-page PDF report showing all inspection activity across all sites for a given date.")
                 (:form :method "get" :action "/sar/generate" :class "form-inline"
                   (:div :class "form-group"
                     (:label "Report Date")
                     (:input :type "date" :name "date" :required t :value today))
                   (:button :type "submit" :class "btn btn-primary" "Generate SAR PDF")))))))
        (redirect-to "/unauthorized"))))

(defun handle-sar-generate ()
  "Generate and serve SAR PDF."
  (let ((user (get-current-user)))
    (if (and user (user-can-generate-sar-p user))
        (let* ((report-date (or (get-param "date")
                                (multiple-value-bind (sec min hour day month year)
                                    (decode-universal-time (get-universal-time))
                                  (declare (ignore sec min hour))
                                  (format nil "~4,'0D-~2,'0D-~2,'0D" year month day))))
               (python-path *python-path*)
               (script-path (namestring (merge-pathnames "scripts/generate_sar_pdf.py" *base-directory*)))
               (reports-dir (namestring (merge-pathnames "reports/sar/" *base-directory*))))
          ;; Ensure directory exists
          (ensure-directories-exist (merge-pathnames "reports/sar/" *base-directory*))
          ;; Generate PDF
          (uiop:run-program (list python-path script-path report-date reports-dir)
                            :output t :error-output t)
          ;; Build filename
          (let* ((date-part (if (and report-date (>= (length report-date) 10))
                                (let* ((year (subseq report-date 2 4))
                                       (month-num (parse-integer (subseq report-date 5 7)))
                                       (day (subseq report-date 8 10))
                                       (month-abbrev (nth (1- month-num) '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" 
                                                                            "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))))
                                  (format nil "~A-~A-~A" day month-abbrev year))
                                "NODATE"))
                 (filename (format nil "DAR_ALL_SITES_~A.pdf" date-part))
                 (filepath (merge-pathnames filename (merge-pathnames "reports/sar/" *base-directory*))))
            (if (probe-file filepath)
                (progn
                  (setf (hunchentoot:content-type*) "application/pdf")
                  (setf (hunchentoot:header-out :content-disposition)
                        (format nil "attachment; filename=\"~A\"" filename))
                  (with-open-file (stream filepath :element-type '(unsigned-byte 8))
                    (let ((buffer (make-array (file-length stream) :element-type '(unsigned-byte 8))))
                      (read-sequence buffer stream)
                      buffer)))
                (redirect-to "/sar"))))
        (redirect-to "/unauthorized"))))
