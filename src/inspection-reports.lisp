;;;; inspection-reports.lisp - Inspection Report Management

(in-package #:tfs-cmms)

;;; File Upload Handling

(defun ensure-upload-directories ()
  "Ensure upload directories exist."
  (ensure-directories-exist (merge-pathnames "buildings/" *uploads-directory*))
  (ensure-directories-exist (merge-pathnames "deficiencies/" *uploads-directory*)))

(defun generate-upload-filename (original-name prefix)
  "Generate a unique filename for an upload."
  (let* ((extension (pathname-type (pathname original-name)))
         (timestamp (get-universal-time))
         (random-suffix (random 10000)))
    (format nil "~A-~A-~A.~A" prefix timestamp random-suffix (or extension "jpg"))))

(defun save-uploaded-file (post-param subdir prefix)
  "Save an uploaded file and return the relative path.
   POST-PARAM is the Hunchentoot post parameter (list of path, filename, content-type).
   Returns the relative path from uploads directory, or NIL if no file."
  (when (and post-param (listp post-param) (first post-param))
    (let* ((temp-path (first post-param))
           (original-name (second post-param))
           (new-filename (generate-upload-filename original-name prefix))
           (relative-path (format nil "~A/~A" subdir new-filename))
           (full-path (merge-pathnames relative-path *uploads-directory*)))
      (ensure-upload-directories)
      ;; Copy the temp file to our uploads directory
      (uiop:copy-file temp-path full-path)
      relative-path)))

;;; PDF Generation

(defvar *python-path* 
  #p"python3"
  "Path to Python interpreter with reportlab.")

(defvar *pdf-script-path*
  (merge-pathnames "scripts/generate_pdf.py" (get-app-directory))
  "Path to PDF generation script.")

(defvar *reports-directory*
  (merge-pathnames "data/reports/" (get-app-directory))
  "Directory for generated PDF reports.")

(defun sanitize-filename (name)
  "Remove or replace characters that are problematic in filenames."
  (let ((result name))
    (setf result (substitute #\_ #\Space result))
    (setf result (substitute #\_ #\/ result))
    (setf result (substitute #\_ #\: result))
    result))

(defun generate-report-pdf (report-id)
  "Generate a PDF for the given report. Returns the PDF filename or NIL on error."
  (let* ((report (get-inspection-report report-id))
         (deficiencies (get-report-deficiencies report-id))
         (report-number (getf report :|report_number|))
         (pdf-filename (format nil "~A.pdf" (sanitize-filename report-number)))
         (pdf-path (merge-pathnames pdf-filename *reports-directory*))
         (json-path (merge-pathnames (format nil "temp-~A.json" report-id) *reports-directory*)))
    ;; Ensure reports directory exists
    (ensure-directories-exist *reports-directory*)
    ;; Create JSON data file
    (let ((json-data (list 
                      (cons "report" 
                            (list (cons "report_number" (getf report :|report_number|))
                                  (cons "tag_id" (getf report :|tag_id|))
                                  (cons "site_name" (getf report :|site_name|))
                                  (cons "site_code" (getf report :|site_code|))
                                  (cons "building_number" (getf report :|building_number|))
                                  (cons "building_type" (or (getf report :|building_type|) ""))
                                  (cons "system_voltage" (getf report :|system_voltage|))
                                  (cons "inspection_phase" (getf report :|inspection_phase|))
                                  (cons "team_number" (getf report :|team_number|))
                                  (cons "inspection_date" (getf report :|inspection_date|))
                                  (cons "location_description" (or (getf report :|location_description|) ""))
                                  (cons "overall_rating" (or (getf report :|overall_rating|) ""))
                                  (cons "summary_of_findings" (or (getf report :|summary_of_findings|) ""))
                                  (cons "building_image_path" (or (getf report :|building_image_path|) ""))
                                  (cons "inspector1_name" (or (getf report :|inspector1_name|) ""))
                                  (cons "inspector1_signed_at" (or (getf report :|inspector1_signed_at|) ""))
                                  (cons "inspector2_name" (or (getf report :|inspector2_name|) ""))
                                  (cons "inspector2_signed_at" (or (getf report :|inspector2_signed_at|) ""))
                                  (cons "qc_name" (or (getf report :|qc_name|) ""))
                                  (cons "qc_signed_at" (or (getf report :|qc_signed_at|) ""))
                                  (cons "work_order_number" (or (getf report :|work_order_number|) "N/A"))))
                      (cons "deficiencies"
                            (mapcar (lambda (def)
                                      (list (cons "deficiency_number" (getf def :|deficiency_number|))
                                            (cons "location_description" (or (getf def :|location_description|) ""))
                                            (cons "num_occurrences" (or (getf def :|num_occurrences|) 1))
                                            (cons "deficiency_category" (or (getf def :|deficiency_category|) ""))
                                            (cons "equipment_category" (or (getf def :|equipment_category|) ""))
                                            (cons "imminent_danger" (or (getf def :|imminent_danger|) "No"))
                                            (cons "action_taken" (or (getf def :|action_taken|) "N/A"))
                                            (cons "sor_issued_to" (or (getf def :|sor_issued_to|) ""))
                                            (cons "description" (or (getf def :|description|) ""))
                                            (cons "code_source" (or (getf def :|code_source|) ""))
                                            (cons "code_reference" (or (getf def :|code_reference|) ""))
                                            (cons "deficiency_status" (or (getf def :|deficiency_status|) ""))
                                            (cons "rac_score" (or (getf def :|rac_score|) 3))
                                            (cons "image_path" (or (getf def :|image_path|) ""))))
                                    deficiencies)))))
      ;; Write JSON file
      (with-open-file (out json-path :direction :output :if-exists :supersede)
        (write-string (cl-json:encode-json-to-string json-data) out))
      ;; Run Python script
      (let ((result (uiop:run-program 
                     (list (namestring *python-path*)
                           (namestring *pdf-script-path*)
                           (namestring json-path)
                           (namestring pdf-path)
                           (namestring *uploads-directory*)
                           (namestring *static-directory*))
                     :output :string
                     :error-output :string
                     :ignore-error-status t)))
        ;; Clean up temp JSON file
        (when (probe-file json-path)
          (delete-file json-path))
        ;; Return PDF filename if it was created
        (when (probe-file pdf-path)
          pdf-filename)))))

;;; Report Number and TAG-ID Generation

(defun generate-report-number (site-code building-number team-number date-str first-def last-def inspection-phase)
  "Generate report number in format: [Phase]-Report_TFSAFE_[SiteCode]_[Building]_[Team]_[Date]_[DefRange]
   Example: Re-Inspection-Report_TFSAFE_5VN8M_11200_102_17-01-22_63-69"
  (let ((phase-prefix (cond
                        ((string= inspection-phase "Initial Inspection") "Inspection")
                        ((string= inspection-phase "Re Inspection") "Re-Inspection")
                        ((string= inspection-phase "Final Inspection") "Final-Inspection")
                        (t "Inspection")))
        (def-range (if (and first-def last-def)
                       (format nil "~A-~A" first-def last-def)
                       "0")))
    (format nil "~A-Report_TFSAFE_~A_~A_~A_~A_~A"
            phase-prefix site-code building-number team-number date-str def-range)))

(defun generate-tag-id (site-code building-number team-number date-str)
  "Generate TAG-ID in format: TFSAFE_[SiteCode]_[Building]_[Team]_[Date]
   Example: TFSAFE_5VN8M_11200_102_17-01-22"
  (format nil "TFSAFE_~A_~A_~A_~A" site-code building-number team-number date-str))

(defun format-date-for-report (date-str)
  "Convert YYYY-MM-DD to DD-MM-YY format for report naming."
  (when (and date-str (>= (length date-str) 10))
    (let ((year (subseq date-str 2 4))
          (month (subseq date-str 5 7))
          (day (subseq date-str 8 10)))
      (format nil "~A-~A-~A" day month year))))

(defun get-voltage-frequency (voltage)
  "Return the frequency (50Hz or 60Hz) for a given voltage."
  (let ((entry (assoc voltage *system-voltages* :test #'string=)))
    (if entry (cdr entry) "60Hz")))

(defun get-code-source (voltage)
  "Return NEC or BritishStandard based on voltage frequency."
  (let ((freq (get-voltage-frequency voltage)))
    (if (string= freq "50Hz") "BritishStandard" "NEC")))

;;; Report CRUD Operations

(defun create-inspection-report (wo-id site-id building-number building-type system-voltage 
                                  inspection-phase team-number inspection-date
                                  &key location-description previous-report-id)
  "Create a new inspection report linked to a work order."
  (let* ((site (fetch-one "SELECT code FROM sites WHERE id = ?" site-id))
         (site-code (getf site :|code|))
         (date-formatted (format-date-for-report inspection-date))
         (tag-id (generate-tag-id site-code building-number team-number date-formatted))
         (report-number (generate-report-number site-code building-number team-number 
                                                 date-formatted nil nil inspection-phase)))
    (execute-sql 
     "INSERT INTO inspection_reports 
      (wo_id, report_number, tag_id, site_id, building_number, building_type, 
       system_voltage, inspection_phase, previous_report_id, location_description,
       team_number, inspection_date, status)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'Draft')"
     wo-id report-number tag-id site-id building-number building-type
     system-voltage inspection-phase previous-report-id location-description
     team-number inspection-date)
    (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))

(defun get-inspection-report (report-id)
  "Get an inspection report by ID with site info."
  (fetch-one 
   "SELECT r.*, s.code as site_code, s.name as site_name,
           wo.wo_number as work_order_number
    FROM inspection_reports r
    JOIN sites s ON r.site_id = s.id
    JOIN work_orders wo ON r.wo_id = wo.id
    WHERE r.id = ?" 
   report-id))

(defun get-reports-for-work-order (wo-id)
  "Get all inspection reports for a work order."
  (fetch-all 
   "SELECT r.*, s.code as site_code, s.name as site_name
    FROM inspection_reports r
    JOIN sites s ON r.site_id = s.id
    WHERE r.wo_id = ?
    ORDER BY r.created_at DESC" 
   wo-id))

(defun list-inspection-reports (&key site-id status limit)
  "List inspection reports with optional filters."
  (let ((conditions '("1=1"))
        (params '()))
    (when site-id
      (push "r.site_id = ?" conditions)
      (push site-id params))
    (when status
      (push "r.status = ?" conditions)
      (push status params))
    (let ((sql (format nil 
                "SELECT r.*, s.code as site_code, s.name as site_name, wo.wo_number
                 FROM inspection_reports r
                 JOIN sites s ON r.site_id = s.id
                 JOIN work_orders wo ON r.wo_id = wo.id
                 WHERE ~{~A~^ AND ~}
                 ORDER BY r.created_at DESC
                 ~@[LIMIT ~A~]"
                (reverse conditions) limit)))
      (if params
          (apply #'fetch-all sql (reverse params))
          (fetch-all sql)))))

(defun update-inspection-report (report-id &key building-image-path overall-rating 
                                            summary-of-findings mrf-needed
                                            inspector1-name inspector2-name
                                            location-description status
                                            inspection-date team-number
                                            building-type building-number)
  "Update an inspection report."
  (let ((updates '())
        (params '()))
    (when building-image-path
      (push "building_image_path = ?" updates)
      (push building-image-path params))
    (when overall-rating
      (push "overall_rating = ?" updates)
      (push overall-rating params))
    (when summary-of-findings
      (push "summary_of_findings = ?" updates)
      (push summary-of-findings params))
    (when mrf-needed
      (push "mrf_needed = ?" updates)
      (push mrf-needed params))
    (when inspector1-name
      (push "inspector1_name = ?" updates)
      (push inspector1-name params))
    (when inspector2-name
      (push "inspector2_name = ?" updates)
      (push inspector2-name params))
    (when location-description
      (push "location_description = ?" updates)
      (push location-description params))
    (when status
      (push "status = ?" updates)
      (push status params))
    (when inspection-date
      (push "inspection_date = ?" updates)
      (push inspection-date params))
    (when team-number
      (push "team_number = ?" updates)
      (push team-number params))
    (when building-type
      (push "building_type = ?" updates)
      (push building-type params))
    (when building-number
      (push "building_number = ?" updates)
      (push building-number params))
    (when updates
      (push "updated_at = datetime('now')" updates)
      (apply #'execute-sql 
             (format nil "UPDATE inspection_reports SET ~{~A~^, ~} WHERE id = ?"
                     (reverse updates))
             (append (reverse params) (list report-id))))))

;;; Report Status Workflow

(defun submit-report-for-qc (report-id inspector1-name &optional inspector2-name)
  "Submit a report for QC review."
  (execute-sql 
   "UPDATE inspection_reports 
    SET status = 'Pending QC', 
        inspector1_name = ?, inspector1_signed_at = datetime('now'),
        inspector2_name = ?, inspector2_signed_at = CASE WHEN ? IS NOT NULL THEN datetime('now') ELSE NULL END,
        updated_at = datetime('now')
    WHERE id = ?"
   inspector1-name inspector2-name inspector2-name report-id))

(defun qc-approve-report (report-id qc-name &optional comments)
  "QC approves a report and syncs deficiencies to master tracker."
  (execute-sql 
   "UPDATE inspection_reports 
    SET status = 'QC Approved', 
        qc_name = ?, qc_signed_at = datetime('now'), qc_comments = ?,
        updated_at = datetime('now')
    WHERE id = ?"
   qc-name comments report-id)
  ;; Sync deficiencies to master tracker
  (sync-report-to-master-tracker report-id))

(defun validate-report-for-sync (report deficiencies)
  "Validate a report before syncing to master tracker. Returns (values valid-p errors)."
  (let ((errors nil)
        (building-number (getf report :|building_number|))
        (inspection-date (getf report :|inspection_date|))
        (inspection-phase (getf report :|inspection_phase|))
        (site-id (getf report :|site_id|)))
    ;; Check required fields
    (unless building-number
      (push "Missing building number" errors))
    (unless inspection-date
      (push "Missing inspection date" errors))
    (unless inspection-phase
      (push "Missing inspection phase" errors))
    (unless site-id
      (push "Missing site" errors))
    ;; Check camp mapping exists
    (when site-id
      (unless (fetch-one "SELECT id FROM camps WHERE site_id = ?" site-id)
        (push "Site not mapped to a camp in master tracker" errors)))
    ;; Check deficiencies have required fields
    (dolist (def deficiencies)
      (unless (getf def :|deficiency_category|)
        (push (format nil "Deficiency #~A missing category" 
                      (getf def :|deficiency_number|)) errors))
      (unless (getf def :|deficiency_status|)
        (push (format nil "Deficiency #~A missing status" 
                      (getf def :|deficiency_number|)) errors)))
    ;; Check for duplicate in master tracker (same building+date+phase)
    (when (and building-number inspection-date inspection-phase site-id)
      (let ((camp (fetch-one "SELECT id FROM camps WHERE site_id = ?" site-id)))
        (when camp
          (let ((existing (fetch-one 
                           "SELECT COUNT(*) as cnt FROM master_deficiencies 
                            WHERE camp_id = ? AND building_number = ? 
                            AND inspection_date = ? AND inspection_phase = ?"
                           (getf camp :|id|) building-number 
                           inspection-date inspection-phase)))
            (when (and existing (> (getf existing :|cnt|) 0))
              (push (format nil "Warning: ~A deficiencies already exist for this building/date/phase"
                            (getf existing :|cnt|)) errors))))))
    (values (null errors) (nreverse errors))))

(defun sync-report-to-master-tracker (report-id &key is-test)
  "Sync deficiencies from an approved report to the master_deficiencies table.
   If is-test is true, marks records for later cleanup."
  (let* ((report (get-inspection-report report-id))
         (deficiencies (get-report-deficiencies report-id))
         (site-id (getf report :|site_id|))
         (building-number (getf report :|building_number|))
         (team-number (or (getf report :|team_number|) ""))
         (inspection-date (getf report :|inspection_date|))
         (inspection-phase (getf report :|inspection_phase|))
         (site-code (getf report :|site_code|))
         (test-flag (if is-test 1 0))
         ;; Get camp_id from site_id
         (camp (fetch-one "SELECT id FROM camps WHERE site_id = ?" site-id)))
    ;; Validate before syncing
    (multiple-value-bind (valid-p errors)
        (validate-report-for-sync report deficiencies)
      (when errors
        (format t "~&Sync validation warnings for report ~A:~%" report-id)
        (dolist (err errors)
          (format t "  - ~A~%" err))))
    (when camp
      (let ((camp-id (getf camp :|id|)))
        (dolist (def deficiencies)
          (let* ((def-num (getf def :|deficiency_number|))
                 ;; Generate deficiency number in master tracker format: SITE_BLDG_TEAM_DATE_SEQ
                 (date-formatted (if inspection-date
                                     (format nil "~A" 
                                             (subseq (substitute #\- #\/ 
                                                                 (or inspection-date "")) 
                                                     2))
                                     ""))
                 (master-def-number (format nil "~A_~A_~A_~A_~A" 
                                            site-code building-number team-number 
                                            date-formatted def-num))
                 (def-category (getf def :|deficiency_category|))
                 (equipment (getf def :|equipment_category|))
                 (occurrences (or (getf def :|num_occurrences|) 1))
                 (imminent (or (getf def :|imminent_danger|) "No"))
                 (sor-issued-to (getf def :|sor_issued_to|))
                 (def-status (or (getf def :|deficiency_status|) "Open"))
                 (rac (getf def :|rac_score|)))
            ;; Check if this deficiency already exists in master tracker
            (let ((existing (fetch-one 
                             "SELECT id FROM master_deficiencies 
                              WHERE camp_id = ? AND building_number = ? AND deficiency_number = ?"
                             camp-id building-number master-def-number)))
              (if existing
                  ;; Update existing record
                  (execute-sql 
                   "UPDATE master_deficiencies 
                    SET def_category = ?, equipment = ?, inspection_phase = ?,
                        repair_by = ?, occurrences = ?, lhs_imminent = ?,
                        deficiency_status = ?, inspection_date = ?, rac = ?,
                        is_test = ?, updated_at = datetime('now')
                    WHERE id = ?"
                   def-category equipment inspection-phase
                   sor-issued-to occurrences imminent
                   def-status inspection-date rac test-flag
                   (getf existing :|id|))
                  ;; Insert new record
                  (execute-sql 
                   "INSERT INTO master_deficiencies 
                    (camp_id, building_number, deficiency_number, def_category, equipment,
                     inspection_phase, repair_by, occurrences, lhs_imminent,
                     deficiency_status, inspection_date, rac, repair_team_number, is_test)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                   camp-id building-number master-def-number def-category equipment
                   inspection-phase sor-issued-to occurrences imminent
                   def-status inspection-date rac team-number test-flag)))))))))

(defun cleanup-test-data ()
  "Remove all test data from master_deficiencies table."
  (let ((count (getf (fetch-one "SELECT COUNT(*) as cnt FROM master_deficiencies WHERE is_test = 1") :|cnt|)))
    (execute-sql "DELETE FROM master_deficiencies WHERE is_test = 1")
    (format nil "Deleted ~A test records from master tracker" count)))

(defun qc-reject-report (report-id qc-name comments)
  "QC rejects a report with comments."
  (execute-sql 
   "UPDATE inspection_reports 
    SET status = 'QC Rejected', 
        qc_name = ?, qc_comments = ?, last_rejection_comments = ?,
        updated_at = datetime('now')
    WHERE id = ?"
   qc-name comments comments report-id))

(defun finalize-report (report-id)
  "Mark report as complete (after QC approval)."
  (let* ((report (get-inspection-report report-id))
         (deficiencies (get-report-deficiencies report-id))
         (first-def (when deficiencies (getf (car (last deficiencies)) :|deficiency_number|)))
         (last-def (when deficiencies (getf (car deficiencies) :|deficiency_number|)))
         (site-code (getf report :|site_code|))
         (building-number (getf report :|building_number|))
         (team-number (getf report :|team_number|))
         (date-formatted (format-date-for-report (getf report :|inspection_date|)))
         (inspection-phase (getf report :|inspection_phase|))
         (new-report-number (generate-report-number site-code building-number team-number
                                                     date-formatted first-def last-def 
                                                     inspection-phase)))
    (execute-sql 
     "UPDATE inspection_reports 
      SET status = 'Complete', report_number = ?, updated_at = datetime('now')
      WHERE id = ?"
     new-report-number report-id)))

(defun create-reinspection-report (original-report-id &key team-number inspection-date)
  "Create a re-inspection report by cloning an existing completed report.
   Copies all deficiencies with their current status (Open).
   Returns the new report ID."
  (let* ((original (get-inspection-report original-report-id))
         (original-deficiencies (get-report-deficiencies original-report-id))
         (new-team (or team-number (getf original :|team_number|)))
         (new-date (or inspection-date (getf original :|inspection_date|)))
         (site-code (getf original :|site_code|))
         (building-number (getf original :|building_number|))
         (date-formatted (format-date-for-report new-date))
         ;; Find unique report number by checking for existing ones
         (base-report-number (format nil "Re-Inspection-Report_TFSAFE_~A_~A_~A_~A" 
                                     site-code building-number new-team date-formatted))
         (existing-count (getf (fetch-one 
                                "SELECT COUNT(*) as cnt FROM inspection_reports WHERE report_number LIKE ?"
                                (concatenate 'string base-report-number "%"))
                               :|cnt|))
         (report-number (format nil "~A_0-~A" base-report-number (1+ existing-count))))
    ;; Create the new report
    (execute-sql 
     "INSERT INTO inspection_reports 
      (wo_id, tag_id, site_id, building_number, building_type, system_voltage,
       inspection_phase, previous_report_id, location_description, building_image_path,
       team_number, inspection_date, status, report_number)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (getf original :|wo_id|)
     "TFSAFE"
     (getf original :|site_id|)
     building-number
     (getf original :|building_type|)
     (getf original :|system_voltage|)
     "Re Inspection"
     original-report-id
     (getf original :|location_description|)
     (getf original :|building_image_path|)
     new-team
     new-date
     "Draft"
     report-number)
    ;; Get the new report ID
    (let ((new-report-id (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))
      ;; Copy all deficiencies from original report
      (dolist (def original-deficiencies)
        (execute-sql 
         "INSERT INTO deficiencies 
          (report_id, deficiency_number, location_description, num_occurrences,
           deficiency_category, equipment_category, imminent_danger, action_taken,
           sor_issued_to, description, code_source, code_reference, 
           deficiency_status, rac_score, image_path)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
         new-report-id
         (getf def :|deficiency_number|)
         (getf def :|location_description|)
         (getf def :|num_occurrences|)
         (getf def :|deficiency_category|)
         (getf def :|equipment_category|)
         (getf def :|imminent_danger|)
         (getf def :|action_taken|)
         (getf def :|sor_issued_to|)
         (getf def :|description|)
         (getf def :|code_source|)
         (getf def :|code_reference|)
         (getf def :|deficiency_status|)  ;; Keep original status (Open)
         (getf def :|rac_score|)
         (getf def :|image_path|)))
      new-report-id)))

;;; Deficiency CRUD Operations

(defun get-next-deficiency-number (report-id)
  "Get the next deficiency number for a report (continues from previous report if exists)."
  (let* ((report (get-inspection-report report-id))
         (prev-report-id (getf report :|previous_report_id|)))
    (if prev-report-id
        ;; Continue numbering from previous report
        (let ((max-def (fetch-one 
                        "SELECT MAX(deficiency_number) as max_num FROM deficiencies WHERE report_id = ?"
                        prev-report-id)))
          (1+ (or (getf max-def :|max_num|) 0)))
        ;; Start fresh
        (let ((max-def (fetch-one 
                        "SELECT MAX(deficiency_number) as max_num FROM deficiencies WHERE report_id = ?"
                        report-id)))
          (1+ (or (getf max-def :|max_num|) 0))))))

(defun add-deficiency (report-id &key location-description num-occurrences 
                                   deficiency-category equipment-category
                                   imminent-danger action-taken sor-issued-to
                                   description code-source code-reference
                                   deficiency-status image-path)
  "Add a deficiency to an inspection report."
  (let ((def-number (get-next-deficiency-number report-id))
        (rac-score (calculate-rac-score deficiency-category imminent-danger)))
    (execute-sql 
     "INSERT INTO deficiencies 
      (report_id, deficiency_number, location_description, num_occurrences,
       deficiency_category, equipment_category, imminent_danger, action_taken,
       sor_issued_to, description, code_source, code_reference, deficiency_status,
       image_path, rac_score)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     report-id def-number location-description (or num-occurrences 1)
     deficiency-category equipment-category (or imminent-danger "No") action-taken
     sor-issued-to description code-source code-reference (or deficiency-status "Open")
     image-path rac-score)
    (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))

(defun get-deficiency (deficiency-id)
  "Get a deficiency by ID."
  (fetch-one "SELECT * FROM deficiencies WHERE id = ?" deficiency-id))

(defun get-report-deficiencies (report-id)
  "Get all deficiencies for a report, ordered by deficiency number."
  (fetch-all 
   "SELECT * FROM deficiencies WHERE report_id = ? ORDER BY deficiency_number DESC"
   report-id))

(defun update-deficiency (deficiency-id &key location-description num-occurrences
                                          deficiency-category equipment-category
                                          imminent-danger action-taken sor-issued-to
                                          description code-source code-reference
                                          deficiency-status image-path)
  "Update a deficiency."
  (let ((updates '())
        (params '()))
    (when location-description
      (push "location_description = ?" updates)
      (push location-description params))
    (when num-occurrences
      (push "num_occurrences = ?" updates)
      (push num-occurrences params))
    (when deficiency-category
      (push "deficiency_category = ?" updates)
      (push deficiency-category params))
    (when equipment-category
      (push "equipment_category = ?" updates)
      (push equipment-category params))
    (when imminent-danger
      (push "imminent_danger = ?" updates)
      (push imminent-danger params))
    (when action-taken
      (push "action_taken = ?" updates)
      (push action-taken params))
    (when sor-issued-to
      (push "sor_issued_to = ?" updates)
      (push sor-issued-to params))
    (when description
      (push "description = ?" updates)
      (push description params))
    (when code-source
      (push "code_source = ?" updates)
      (push code-source params))
    (when code-reference
      (push "code_reference = ?" updates)
      (push code-reference params))
    (when deficiency-status
      (push "deficiency_status = ?" updates)
      (push deficiency-status params))
    (when image-path
      (push "image_path = ?" updates)
      (push image-path params))
    ;; Recalculate RAC if category or danger changed
    (when (or deficiency-category imminent-danger)
      (let* ((current (get-deficiency deficiency-id))
             (cat (or deficiency-category (getf current :|deficiency_category|)))
             (danger (or imminent-danger (getf current :|imminent_danger|)))
             (rac (calculate-rac-score cat danger)))
        (push "rac_score = ?" updates)
        (push rac params)))
    (when updates
      (push "updated_at = datetime('now')" updates)
      (apply #'execute-sql 
             (format nil "UPDATE deficiencies SET ~{~A~^, ~} WHERE id = ?"
                     (reverse updates))
             (append (reverse params) (list deficiency-id))))))

(defun delete-deficiency (deficiency-id)
  "Delete a deficiency."
  (execute-sql "DELETE FROM deficiencies WHERE id = ?" deficiency-id))

;;; RAC Score Calculation

(defun calculate-rac-score (deficiency-category imminent-danger)
  "Calculate Risk Assessment Code (RAC) score.
   RAC 1 = Imminent danger
   RAC 2 = Serious (most deficiencies)
   RAC 3 = Moderate
   RAC 4 = Minor"
  (cond
    ((string= imminent-danger "Yes") 1)
    ((member deficiency-category '("Grounding and Bonding" "Improper Terminations") :test #'string=) 2)
    ((member deficiency-category '("Improper Use / Damaged" "Unlisted Equipment") :test #'string=) 2)
    ((string= deficiency-category "Poor Workmanship") 3)
    (t 3)))

;;; Inspection Report Handlers (moved from routes.lisp)

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
