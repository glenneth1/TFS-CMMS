;;;; inspection-reports.lisp - Inspection Report Management

(in-package #:tfs-cmms)

;;; File Upload Handling

(defun ensure-upload-directories ()
  "Ensure upload directories exist."
  (ensure-directories-exist (merge-pathnames "buildings/" *uploads-directory*))
  (ensure-directories-exist (merge-pathnames "deficiencies/" *uploads-directory*)))

(defun generate-upload-filename (original-name prefix)
  "Generate a unique filename for an upload. Always uses .jpg extension for optimized images."
  (let* ((timestamp (get-universal-time))
         (random-suffix (random 10000)))
    (format nil "~A-~A-~A.jpg" prefix timestamp random-suffix)))

(defvar *optimize-script-path*
  (merge-pathnames "scripts/optimize_image.py" (get-app-directory))
  "Path to image optimization script.")

(defvar *image-max-dimension* 1920
  "Maximum dimension (width or height) for uploaded images.")

(defvar *image-quality* 85
  "JPEG quality for optimized images (1-100).")

(defun optimize-uploaded-image (input-path output-path)
  "Optimize an uploaded image using Python script.
   Returns T on success, NIL on failure."
  (let* ((command (format nil "python3 ~A ~A ~A ~A ~A"
                          (namestring *optimize-script-path*)
                          (namestring input-path)
                          (namestring output-path)
                          *image-max-dimension*
                          *image-quality*)))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program command
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore output))
      (if (zerop exit-code)
          t
          (progn
            (format t "~&Image optimization failed: ~A~%" error-output)
            nil)))))

(defun save-uploaded-file (post-param subdir prefix)
  "Save an uploaded file and return the relative path.
   POST-PARAM is the Hunchentoot post parameter (list of path, filename, content-type).
   Images are automatically optimized (resized, compressed, EXIF stripped).
   Returns the relative path from uploads directory, or NIL if no file."
  (when (and post-param (listp post-param) (first post-param))
    (let* ((temp-path (first post-param))
           (original-name (second post-param))
           (content-type (third post-param))
           (new-filename (generate-upload-filename original-name prefix))
           (relative-path (format nil "~A/~A" subdir new-filename))
           (full-path (merge-pathnames relative-path *uploads-directory*)))
      (ensure-upload-directories)
      ;; Check if it's an image that should be optimized
      (if (and content-type 
               (or (search "image/" content-type)
                   (member (pathname-type (pathname original-name))
                           '("jpg" "jpeg" "png" "gif" "bmp" "webp")
                           :test #'string-equal)))
          ;; Optimize the image
          (if (optimize-uploaded-image temp-path full-path)
              relative-path
              ;; Fallback: copy original if optimization fails
              (progn
                (uiop:copy-file temp-path full-path)
                relative-path))
          ;; Non-image file: just copy
          (progn
            (uiop:copy-file temp-path full-path)
            relative-path)))))

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

(defun handle-api-generate-pdf (report-id-str)
  "API handler to generate and serve PDF for an inspection report."
  (let* ((report-id (parse-integer report-id-str :junk-allowed t))
         (pdf-filename (generate-report-pdf report-id)))
    (if pdf-filename
        (let ((pdf-path (merge-pathnames pdf-filename *reports-directory*)))
          (setf (hunchentoot:content-type*) "application/pdf")
          (setf (hunchentoot:header-out :content-disposition)
                (format nil "inline; filename=\"~A\"" pdf-filename))
          (with-open-file (in pdf-path :element-type '(unsigned-byte 8))
            (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
              (read-sequence buffer in)
              buffer)))
        (progn
          (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
          "Error generating PDF"))))

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
    LEFT JOIN work_orders wo ON r.wo_id = wo.id
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
    ;; Generate new tag_id with deficiency range and reinspection suffix if applicable
    (let* ((def-range (if (and first-def last-def) (format nil "~A-~A" first-def last-def) "0"))
           (reinspection-num (getf report :|reinspection_number|))
           (base-tag (format nil "TFSAFE_~A_~A_~A_~A_~A" site-code building-number team-number date-formatted def-range))
           (final-tag (if reinspection-num (format nil "~A_~A" base-tag reinspection-num) base-tag))
           (final-report (if reinspection-num (format nil "~A_~A" new-report-number reinspection-num) new-report-number)))
      (execute-sql 
       "UPDATE inspection_reports 
        SET status = 'Complete', report_number = ?, 
            tag_id = ?, updated_at = datetime('now')
        WHERE id = ?"
       final-report final-tag report-id))))

(defun create-reinspection-report (original-report-id &key team-number inspection-date)
  "Create a re-inspection report by cloning an existing completed report.
   Copies all deficiencies with their current status (Open).
   Appends _1, _2, etc. for each re-inspection of the same building.
   Returns the new report ID."
  (let* ((original (get-inspection-report original-report-id))
         (original-deficiencies (get-report-deficiencies original-report-id))
         (new-team (or team-number (getf original :|team_number|)))
         (new-date (or inspection-date (getf original :|inspection_date|)))
         (site-code (getf original :|site_code|))
         (building-number (getf original :|building_number|))
         (site-id (getf original :|site_id|))
         (date-formatted (format-date-for-report new-date))
         ;; Count existing re-inspections for this building to determine suffix
         (existing-reinspections (getf (fetch-one 
                                        "SELECT COUNT(*) as cnt FROM inspection_reports 
                                         WHERE site_id = ? AND building_number = ? 
                                         AND inspection_phase = 'Re Inspection'"
                                        site-id building-number)
                                       :|cnt|))
         (reinspection-number (1+ existing-reinspections))
         ;; Initial report/tag (will be updated on finalize with actual deficiency range)
         (report-number (format nil "Re-Inspection-Report_TFSAFE_~A_~A_~A_~A_0_~A" 
                                site-code building-number new-team date-formatted reinspection-number))
         (tag-id (format nil "TFSAFE_~A_~A_~A_~A_0_~A" 
                         site-code building-number new-team date-formatted reinspection-number)))
    ;; Create the new report
    (execute-sql 
     "INSERT INTO inspection_reports 
      (wo_id, tag_id, site_id, building_number, building_type, system_voltage,
       inspection_phase, previous_report_id, location_description, building_image_path,
       team_number, inspection_date, status, report_number, reinspection_number)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (getf original :|wo_id|)
     tag-id
     site-id
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
     report-number
     reinspection-number)
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
    (let ((def-id (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))
      ;; Update report number to reflect deficiency range
      (update-report-deficiency-range report-id)
      def-id)))

(defun update-report-deficiency-range (report-id)
  "Update the report number to reflect the current deficiency range."
  (let* ((report (get-inspection-report report-id))
         (deficiencies (fetch-all "SELECT deficiency_number FROM deficiencies WHERE report_id = ? ORDER BY deficiency_number" report-id))
         (first-def (when deficiencies (getf (first deficiencies) :|deficiency_number|)))
         (last-def (when deficiencies (getf (car (last deficiencies)) :|deficiency_number|)))
         (site-code (getf report :|site_code|))
         (building-number (getf report :|building_number|))
         (team-number (getf report :|team_number|))
         (inspection-date (getf report :|inspection_date|))
         (inspection-phase (getf report :|inspection_phase|))
         (date-formatted (format-date-for-report inspection-date))
         (new-report-number (generate-report-number site-code building-number team-number 
                                                     date-formatted first-def last-def inspection-phase)))
    (execute-sql "UPDATE inspection_reports SET report_number = ? WHERE id = ?"
                 new-report-number report-id)))

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
  "Delete a deficiency and update report number."
  (let ((def (get-deficiency deficiency-id)))
    (when def
      (let ((report-id (getf def :|report_id|)))
        (execute-sql "DELETE FROM deficiencies WHERE id = ?" deficiency-id)
        ;; Update report number to reflect new deficiency range
        (update-report-deficiency-range report-id)))))

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
         (is-admin (and user (eql 1 (getf user :|is_admin|))))
         (user-team (when user (getf user :|team_number|)))
         (status-filter (get-param "status"))
         (show-assigned (and user (or (user-is-admin-p user)
                                      (string= (string-downcase (or (getf user :|role|) "")) "qc_manager"))))
         (base-query "SELECT r.*, s.name as site_name, s.code as site_code, u.full_name as assigned_to_name
                      FROM inspection_reports r
                      JOIN sites s ON r.site_id = s.id
                      LEFT JOIN users u ON r.assigned_qc_id = u.id")
         ;; Non-admin users only see their team's reports (handle both "113" and "Team 113" formats)
         (team-clause (if (and (not is-admin) user-team)
                          (format nil " AND (r.team_number = '~A' OR r.team_number = 'Team ~A')" user-team user-team)
                          ""))
         (reports (cond
                    ((and status-filter (string= status-filter "rejected"))
                     (fetch-all (concatenate 'string base-query 
                                " WHERE r.rejection_count > 0" team-clause " ORDER BY r.rejection_count DESC, r.created_at DESC LIMIT 100")))
                    ((and status-filter (> (length status-filter) 0))
                     (fetch-all (concatenate 'string base-query 
                                " WHERE r.status = ?" team-clause " ORDER BY r.created_at DESC LIMIT 100") status-filter))
                    (t (fetch-all (concatenate 'string base-query 
                                  " WHERE 1=1" team-clause " ORDER BY r.created_at DESC LIMIT 100"))))))
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
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-inspection-report-new (redirect-to "/unauthorized"))))
  (let* ((wo-id (parse-int (get-param "wo_id")))
         (wo (when wo-id (get-work-order wo-id)))
         (sites (list-sites))
         ;; Extract building info from work order location_details
         ;; Format is typically "BuildingName (BuildingType)" or just "BuildingName"
         (wo-location (when wo (getf wo :|location_details|)))
         (wo-building-name (when wo-location
                            (let ((paren-pos (position #\( wo-location)))
                              (if paren-pos
                                  (string-trim " " (subseq wo-location 0 paren-pos))
                                  wo-location))))
         (wo-building-type (when wo-location
                            (let ((paren-pos (position #\( wo-location)))
                              (when paren-pos
                                (let ((end-pos (position #\) wo-location :start paren-pos)))
                                  (when end-pos
                                    (subseq wo-location (1+ paren-pos) end-pos)))))))
         ;; Get team number from work order (strip "Team " prefix if present)
         (wo-team (when wo 
                    (let ((assigned (getf wo :|assigned_to|)))
                      (if (and assigned (>= (length assigned) 5) 
                               (string= "Team " (subseq assigned 0 5)))
                          (subseq assigned 5)
                          (or (getf wo :|team_number|) assigned))))))
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
                       :value (or wo-building-name "")
                       :placeholder "e.g., 11200, GEN-1")))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "Building Type")
               (:select :name "building_type"
                 (:option :value "" "-- Select --")
                 (dolist (bt *building-types*)
                   (let ((selected (and wo-building-type (string= bt wo-building-type))))
                     (cl-who:htm (:option :value bt :selected selected (cl-who:str bt)))))))
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
                       :value (or wo-team ""))))
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
                           (when (member (getf report :|status|) '("Draft" "QC Rejected") :test #'string=)
                             (cl-who:htm
                              (:a :href (format nil "/mrf/~A" (getf mrf :|id|))
                                  :class "btn btn-primary" "Edit MRF Items")))
                           (:a :href (format nil "/mrf/~A/pdf" (getf mrf :|id|))
                               :class "btn" :target "_blank" "Download MRF PDF")))
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
                  (let ((has-mrf (and (string= "Yes" (getf report :|mrf_needed|))
                                      (get-mrf-by-report report-id))))
                    (cl-who:htm
                     (:form :method "post" :action (format nil "/api/inspection-reports/~A/submit" report-id)
                       (:div :class "form-row"
                         (:div :class "form-group required"
                           (:label "Inspector 1 Name")
                           (:input :type "text" :name "inspector1_name" :required t))
                         (:div :class "form-group"
                           (:label "Inspector 2 Name")
                           (:input :type "text" :name "inspector2_name")))
                       (:button :type "submit" :class "btn btn-primary" 
                                (cl-who:str (if has-mrf "Submit Report & MRF for QC Review" "Submit for QC Review")))))))
                 
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
                  (when (or (string= (getf report :|inspection_phase|) "Initial Inspection")
                            (string= (getf report :|inspection_phase|) "Initial"))
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
    (if (and original 
             (string= (getf original :|status|) "Complete")
             (or (string= (getf original :|inspection_phase|) "Initial Inspection")
                 (string= (getf original :|inspection_phase|) "Initial")))
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
                 (:div :class "form-actions" :style "margin-top: 1.5rem; padding-top: 1rem;"
                   (:button :type "submit" :class "btn btn-primary" :style "display: inline-block;" "Create Re-inspection Report")
                   (:a :href (format nil "/inspection-reports/~A" original-id) 
                       :class "btn" "Cancel")))))))
        (redirect-to "/inspection-reports"))))


(defun handle-deficiency-added (report-id-str)
  "Deficiency added success page with options to add another or return."
  (let* ((report-id (parse-int report-id-str))
         (report (get-inspection-report report-id))
         (deficiencies (get-report-deficiencies report-id))
         (def-count (length deficiencies)))
    (if report
        (html-response
         (render-page "Deficiency Added"
           (cl-who:with-html-output-to-string (s)
             (:div :class "success-page" :style "text-align: center; padding: 2rem;"
               (:div :class "success-icon" :style "font-size: 4rem; color: #28a745; margin-bottom: 1rem;"
                 "✓")
               (:h1 "Deficiency Added Successfully!")
               (:p :style "font-size: 1.2rem; margin: 1rem 0;"
                   (cl-who:fmt "Report: ~A" (getf report :|report_number|)))
               (:p :style "color: #666; margin-bottom: 2rem;"
                   (cl-who:fmt "Total deficiencies on this report: ~A" def-count))
               (:div :class "action-buttons" :style "display: flex; gap: 1rem; justify-content: center; flex-wrap: wrap;"
                 (:a :href (format nil "/inspection-reports/~A/deficiency/new" report-id) 
                     :class "btn btn-primary" :style "padding: 1rem 2rem;"
                     "+ Add Another Deficiency")
                 (:a :href (format nil "/inspection-reports/~A" report-id) 
                     :class "btn btn-secondary" :style "padding: 1rem 2rem;"
                     "Done - Return to Report"))))))
        (redirect-to "/inspection-reports"))))

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


(defun handle-api-inspection-report-create ()
  "Create a new inspection report."
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-api-inspection-report-create (redirect-to "/unauthorized"))))
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
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-api-inspection-report-update (redirect-to "/unauthorized"))))
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
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-api-inspection-report-building-image (redirect-to "/unauthorized"))))
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
  "Submit report and linked MRF for QC review."
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-api-inspection-report-submit (redirect-to "/unauthorized"))))
  (let* ((report-id (parse-int id))
         (user (get-current-user))
         (user-id (getf user :|id|))
         (inspector1-name (get-param "inspector1_name"))
         (inspector2-name (get-param "inspector2_name"))
         (report (get-inspection-report report-id))
         (linked-mrf (get-mrf-by-report report-id)))
    (submit-report-for-qc report-id inspector1-name inspector2-name)
    ;; Also submit linked MRF
    (when linked-mrf
      (update-mrf-status (getf linked-mrf :|id|) "Submitted"))
    ;; Record who submitted the report for notifications
    (execute-sql "UPDATE inspection_reports SET submitted_by_user_id = ? WHERE id = ?" user-id report-id)
    ;; Notify QC team
    (notify-qc-team report-id (getf report :|report_number|) inspector1-name)
    (redirect-to (format nil "/inspection-reports/~A" report-id))))


(defun handle-api-inspection-report-qc-approve (id)
  "QC approve a report and its linked MRF."
  (let* ((report-id (parse-int id))
         (report (get-inspection-report report-id))
         (qc-name (get-param "qc_name"))
         (qc-comments (or (get-param "qc_comments") ""))
         (submitted-by (getf report :|submitted_by_user_id|))
         (linked-mrf (get-mrf-by-report report-id)))
    (qc-approve-report report-id qc-name qc-comments)
    (finalize-report report-id)
    ;; Also approve linked MRF (this will deduct inventory)
    (when linked-mrf
      (update-mrf-status (getf linked-mrf :|id|) "Approved" :approved-by qc-name))
    ;; Notify submitting inspector
    (when submitted-by
      (execute-sql 
       "INSERT INTO notifications (user_id, title, message, link) VALUES (?, ?, ?, ?)"
       submitted-by
       "Report Approved - Signature Required"
       (format nil "Report ~A has been approved by QC.~A Please sign and finalize." 
               (getf report :|report_number|)
               (if linked-mrf " MRF also approved." ""))
       (format nil "/inspection-reports/~A" report-id)))
    (redirect-to (format nil "/inspection-reports/~A" report-id))))


(defun handle-api-create-reinspection (original-id-str)
  "Create a re-inspection report from a completed initial inspection."
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-api-create-reinspection (redirect-to "/unauthorized"))))
  (let* ((original-id (parse-int original-id-str))
         (team-number (get-param "team_number"))
         (inspection-date (get-param "inspection_date"))
         (new-report-id (create-reinspection-report original-id 
                                                     :team-number team-number
                                                     :inspection-date inspection-date)))
    (redirect-to (format nil "/inspection-reports/~A" new-report-id))))


(defun handle-api-inspection-report-qc-reject (id)
  "QC reject a report and reset its linked MRF to Draft."
  (let* ((report-id (parse-int id))
         (report (get-inspection-report report-id))
         (qc-name (get-param "qc_name"))
         (qc-comments (or (get-param "qc_comments") ""))
         (submitted-by (getf report :|submitted_by_user_id|))
         (current-rejection-count (or (getf report :|rejection_count|) 0))
         (new-rejection-number (1+ current-rejection-count))
         (linked-mrf (get-mrf-by-report report-id)))
    (qc-reject-report report-id qc-name qc-comments)
    ;; Increment rejection counter
    (execute-sql "UPDATE inspection_reports SET rejection_count = ? WHERE id = ?" 
                 new-rejection-number report-id)
    ;; Reset linked MRF to Draft so inspector can modify it
    (when linked-mrf
      (update-mrf-status (getf linked-mrf :|id|) "Draft"))
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
       (format nil "Report ~A was rejected by ~A.~A Comments: ~A" 
               (getf report :|report_number|) qc-name 
               (if linked-mrf " MRF returned to Draft." "")
               qc-comments)
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
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-api-deficiency-create (redirect-to "/unauthorized"))))
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
    ;; Redirect to success page with option to add another
    (redirect-to (format nil "/inspection-reports/~A/deficiency/added" report-id))))


(defun handle-api-deficiency-delete (deficiency-id-str)
  "Delete a deficiency."
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-api-deficiency-delete (redirect-to "/unauthorized"))))
  (let ((deficiency-id (parse-int deficiency-id-str))
        (report-id (get-param "report_id")))
    (delete-deficiency deficiency-id)
    (redirect-to (format nil "/inspection-reports/~A" report-id))))


(defun handle-api-deficiency-update (deficiency-id-str)
  "Update a deficiency."
  ;; Block read-only users (Program Manager)
  (let ((user (get-current-user)))
    (when (user-is-read-only-p user)
      (return-from handle-api-deficiency-update (redirect-to "/unauthorized"))))
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

