(in-package #:tfs-cmms)

;;; SOR (Statement of Requirement) Reports Module

;;; Helper Functions

(defun list-camps ()
  "Get all camps for dropdown."
  (fetch-all "SELECT c.id, c.name, co.name as country 
              FROM camps c 
              LEFT JOIN countries co ON c.country_id = co.id 
              WHERE c.name != 'Unknown' 
              ORDER BY co.name, c.name"))

(defun get-camp (camp-id)
  "Get a single camp by ID."
  (fetch-one "SELECT c.id, c.name, co.name as country 
              FROM camps c 
              LEFT JOIN countries co ON c.country_id = co.id 
              WHERE c.id = ?" camp-id))

;;; Database Query Functions

(defun get-sor-summary (&key camp-id date-from date-to repair-by status)
  "Get summary statistics for SOR report."
  (let ((conditions '("(is_test IS NULL OR is_test = 0)"))
        (params '()))
    (when camp-id
      (push "camp_id = ?" conditions)
      (push camp-id params))
    (when (and date-from (> (length date-from) 0))
      (push "date(inspection_date) >= ?" conditions)
      (push date-from params))
    (when (and date-to (> (length date-to) 0))
      (push "date(inspection_date) <= ?" conditions)
      (push date-to params))
    (when (and repair-by (> (length repair-by) 0))
      (push "repair_by = ?" conditions)
      (push repair-by params))
    (when (and status (> (length status) 0))
      (push "deficiency_status = ?" conditions)
      (push status params))
    (let* ((where-clause (format nil "~{~A~^ AND ~}" (reverse conditions)))
           (sql (format nil 
                  "SELECT COUNT(*) as total,
                     SUM(CASE WHEN deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
                     SUM(CASE WHEN deficiency_status IN ('Closed / Repaired', 'Closed Previously', 'TFS Closed') THEN 1 ELSE 0 END) as closed_count,
                     SUM(CASE WHEN deficiency_status = 'ReInsp-Open' THEN 1 ELSE 0 END) as reinsp_count
                   FROM master_deficiencies WHERE ~A" where-clause)))
      (apply #'fetch-one sql (reverse params)))))

(defun get-sor-by-repair-responsibility (&key camp-id date-from date-to)
  "Get SOR counts grouped by repair responsibility."
  (let ((conditions '("repair_by IS NOT NULL AND repair_by != ''" "(is_test IS NULL OR is_test = 0)"))
        (params '()))
    (when camp-id
      (push "camp_id = ?" conditions)
      (push camp-id params))
    (when (and date-from (> (length date-from) 0))
      (push "date(inspection_date) >= ?" conditions)
      (push date-from params))
    (when (and date-to (> (length date-to) 0))
      (push "date(inspection_date) <= ?" conditions)
      (push date-to params))
    (let* ((where-clause (format nil "~{~A~^ AND ~}" (reverse conditions)))
           (sql (format nil 
                  "SELECT repair_by, COUNT(*) as total,
                     SUM(CASE WHEN deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
                     SUM(CASE WHEN deficiency_status IN ('Closed / Repaired', 'Closed Previously', 'TFS Closed') THEN 1 ELSE 0 END) as closed_count,
                     SUM(CASE WHEN deficiency_status = 'ReInsp-Open' THEN 1 ELSE 0 END) as reinsp_count
                   FROM master_deficiencies WHERE ~A GROUP BY repair_by ORDER BY total DESC" where-clause)))
      (apply #'fetch-all sql (reverse params)))))

(defun get-sor-by-camp (&key date-from date-to repair-by)
  "Get SOR counts grouped by camp/site."
  (let ((conditions '("(md.is_test IS NULL OR md.is_test = 0)"))
        (params '()))
    (when (and date-from (> (length date-from) 0))
      (push "date(md.inspection_date) >= ?" conditions)
      (push date-from params))
    (when (and date-to (> (length date-to) 0))
      (push "date(md.inspection_date) <= ?" conditions)
      (push date-to params))
    (when (and repair-by (> (length repair-by) 0))
      (push "md.repair_by = ?" conditions)
      (push repair-by params))
    (let* ((where-clause (format nil "~{~A~^ AND ~}" (reverse conditions)))
           (sql (format nil 
                  "SELECT c.name as camp_name, co.name as country, COUNT(*) as total,
                     SUM(CASE WHEN md.deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
                     SUM(CASE WHEN md.deficiency_status IN ('Closed / Repaired', 'Closed Previously', 'TFS Closed') THEN 1 ELSE 0 END) as closed_count,
                     SUM(CASE WHEN md.deficiency_status = 'ReInsp-Open' THEN 1 ELSE 0 END) as reinsp_count
                   FROM master_deficiencies md 
                   JOIN camps c ON md.camp_id = c.id
                   LEFT JOIN countries co ON c.country_id = co.id
                   WHERE ~A GROUP BY md.camp_id ORDER BY total DESC" where-clause)))
      (apply #'fetch-all sql (reverse params)))))

(defun get-sor-details (&key camp-id date-from date-to repair-by status limit)
  "Get detailed SOR records."
  (let ((conditions '("(md.is_test IS NULL OR md.is_test = 0)"))
        (params '()))
    (when camp-id
      (push "md.camp_id = ?" conditions)
      (push camp-id params))
    (when (and date-from (> (length date-from) 0))
      (push "date(md.inspection_date) >= ?" conditions)
      (push date-from params))
    (when (and date-to (> (length date-to) 0))
      (push "date(md.inspection_date) <= ?" conditions)
      (push date-to params))
    (when (and repair-by (> (length repair-by) 0))
      (push "md.repair_by = ?" conditions)
      (push repair-by params))
    (when (and status (> (length status) 0))
      (push "md.deficiency_status = ?" conditions)
      (push status params))
    (let* ((where-clause (format nil "~{~A~^ AND ~}" (reverse conditions)))
           (sql (format nil 
                  "SELECT md.*, c.name as camp_name, co.name as country
                   FROM master_deficiencies md 
                   JOIN camps c ON md.camp_id = c.id
                   LEFT JOIN countries co ON c.country_id = co.id
                   WHERE ~A ORDER BY md.inspection_date DESC, md.camp_id ~@[LIMIT ~A~]" where-clause limit)))
      (apply #'fetch-all sql (reverse params)))))

(defun get-repair-by-options ()
  "Get distinct repair_by values for dropdown."
  (fetch-all "SELECT DISTINCT repair_by FROM master_deficiencies 
              WHERE repair_by IS NOT NULL AND repair_by != '' AND repair_by != ' ' ORDER BY repair_by"))

(defun get-deficiency-status-options ()
  "Get distinct deficiency_status values for dropdown."
  (fetch-all "SELECT DISTINCT deficiency_status FROM master_deficiencies 
              WHERE deficiency_status IS NOT NULL AND deficiency_status != '' ORDER BY deficiency_status"))

;;; Styles

(defparameter *sor-report-styles*
  "@media print { .no-print { display: none !important; } .navbar { display: none !important; } }
   .summary-grid { display: flex; gap: 1rem; flex-wrap: wrap; margin-bottom: 1.5rem; }
   .summary-card { background: white; border: 1px solid #ddd; border-radius: 8px; padding: 1rem 1.5rem; text-align: center; min-width: 150px; }
   .summary-card.open { border-left: 4px solid #e53e3e; }
   .summary-card.closed { border-left: 4px solid #38a169; }
   .summary-card.reinsp { border-left: 4px solid #d69e2e; }
   .summary-card.total { border-left: 4px solid #3182ce; }
   .summary-value { font-size: 2rem; font-weight: bold; display: block; }
   .summary-label { font-size: 0.875rem; color: #666; }
   .responsibility-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 1rem; margin-bottom: 1.5rem; }
   .responsibility-card { background: #f8f9fa; border-radius: 8px; padding: 1rem; }
   .responsibility-card h4 { margin: 0 0 0.5rem 0; color: #1a365d; }
   .responsibility-stats { display: flex; gap: 1rem; flex-wrap: wrap; }
   .responsibility-stat { font-size: 0.875rem; }
   .responsibility-stat strong { display: block; font-size: 1.25rem; }")

;;; Handler Functions

(defun handle-sor-reports ()
  "SOR Reports page with filters and summary."
  (let* ((camp-param (get-param "camp"))
         (all-camps-p (equal camp-param "all"))
         (camp-id (unless all-camps-p (parse-int camp-param)))
         (date-from (get-param "date_from"))
         (date-to (get-param "date_to"))
         (repair-by (get-param "repair_by"))
         (status-filter (get-param "status"))
         (contract-week (get-param "contract_week"))
         (camps (list-camps))
         (repair-by-options (get-repair-by-options))
         (status-options (get-deficiency-status-options))
         (has-filters (or camp-id all-camps-p (and date-from (> (length date-from) 0)))))
    (if has-filters
        (render-sor-report camp-id all-camps-p date-from date-to repair-by status-filter contract-week camps)
        (render-sor-form camps repair-by-options status-options))))

(defun render-sor-report (camp-id all-camps-p date-from date-to repair-by status-filter contract-week camps)
  "Render the SOR report results."
  (let* ((summary (get-sor-summary :camp-id (unless all-camps-p camp-id)
                                   :date-from date-from :date-to date-to
                                   :repair-by (when (and repair-by (> (length repair-by) 0)) repair-by)
                                   :status (when (and status-filter (> (length status-filter) 0)) status-filter)))
         (by-responsibility (get-sor-by-repair-responsibility 
                             :camp-id (unless all-camps-p camp-id) :date-from date-from :date-to date-to))
         (by-camp (when all-camps-p
                    (get-sor-by-camp :date-from date-from :date-to date-to
                                     :repair-by (when (and repair-by (> (length repair-by) 0)) repair-by))))
         (details (get-sor-details :camp-id (unless all-camps-p camp-id)
                                   :date-from date-from :date-to date-to
                                   :repair-by (when (and repair-by (> (length repair-by) 0)) repair-by)
                                   :status (when (and status-filter (> (length status-filter) 0)) status-filter)
                                   :limit 500))
         (camp (when camp-id (get-camp camp-id)))
         (total (or (getf summary :|total|) 0))
         (open-count (or (getf summary :|open_count|) 0))
         (closed-count (or (getf summary :|closed_count|) 0))
         (reinsp-count (or (getf summary :|reinsp_count|) 0)))
    (html-response
     (render-page (format nil "SOR Report - ~A" (if all-camps-p "All Sites" (or (getf camp :|name|) "Report")))
       (cl-who:with-html-output-to-string (s)
         (:style (cl-who:str *sor-report-styles*))
         (:div :class "page-header"
           (:h1 "SOR Report")
           (:div :class "no-print" :style "margin-top: 0.5rem;"
             (:a :href (format nil "/reports/sor/pdf?~A" (hunchentoot:query-string*))
                 :class "btn btn-primary" "Download PDF")
             (:a :href "/reports/sor" :class "btn btn-secondary" :style "margin-left: 0.5rem;" "New Search")))
         ;; Report Header
         (:div :class "card"
           (:div :class "report-meta"
             (:p (:strong "Site: ") (cl-who:str (if all-camps-p "All Sites" (or (getf camp :|name|) "Unknown"))))
             (when (and contract-week (> (length contract-week) 0))
               (cl-who:htm (:p (:strong "Contract Week: ") (cl-who:str contract-week))))
             (when (and date-from (> (length date-from) 0))
               (cl-who:htm (:p (:strong "Date Range: ") (cl-who:fmt "~A to ~A" date-from date-to))))
             (when (and repair-by (> (length repair-by) 0))
               (cl-who:htm (:p (:strong "Repair By: ") (cl-who:str repair-by))))
             (when (and status-filter (> (length status-filter) 0))
               (cl-who:htm (:p (:strong "Status: ") (cl-who:str status-filter))))
             (:p (:strong "Generated: ") (cl-who:str (local-time:format-timestring nil (local-time:now) 
                                                       :format '(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))))))
         ;; Summary Cards
         (:section :class "card"
           (:h2 "Summary")
           (:div :class "summary-grid"
             (:div :class "summary-card total"
               (:span :class "summary-value" (cl-who:str total))
               (:span :class "summary-label" "Total SORs"))
             (:div :class "summary-card open"
               (:span :class "summary-value" (cl-who:str open-count))
               (:span :class "summary-label" "Open"))
             (:div :class "summary-card closed"
               (:span :class "summary-value" (cl-who:str closed-count))
               (:span :class "summary-label" "Closed"))
             (:div :class "summary-card reinsp"
               (:span :class "summary-value" (cl-who:str reinsp-count))
               (:span :class "summary-label" "Re-Inspection"))))
         ;; By Repair Responsibility
         (when by-responsibility
           (cl-who:htm
            (:section :class "card"
              (:h2 "By Repair Responsibility")
              (:div :class "responsibility-grid"
                (dolist (resp by-responsibility)
                  (cl-who:htm
                   (:div :class "responsibility-card"
                     (:h4 (cl-who:str (or (getf resp :|repair_by|) "Unassigned")))
                     (:div :class "responsibility-stats"
                       (:div :class "responsibility-stat" (:strong (cl-who:str (getf resp :|total|))) "Total")
                       (:div :class "responsibility-stat" (:strong :style "color:#e53e3e;" (cl-who:str (or (getf resp :|open_count|) 0))) "Open")
                       (:div :class "responsibility-stat" (:strong :style "color:#38a169;" (cl-who:str (or (getf resp :|closed_count|) 0))) "Closed")
                       (:div :class "responsibility-stat" (:strong :style "color:#d69e2e;" (cl-who:str (or (getf resp :|reinsp_count|) 0))) "Re-Insp")))))))))
         ;; By Camp
         (when by-camp
           (cl-who:htm
            (:section :class "card"
              (:h2 "By Site")
              (:table :class "data-table"
                (:thead (:tr (:th "Site") (:th "Country") (:th "Total") (:th "Open") (:th "Closed") (:th "Re-Insp")))
                (:tbody
                  (dolist (c by-camp)
                    (cl-who:htm
                     (:tr
                       (:td (cl-who:str (getf c :|camp_name|)))
                       (:td (cl-who:str (or (getf c :|country|) "-")))
                       (:td (cl-who:str (getf c :|total|)))
                       (:td :style "color:#e53e3e;" (cl-who:str (or (getf c :|open_count|) 0)))
                       (:td :style "color:#38a169;" (cl-who:str (or (getf c :|closed_count|) 0)))
                       (:td :style "color:#d69e2e;" (cl-who:str (or (getf c :|reinsp_count|) 0)))))))))))
         ;; Details Table
         (:section :class "card"
           (:h2 (cl-who:fmt "Detailed Records (~A shown)" (length details)))
           (:table :class "data-table"
             (:thead (:tr (:th "Site") (:th "Building") (:th "Def #") (:th "Category") (:th "Repair By") (:th "SOR #") (:th "Status") (:th "RAC") (:th "Insp Date")))
             (:tbody
               (if details
                   (dolist (d details)
                     (cl-who:htm
                      (:tr
                        (:td (cl-who:str (or (getf d :|camp_name|) "-")))
                        (:td (cl-who:str (or (getf d :|building_number|) "-")))
                        (:td (cl-who:str (or (getf d :|deficiency_number|) "-")))
                        (:td (cl-who:str (or (getf d :|def_category|) "-")))
                        (:td (cl-who:str (or (getf d :|repair_by|) "-")))
                        (:td (cl-who:str (or (getf d :|om_sor_number|) "-")))
                        (:td (:span :class "status-badge" (cl-who:str (or (getf d :|deficiency_status|) "-"))))
                        (:td (cl-who:str (or (getf d :|rac|) "-")))
                        (:td (cl-who:str (format-date-display (getf d :|inspection_date|)))))))
                   (cl-who:htm (:tr (:td :colspan "9" :class "empty-state" "No records found.")))))))
         (:div :class "page-actions no-print"
           (:a :href "/reports/sor" :class "btn" "Back to SOR Reports")))))))

(defun render-sor-form (camps repair-by-options status-options)
  "Render the SOR report filter form."
  (let* ((current-period (or (get-system-setting "contract_current_period") "BY"))
         (available-weeks (get-available-contract-weeks current-period)))
    (html-response
     (render-page "SOR Reports"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header" (:h1 "SOR Reports"))
         (:div :class "card"
           (:h2 "Generate SOR Report")
           (:p "Select filters to generate an SOR (Statement of Requirement) report.")
           (:form :method "get" :action "/reports/sor" :class "report-form" :id "sor-report-form"
             (:div :class "form-row"
               (:div :class "form-group"
                 (:label "Site")
                 (:select :name "camp"
                   (:option :value "" "Select Site...")
                   (:option :value "all" "All Sites")
                   (dolist (c camps)
                     (cl-who:htm
                      (:option :value (princ-to-string (getf c :|id|))
                               (cl-who:fmt "~A (~A)" (getf c :|name|) (or (getf c :|country|) "-")))))))
               (:div :class "form-group"
                 (:label "Repair By")
                 (:select :name "repair_by"
                   (:option :value "" "All")
                   (dolist (opt repair-by-options)
                     (cl-who:htm (:option :value (getf opt :|repair_by|) (cl-who:str (getf opt :|repair_by|)))))))
               (:div :class "form-group"
                 (:label "Status")
                 (:select :name "status"
                   (:option :value "" "All Statuses")
                   (dolist (opt status-options)
                     (cl-who:htm (:option :value (getf opt :|deficiency_status|) (cl-who:str (getf opt :|deficiency_status|))))))))
             ;; Contract Week
             (:div :class "form-row" :style "background: #f8f9fa; padding: 1rem; border-radius: 4px; margin-bottom: 1rem;"
               (:div :class "form-group"
                 (:label (:strong "Option 1: Contract Week"))
                 (:select :name "contract_week" :id "contract-week-select" :onchange "updateDatesFromWeek()"
                   (:option :value "" "-- Select Week --")
                   (dolist (week available-weeks)
                     (cl-who:htm
                      (:option :value (format nil "~A-~A" current-period (getf week :week-number))
                               :data-start (getf week :week-start) :data-end (getf week :week-end)
                               (cl-who:fmt "~A Week ~A (~A to ~A)" current-period (getf week :week-number)
                                           (getf week :week-start) (getf week :week-end))))))))
             ;; Date Range
             (:div :class "form-row"
               (:div :class "form-group" (:label (:strong "Option 2: Custom Date Range")))
               (:div :class "form-group" (:label "From Date") (:input :type "date" :name "date_from" :id "date-from"))
               (:div :class "form-group" (:label "To Date") (:input :type "date" :name "date_to" :id "date-to")))
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
}"))))))
