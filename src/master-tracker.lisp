(in-package #:tfs-cmms)

;;; Master Tracker Handlers
;;; Handles master deficiency tracking and weekly report generation

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
         (contract-week (get-param "contract_week"))
         (current-period (or (get-system-setting "contract_current_period") "BY"))
         (available-weeks (get-available-contract-weeks current-period))
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
                     (:label "Contract Week")
                     (:select :name "contract_week" :id "mt-contract-week" :onchange "updateMtDatesFromWeek()"
                       (:option :value "" "-- Select --")
                       (dolist (week available-weeks)
                         (cl-who:htm
                          (:option :value (format nil "~A-~A" current-period (getf week :week-number))
                                   :data-start (getf week :week-start)
                                   :data-end (getf week :week-end)
                                   :selected (string= contract-week (format nil "~A-~A" current-period (getf week :week-number)))
                                   (cl-who:fmt "~A Wk ~A" current-period (getf week :week-number)))))))
                   (:div :class "form-group"
                     (:label "Date From")
                     (:input :type "date" :name "date_from" :id "mt-date-from" :value (or date-from "")))
                   (:div :class "form-group"
                     (:label "Date To")
                     (:input :type "date" :name "date_to" :id "mt-date-to" :value (or date-to "")))
                   (:div :class "form-group" :style "align-self: flex-end;"
                     (:button :type "submit" :class "btn" "Apply Filters")))
                 (:script "
function updateMtDatesFromWeek() {
  var select = document.getElementById('mt-contract-week');
  var option = select.options[select.selectedIndex];
  if (option.value) {
    document.getElementById('mt-date-from').value = option.dataset.start;
    document.getElementById('mt-date-to').value = option.dataset.end;
  }
}
")))
             
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
         (contract-week (get-param "contract_week"))
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
                          (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))
         (current-period (or (get-system-setting "contract_current_period") "BY"))
         (available-weeks (get-available-contract-weeks current-period)))
    (let* ((start-date (or week-start default-start))
           (end-date (or week-end default-end))
           (report-dir (format nil "week_~A_to_~A" start-date end-date))
           (report-path (merge-pathnames (make-pathname :directory `(:relative "reports" ,report-dir))
                                         *base-directory*))
           (images-exist (probe-file (merge-pathnames "01_operational_updates.png" report-path)))
           (generated-now nil))
      ;; Generate report if requested
      (when generate-p
        (let ((python-path "python3")
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
             (:p "Select a contract week OR enter custom dates.")
             (:form :method "get" :action "/master-tracker/weekly-report"
               ;; Contract Week Selection
               (:div :class "form-row" :style "background: #f8f9fa; padding: 1rem; border-radius: 4px; margin-bottom: 1rem;"
                 (:div :class "form-group"
                   (:label (:strong "Option 1: Contract Week"))
                   (:select :name "contract_week" :id "contract-week-select" :onchange "updateWeeklyDatesFromWeek()"
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
                   (:label "Week Start")
                   (:input :type "date" :name "week_start" :id "week-start" :value start-date))
                 (:div :class "form-group"
                   (:label "Week End")
                   (:input :type "date" :name "week_end" :id "week-end" :value end-date))
                 (:div :class "form-group" :style "align-self: flex-end;"
                   (:input :type "hidden" :name "generate" :value "1")
                   (:button :type "submit" :class "btn btn-primary" "Generate Report Slides"))))
             (:script "
function updateWeeklyDatesFromWeek() {
  var select = document.getElementById('contract-week-select');
  var option = select.options[select.selectedIndex];
  if (option.value) {
    document.getElementById('week-start').value = option.dataset.start;
    document.getElementById('week-end').value = option.dataset.end;
  }
}
"))
           
           ;; Show generated images if they exist
           (when images-exist
             (cl-who:htm
              (:div :class "alert alert-success" :style "margin: 1rem 0; padding: 1rem; background: #d4edda; border-radius: 4px;"
                "✓ Report slides available for download.")
              (:section :class "card"
                (:h2 "Generated Slides")
                (:p "Click on any image to download it for PowerPoint.")
                (:div :class "form-actions" :style "margin-bottom: 1rem; flex-wrap: wrap;"
                  (:a :href (format nil "/reports/~A/01_operational_updates.png" report-dir)
                      :download "01_operational_updates.png" :class "btn" "Download Slide 1")
                  (:a :href (format nil "/reports/~A/02_weekly_by_country.png" report-dir)
                      :download "02_weekly_by_country.png" :class "btn" "Download Slide 2")
                  (:a :href (format nil "/reports/~A/03_deficiency_types.png" report-dir)
                      :download "03_deficiency_types.png" :class "btn" "Download Slide 3")
                  (:a :href (format nil "/reports/~A/04_cumulative_stats.png" report-dir)
                      :download "04_cumulative_stats.png" :class "btn" "Download Slide 4")
                  (:a :href (format nil "/reports/~A/05_90_day_stats.png" report-dir)
                      :download "05_90_day_stats.png" :class "btn" "Download Slide 5")
                  (:a :href (format nil "/reports/~A/06_combined_deficiency_comparison.png" report-dir)
                      :download "06_combined_deficiency_comparison.png" :class "btn" "Download Slide 6")
                  (:a :href (format nil "/reports/~A/07_repair_responsibility.png" report-dir)
                      :download "07_repair_responsibility.png" :class "btn" "Download Slide 7")))
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
                      :style "max-width: 100%; border: 1px solid #ccc;"))
              (:section :class "card"
                (:h3 "Slide 6: Combined Deficiency Comparison")
                (:img :src (format nil "/reports/~A/06_combined_deficiency_comparison.png" report-dir)
                      :style "max-width: 100%; border: 1px solid #ccc;"))
              (:section :class "card"
                (:h3 "Slide 7: Repair Responsibility")
                (:img :src (format nil "/reports/~A/07_repair_responsibility.png" report-dir)
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
