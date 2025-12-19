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
  (cl-who:with-html-output-to-string (s)
    (:nav :class "navbar"
      (:div :class "nav-logos"
        (:img :src "/static/img/company_logo.png" :alt "Versar" :class "logo-company")
        (:img :src "/static/img/TFS_Logo.png" :alt "Task Force SAFE" :class "logo-tfs"))
      (:div :class "nav-brand"
        (:a :href "/" "TF SAFE CMMS"))
      (:ul :class "nav-links"
        (:li (:a :href "/work-orders" "Work Orders"))
        (:li (:a :href "/sites" "Sites"))
        (:li (:a :href "/inventory" "Inventory"))
        (:li (:a :href "/reports" "Reports"))))))

(defun render-page (title content)
  "Wrap content in page layout."
  (with-html
    (:html
     (:head
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title (cl-who:str (format nil "TF SAFE CMMS - ~A" title)))
      (:link :rel "stylesheet" :href "/static/css/style.css"))
     (:body
      (cl-who:str (render-nav))
      (:main :class "container"
        (cl-who:str content))
      (:footer :class "footer"
        (:p "Task Force SAFE - Computerized Maintenance Management System"))
      (:script :src "/static/js/app.js")))))

;;; Route handlers

(defun handle-index ()
  "Dashboard - redirect to work orders."
  (redirect-to "/work-orders"))

(defun handle-work-orders ()
  "Work orders list page."
  (let* ((site-id (parse-int (get-param "site")))
         (status-param (get-param "status"))
         (status (when (and status-param (> (length status-param) 0)) status-param))
         (date-from (get-param "date_from"))
         (date-to (get-param "date_to"))
         (work-orders (list-work-orders :site-id site-id :status status 
                                        :date-from date-from :date-to date-to :limit 200))
         (sites (list-sites)))
    (html-response
     (render-page "Work Orders"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Work Orders")
           (:a :href "/work-orders/new" :class "btn btn-primary" "+ New Work Order"))
         (:div :class "filters"
           (:form :method "get" :action "/work-orders" :class "filter-form"
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
  (let ((wo (get-work-order (parse-int id))))
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
                            :class "btn btn-sm" "Report")))))
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

;;; Static file handler

(defun handle-static ()
  "Serve static files."
  (let* ((uri (hunchentoot:request-uri*))
         (path (subseq uri 8)) ; Remove "/static/"
         (file-path (merge-pathnames path *static-directory*)))
    (if (probe-file file-path)
        (hunchentoot:handle-static-file file-path)
        (setf (hunchentoot:return-code*) 404))))

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
      ((string= uri "/reports")
       (handle-reports))
      
      ;; API endpoints
      ((and (eq method :post) (string= uri "/api/sites/create"))
       (handle-api-sites-create))
      ((and (eq method :post) (string= uri "/api/work-orders/create"))
       (handle-api-work-orders-create))
      ((and (eq method :post) (cl-ppcre:scan "^/api/work-orders/(\\d+)/update$" uri))
       (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^/api/work-orders/(\\d+)/update$" uri)
         (declare (ignore match))
         (handle-api-work-orders-update (aref groups 0))))
      
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
