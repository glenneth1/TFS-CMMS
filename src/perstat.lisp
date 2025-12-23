;;;; perstat.lisp - Personnel Status and Movement Tracking
;;;;
;;;; This module handles:
;;;; - Personnel roster management
;;;; - Location tracking (current base/country)
;;;; - Movement requests (AMR and Space-A)
;;;; - PERSTAT report generation (PDF and Excel)

(in-package #:tfs-cmms)

;;; Database Schema Setup

(defun ensure-perstat-tables ()
  "Create personnel and movement tracking tables if they don't exist."
  ;; Personnel roster (separate from users - includes non-system users)
  (execute-sql "CREATE TABLE IF NOT EXISTS personnel (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    full_name TEXT NOT NULL,
    nationality TEXT,
    role_category TEXT,  -- 'Electrician', 'PMO', 'QC', 'Property/Materials'
    blood_type TEXT,
    body_weight_lbs INTEGER,
    passport_number TEXT,
    dodi_number TEXT,
    bag_types TEXT,
    bag_count INTEGER,
    bag_weight_lbs INTEGER,
    current_location TEXT,  -- Free text for remote staff (e.g., UK)
    current_camp_id INTEGER REFERENCES camps(id),
    user_id INTEGER REFERENCES users(id),  -- Link to system user if applicable
    status TEXT DEFAULT 'Active',  -- Active, On Leave, Departed
    notes TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
  )")
  
  ;; Movement requests
  (execute-sql "CREATE TABLE IF NOT EXISTS movement_requests (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    request_number TEXT UNIQUE,
    movement_type TEXT NOT NULL,  -- 'AMR', 'Space-A'
    status TEXT DEFAULT 'Pending',  -- Pending, Approved, In Transit, Completed, Cancelled
    from_location TEXT NOT NULL,
    from_camp_id INTEGER REFERENCES camps(id),
    to_location TEXT NOT NULL,
    to_camp_id INTEGER REFERENCES camps(id),
    requested_date DATE,
    actual_date DATE,
    requested_by INTEGER REFERENCES users(id),
    approved_by INTEGER REFERENCES users(id),
    notes TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
  )")
  
  ;; Personnel on each movement request (many-to-many)
  (execute-sql "CREATE TABLE IF NOT EXISTS movement_request_personnel (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    movement_request_id INTEGER NOT NULL REFERENCES movement_requests(id) ON DELETE CASCADE,
    personnel_id INTEGER NOT NULL REFERENCES personnel(id),
    UNIQUE(movement_request_id, personnel_id)
  )")
  
  ;; Location history for audit trail
  (execute-sql "CREATE TABLE IF NOT EXISTS personnel_location_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    personnel_id INTEGER NOT NULL REFERENCES personnel(id),
    location TEXT,
    camp_id INTEGER REFERENCES camps(id),
    movement_request_id INTEGER REFERENCES movement_requests(id),
    changed_at DATETIME DEFAULT CURRENT_TIMESTAMP
  )"))

;;; Personnel CRUD Operations

(defun create-personnel (&key full-name nationality role-category blood-type 
                              body-weight-lbs passport-number dodi-number
                              bag-types bag-count bag-weight-lbs
                              current-location current-camp-id user-id notes)
  "Create a new personnel record."
  (execute-sql 
   "INSERT INTO personnel (full_name, nationality, role_category, blood_type,
                          body_weight_lbs, passport_number, dodi_number,
                          bag_types, bag_count, bag_weight_lbs,
                          current_location, current_camp_id, user_id, notes)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   full-name nationality role-category blood-type
   body-weight-lbs passport-number dodi-number
   bag-types bag-count bag-weight-lbs
   current-location current-camp-id user-id notes)
  (fetch-one "SELECT last_insert_rowid() as id"))

(defun get-personnel (id)
  "Get a personnel record by ID."
  (fetch-one 
   "SELECT p.*, c.name as camp_name, co.name as country_name
    FROM personnel p
    LEFT JOIN camps c ON p.current_camp_id = c.id
    LEFT JOIN countries co ON c.country_id = co.id
    WHERE p.id = ?" id))

(defun list-personnel (&key role-category status camp-id search)
  "List personnel with optional filters."
  (let ((query "SELECT p.*, c.name as camp_name, co.name as country_name
                FROM personnel p
                LEFT JOIN camps c ON p.current_camp_id = c.id
                LEFT JOIN countries co ON c.country_id = co.id
                WHERE 1=1")
        (params nil))
    (when role-category
      (setf query (concatenate 'string query " AND p.role_category = ?"))
      (push role-category params))
    (when status
      (setf query (concatenate 'string query " AND p.status = ?"))
      (push status params))
    (when camp-id
      (setf query (concatenate 'string query " AND p.current_camp_id = ?"))
      (push camp-id params))
    (when search
      (setf query (concatenate 'string query " AND (p.full_name LIKE ? OR p.passport_number LIKE ?)"))
      (let ((search-pattern (format nil "%~A%" search)))
        (push search-pattern params)
        (push search-pattern params)))
    (setf query (concatenate 'string query " ORDER BY p.full_name"))
    (apply #'fetch-all query (reverse params))))

(defun update-personnel (id &key full-name nationality role-category blood-type
                              body-weight-lbs passport-number dodi-number
                              bag-types bag-count bag-weight-lbs
                              current-location current-camp-id status notes)
  "Update a personnel record."
  (let ((sets nil)
        (params nil))
    (when full-name (push "full_name = ?" sets) (push full-name params))
    (when nationality (push "nationality = ?" sets) (push nationality params))
    (when role-category (push "role_category = ?" sets) (push role-category params))
    (when blood-type (push "blood_type = ?" sets) (push blood-type params))
    (when body-weight-lbs (push "body_weight_lbs = ?" sets) (push body-weight-lbs params))
    (when passport-number (push "passport_number = ?" sets) (push passport-number params))
    (when dodi-number (push "dodi_number = ?" sets) (push dodi-number params))
    (when bag-types (push "bag_types = ?" sets) (push bag-types params))
    (when bag-count (push "bag_count = ?" sets) (push bag-count params))
    (when bag-weight-lbs (push "bag_weight_lbs = ?" sets) (push bag-weight-lbs params))
    (when current-location (push "current_location = ?" sets) (push current-location params))
    (when current-camp-id (push "current_camp_id = ?" sets) (push current-camp-id params))
    (when status (push "status = ?" sets) (push status params))
    (when notes (push "notes = ?" sets) (push notes params))
    (push "updated_at = CURRENT_TIMESTAMP" sets)
    (when sets
      (let ((query (format nil "UPDATE personnel SET ~{~A~^, ~} WHERE id = ?"
                           (reverse sets))))
        (apply #'execute-sql query (append (reverse params) (list id)))))))

(defun update-personnel-location (personnel-id new-location &key camp-id movement-request-id)
  "Update personnel location and record in history."
  ;; Record history
  (execute-sql 
   "INSERT INTO personnel_location_history (personnel_id, location, camp_id, movement_request_id)
    VALUES (?, ?, ?, ?)"
   personnel-id new-location camp-id movement-request-id)
  ;; Update current location
  (execute-sql 
   "UPDATE personnel SET current_location = ?, current_camp_id = ?, updated_at = CURRENT_TIMESTAMP
    WHERE id = ?"
   new-location camp-id personnel-id))

;;; Movement Request Operations

(defun generate-movement-request-number ()
  "Generate a unique movement request number."
  (let* ((today (multiple-value-bind (sec min hour day month year)
                    (decode-universal-time (get-universal-time))
                  (declare (ignore sec min hour))
                  (format nil "~4,'0D~2,'0D~2,'0D" year month day)))
         (count (getf (fetch-one 
                       "SELECT COUNT(*) as c FROM movement_requests 
                        WHERE request_number LIKE ?" 
                       (format nil "MR-~A%" today)) :|c|)))
    (format nil "MR-~A-~3,'0D" today (1+ (or count 0)))))

(defun create-movement-request (&key movement-type from-location from-camp-id
                                     to-location to-camp-id requested-date
                                     requested-by notes personnel-ids)
  "Create a new movement request with associated personnel."
  (let ((request-number (generate-movement-request-number)))
    (execute-sql 
     "INSERT INTO movement_requests (request_number, movement_type, from_location, from_camp_id,
                                    to_location, to_camp_id, requested_date, requested_by, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     request-number movement-type from-location from-camp-id
     to-location to-camp-id requested-date requested-by notes)
    (let ((request-id (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))
      ;; Add personnel to the request
      (dolist (pid personnel-ids)
        (execute-sql 
         "INSERT INTO movement_request_personnel (movement_request_id, personnel_id) VALUES (?, ?)"
         request-id pid))
      request-id)))

(defun get-movement-request (id)
  "Get a movement request by ID with personnel list."
  (let ((request (fetch-one 
                  "SELECT mr.*, 
                          fc.name as from_camp_name, fco.name as from_country_name,
                          tc.name as to_camp_name, tco.name as to_country_name,
                          u.full_name as requested_by_name,
                          au.full_name as approved_by_name
                   FROM movement_requests mr
                   LEFT JOIN camps fc ON mr.from_camp_id = fc.id
                   LEFT JOIN countries fco ON fc.country_id = fco.id
                   LEFT JOIN camps tc ON mr.to_camp_id = tc.id
                   LEFT JOIN countries tco ON tc.country_id = tco.id
                   LEFT JOIN users u ON mr.requested_by = u.id
                   LEFT JOIN users au ON mr.approved_by = au.id
                   WHERE mr.id = ?" id)))
    (when request
      (setf (getf request :personnel)
            (fetch-all 
             "SELECT p.* FROM personnel p
              JOIN movement_request_personnel mrp ON p.id = mrp.personnel_id
              WHERE mrp.movement_request_id = ?" id)))
    request))

(defun list-movement-requests (&key status movement-type limit)
  "List movement requests with optional filters."
  (let ((query "SELECT mr.*, 
                       fc.name as from_camp_name, tc.name as to_camp_name,
                       u.full_name as requested_by_name,
                       (SELECT COUNT(*) FROM movement_request_personnel WHERE movement_request_id = mr.id) as personnel_count
                FROM movement_requests mr
                LEFT JOIN camps fc ON mr.from_camp_id = fc.id
                LEFT JOIN camps tc ON mr.to_camp_id = tc.id
                LEFT JOIN users u ON mr.requested_by = u.id
                WHERE 1=1")
        (params nil))
    (when status
      (setf query (concatenate 'string query " AND mr.status = ?"))
      (push status params))
    (when movement-type
      (setf query (concatenate 'string query " AND mr.movement_type = ?"))
      (push movement-type params))
    (setf query (concatenate 'string query " ORDER BY mr.created_at DESC"))
    (when limit
      (setf query (concatenate 'string query (format nil " LIMIT ~A" limit))))
    (apply #'fetch-all query (reverse params))))

(defun update-movement-request-status (id new-status &key approved-by actual-date)
  "Update movement request status."
  (execute-sql 
   "UPDATE movement_requests SET status = ?, approved_by = ?, actual_date = ?, 
    updated_at = CURRENT_TIMESTAMP WHERE id = ?"
   new-status approved-by actual-date id)
  ;; If completed, update personnel locations
  (when (string= new-status "Completed")
    (let ((request (get-movement-request id)))
      (dolist (person (getf request :personnel))
        (update-personnel-location 
         (getf person :|id|)
         (or (getf request :|to_location|) (getf request :|to_camp_name|))
         :camp-id (getf request :|to_camp_id|)
         :movement-request-id id)))))

;;; Personnel Categories

(defvar *personnel-categories*
  '("Electrician" "PMO" "QC" "Property/Materials")
  "Valid personnel role categories.")

(defvar *blood-types*
  '("A Pos" "A Neg" "B Pos" "B Neg" "AB Pos" "AB Neg" "O Pos" "O Neg")
  "Valid blood types.")

(defvar *movement-types*
  '("AMR" "Space-A")
  "Valid movement request types.")

(defvar *movement-statuses*
  '("Pending" "Approved" "In Transit" "Completed" "Cancelled")
  "Valid movement request statuses.")

;;; PERSTAT Report Generation

(defun get-perstat-data (&key role-category camp-id country-id)
  "Get personnel data for PERSTAT report."
  (let ((query "SELECT p.*, c.name as camp_name, co.name as country_name
                FROM personnel p
                LEFT JOIN camps c ON p.current_camp_id = c.id
                LEFT JOIN countries co ON c.country_id = co.id
                WHERE p.status = 'Active'")
        (params nil))
    (when role-category
      (setf query (concatenate 'string query " AND p.role_category = ?"))
      (push role-category params))
    (when camp-id
      (setf query (concatenate 'string query " AND p.current_camp_id = ?"))
      (push camp-id params))
    (when country-id
      (setf query (concatenate 'string query " AND co.id = ?"))
      (push country-id params))
    (setf query (concatenate 'string query " ORDER BY p.role_category, p.full_name"))
    (apply #'fetch-all query (reverse params))))

(defun get-perstat-summary ()
  "Get summary counts for PERSTAT."
  (fetch-all 
   "SELECT 
      p.role_category,
      co.name as country_name,
      c.name as camp_name,
      COUNT(*) as count
    FROM personnel p
    LEFT JOIN camps c ON p.current_camp_id = c.id
    LEFT JOIN countries co ON c.country_id = co.id
    WHERE p.status = 'Active'
    GROUP BY p.role_category, co.name, c.name
    ORDER BY p.role_category, co.name, c.name"))

;;; Web Handlers

(defun handle-perstat-dashboard ()
  "Personnel status dashboard."
  (let* ((user (get-current-user))
         (summary (get-perstat-summary))
         (recent-movements (list-movement-requests :limit 10))
         (personnel-count (getf (fetch-one "SELECT COUNT(*) as c FROM personnel WHERE status = 'Active'") :|c|)))
    (html-response
     (render-page "Personnel Status"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Personnel Status (PERSTAT)")
           (:div :class "header-actions"
             (:a :href "/perstat/personnel" :class "btn" "View Roster")
             (:a :href "/perstat/personnel/new" :class "btn btn-primary" "+ Add Personnel")
             (:a :href "/perstat/movements" :class "btn" "Movement Requests")
             (:a :href "/perstat/report" :class "btn" "Generate PERSTAT")))
         
         ;; Summary stats
         (:section :class "card"
           (:h2 "Current Status")
           (:div :class "dashboard-stats"
             (:div :class "stat-card"
               (:h3 "Total Active Personnel")
               (:p :class "stat-number" (cl-who:str (or personnel-count 0))))
             (dolist (cat *personnel-categories*)
               (let ((cat-count (reduce #'+ (remove-if-not 
                                             (lambda (s) (string= (getf s :|role_category|) cat))
                                             summary)
                                        :key (lambda (s) (or (getf s :|count|) 0)))))
                 (cl-who:htm
                  (:div :class "stat-card"
                    (:h3 (cl-who:str cat))
                    (:p :class "stat-number" (cl-who:str cat-count))))))))
         
         ;; Personnel by location
         (:section :class "card"
           (:h2 "Personnel by Location")
           (if summary
               (cl-who:htm
                (:table :class "data-table"
                  (:thead
                    (:tr (:th "Category") (:th "Country") (:th "Camp") (:th "Count")))
                  (:tbody
                    (dolist (row summary)
                      (cl-who:htm
                       (:tr
                         (:td (cl-who:str (or (getf row :|role_category|) "-")))
                         (:td (cl-who:str (or (getf row :|country_name|) "Remote")))
                         (:td (cl-who:str (or (getf row :|camp_name|) "-")))
                         (:td (cl-who:str (getf row :|count|)))))))))
               (cl-who:htm
                (:p :class "empty-state" "No personnel records found."))))
         
         ;; Recent movement requests
         (:section :class "card"
           (:h2 "Recent Movement Requests")
           (if recent-movements
               (cl-who:htm
                (:table :class "data-table"
                  (:thead
                    (:tr (:th "Request #") (:th "Type") (:th "From") (:th "To") 
                         (:th "Date") (:th "Personnel") (:th "Status")))
                  (:tbody
                    (dolist (mr recent-movements)
                      (cl-who:htm
                       (:tr
                         (:td (:a :href (format nil "/perstat/movements/~A" (getf mr :|id|))
                                  (cl-who:str (getf mr :|request_number|))))
                         (:td (cl-who:str (getf mr :|movement_type|)))
                         (:td (cl-who:str (or (getf mr :|from_camp_name|) (getf mr :|from_location|))))
                         (:td (cl-who:str (or (getf mr :|to_camp_name|) (getf mr :|to_location|))))
                         (:td (cl-who:str (or (getf mr :|requested_date|) "-")))
                         (:td (cl-who:str (getf mr :|personnel_count|)))
                         (:td (:span :class (format nil "badge badge-~A" 
                                                    (string-downcase (getf mr :|status|)))
                                     (cl-who:str (getf mr :|status|))))))))))
               (cl-who:htm
                (:p :class "empty-state" "No movement requests found.")))))))))

(defun handle-perstat-personnel-list ()
  "List all personnel."
  (let* ((role-filter (get-param "role"))
         (status-filter (or (get-param "status") "Active"))
         (search (get-param "search"))
         (personnel (list-personnel :role-category (when (and role-filter (> (length role-filter) 0)) role-filter)
                                    :status (when (and status-filter (> (length status-filter) 0)) status-filter)
                                    :search search)))
    (html-response
     (render-page "Personnel Roster"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Personnel Roster")
           (:a :href "/perstat/personnel/new" :class "btn btn-primary" "+ Add Personnel"))
         
         ;; Filters
         (:section :class "card"
           (:form :method "get" :action "/perstat/personnel" :class "filter-form"
             (:div :class "form-row"
               (:div :class "form-group"
                 (:label "Role Category")
                 (:select :name "role" :onchange "this.form.submit()"
                   (:option :value "" "-- All --")
                   (dolist (cat *personnel-categories*)
                     (cl-who:htm
                      (:option :value cat :selected (string= role-filter cat)
                               (cl-who:str cat))))))
               (:div :class "form-group"
                 (:label "Status")
                 (:select :name "status" :onchange "this.form.submit()"
                   (:option :value "" "-- All --")
                   (:option :value "Active" :selected (string= status-filter "Active") "Active")
                   (:option :value "On Leave" :selected (string= status-filter "On Leave") "On Leave")
                   (:option :value "Departed" :selected (string= status-filter "Departed") "Departed")))
               (:div :class "form-group"
                 (:label "Search")
                 (:input :type "text" :name "search" :value (or search "") :placeholder "Name or passport..."))
               (:div :class "form-group" :style "align-self: flex-end;"
                 (:button :type "submit" :class "btn" "Filter")))))
         
         ;; Personnel table
         (:section :class "card"
           (if personnel
               (cl-who:htm
                (:table :class "data-table"
                  (:thead
                    (:tr (:th "Name") (:th "Category") (:th "Location") (:th "Nationality")
                         (:th "Blood Type") (:th "Status") (:th "Actions")))
                  (:tbody
                    (dolist (p personnel)
                      (cl-who:htm
                       (:tr
                         (:td (cl-who:str (getf p :|full_name|)))
                         (:td (cl-who:str (or (getf p :|role_category|) "-")))
                         (:td (cl-who:str (or (getf p :|camp_name|) (getf p :|current_location|) "-")))
                         (:td (cl-who:str (or (getf p :|nationality|) "-")))
                         (:td (cl-who:str (or (getf p :|blood_type|) "-")))
                         (:td (:span :class (format nil "badge badge-~A" 
                                                    (string-downcase (or (getf p :|status|) "active")))
                                     (cl-who:str (or (getf p :|status|) "Active"))))
                         (:td 
                           (:a :href (format nil "/perstat/personnel/~A" (getf p :|id|))
                               :class "btn btn-sm" "View")
                           (:a :href (format nil "/perstat/personnel/~A/edit" (getf p :|id|))
                               :class "btn btn-sm" "Edit"))))))))
               (cl-who:htm
                (:p :class "empty-state" "No personnel found.")))))))))

(defun handle-perstat-personnel-new ()
  "New personnel form."
  (let ((camps (fetch-all "SELECT c.id, c.name, co.name as country_name 
                           FROM camps c 
                           JOIN countries co ON c.country_id = co.id 
                           ORDER BY co.name, c.name")))
    (html-response
     (render-page "Add Personnel"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Add Personnel"))
         (:form :method "post" :action "/api/perstat/personnel/create" :class "form-card"
           (:div :class "form-row"
             (:div :class "form-group required"
               (:label "Full Name")
               (:input :type "text" :name "full_name" :required t))
             (:div :class "form-group required"
               (:label "Role Category")
               (:select :name "role_category" :required t
                 (:option :value "" "-- Select --")
                 (dolist (cat *personnel-categories*)
                   (cl-who:htm (:option :value cat (cl-who:str cat)))))))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "Nationality")
               (:input :type "text" :name "nationality" :placeholder "e.g., British - United Kingdom"))
             (:div :class "form-group"
               (:label "Blood Type")
               (:select :name "blood_type"
                 (:option :value "" "-- Select --")
                 (dolist (bt *blood-types*)
                   (cl-who:htm (:option :value bt (cl-who:str bt)))))))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "Body Weight (lbs)")
               (:input :type "number" :name "body_weight_lbs"))
             (:div :class "form-group"
               (:label "Passport Number")
               (:input :type "text" :name "passport_number")))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "DODI Number")
               (:input :type "text" :name "dodi_number"))
             (:div :class "form-group"
               (:label "Bag Types")
               (:input :type "text" :name "bag_types" :placeholder "e.g., Hard Shell Tool Box / Duffle Bag")))
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "Number of Bags")
               (:input :type "number" :name "bag_count"))
             (:div :class "form-group"
               (:label "Total Bag Weight (lbs)")
               (:input :type "number" :name "bag_weight_lbs")))
           (:hr)
           (:h3 "Current Location")
           (:div :class "form-row"
             (:div :class "form-group"
               (:label "Camp/Base")
               (:select :name "current_camp_id" :required t
                 (:option :value "" "-- Select Location --")
                 (dolist (c camps)
                   (cl-who:htm 
                    (:option :value (getf c :|id|)
                             (cl-who:str (format nil "~A - ~A" 
                                                 (getf c :|country_name|)
                                                 (getf c :|name|)))))))))
           (:div :class "form-group"
             (:label "Notes")
             (:textarea :name "notes" :rows "3"))
           (:div :class "form-actions"
             (:button :type "submit" :class "btn btn-primary" "Add Personnel")
             (:a :href "/perstat/personnel" :class "btn" "Cancel"))))))))

(defun handle-perstat-personnel-detail (id-str)
  "View personnel detail page."
  (let* ((id (parse-int id-str))
         (person (when id (get-personnel id))))
    (if person
        (html-response
         (render-page (format nil "Personnel - ~A" (getf person :|full_name|))
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 (cl-who:str (getf person :|full_name|)))
               (:div :class "header-actions"
                 (:a :href (format nil "/perstat/personnel/~A/edit" id) :class "btn btn-primary" "Edit")
                 (:a :href "/perstat/personnel" :class "btn" "Back to Roster")))
             (:section :class "card"
               (:h2 "Personnel Information")
               (:div :class "detail-grid"
                 (:div :class "detail-item"
                   (:label "Role Category")
                   (:span (cl-who:str (or (getf person :|role_category|) "-"))))
                 (:div :class "detail-item"
                   (:label "Status")
                   (:span (cl-who:str (or (getf person :|status|) "Active"))))
                 (:div :class "detail-item"
                   (:label "Nationality")
                   (:span (cl-who:str (or (getf person :|nationality|) "-"))))
                 (:div :class "detail-item"
                   (:label "Blood Type")
                   (:span (cl-who:str (or (getf person :|blood_type|) "-"))))))
             (:section :class "card"
               (:h2 "Current Location")
               (:div :class "detail-grid"
                 (:div :class "detail-item"
                   (:label "Country")
                   (:span (cl-who:str (or (getf person :|country_name|) "-"))))
                 (:div :class "detail-item"
                   (:label "Location")
                   (:span (cl-who:str (or (getf person :|camp_name|) "-")))))))))
        (progn
          (setf (hunchentoot:return-code*) 404)
          (html-response
           (render-page "Not Found"
             "<div class='empty-state'><h1>Personnel Not Found</h1><a href='/perstat/personnel' class='btn'>Back to Roster</a></div>"))))))

(defun handle-perstat-personnel-edit (id-str)
  "Edit personnel form."
  (let* ((id (parse-int id-str))
         (person (when id (get-personnel id)))
         (camps (fetch-all "SELECT c.id, c.name, co.name as country_name 
                           FROM camps c 
                           JOIN countries co ON c.country_id = co.id 
                           ORDER BY co.name, c.name")))
    (if person
        (html-response
         (render-page (format nil "Edit - ~A" (getf person :|full_name|))
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 "Edit Personnel"))
             (:form :method "post" :action (format nil "/api/perstat/personnel/~A/update" id) :class "form-card"
               (:div :class "form-row"
                 (:div :class "form-group required"
                   (:label "Full Name")
                   (:input :type "text" :name "full_name" :required t
                           :value (or (getf person :|full_name|) "")))
                 (:div :class "form-group required"
                   (:label "Role Category")
                   (:select :name "role_category" :required t
                     (:option :value "" "-- Select --")
                     (dolist (cat *personnel-categories*)
                       (let ((selected (string= cat (getf person :|role_category|))))
                         (cl-who:htm 
                          (:option :value cat :selected selected (cl-who:str cat))))))))
               (:div :class "form-row"
                 (:div :class "form-group"
                   (:label "Status")
                   (:select :name "status"
                     (:option :value "Active" :selected (string= "Active" (getf person :|status|)) "Active / Present")
                     (:option :value "R/R" :selected (string= "R/R" (getf person :|status|)) "R/R (Rest & Recuperation)")
                     (:option :value "Remote" :selected (string= "Remote" (getf person :|status|)) "Remote (Working from HOR)")
                     (:option :value "Leave" :selected (string= "Leave" (getf person :|status|)) "Leave")
                     (:option :value "LWOP" :selected (string= "LWOP" (getf person :|status|)) "LWOP (Leave Without Pay)")
                     (:option :value "Medical" :selected (string= "Medical" (getf person :|status|)) "Medical Leave")
                     (:option :value "Departed" :selected (string= "Departed" (getf person :|status|)) "Departed")))
                 (:div :class "form-group"
                   (:label "Blood Type")
                   (:select :name "blood_type"
                     (:option :value "" "-- Select --")
                     (dolist (bt *blood-types*)
                       (let ((selected (string= bt (getf person :|blood_type|))))
                         (cl-who:htm 
                          (:option :value bt :selected selected (cl-who:str bt))))))))
               (:div :class "form-row"
                 (:div :class "form-group"
                   (:label "Nationality")
                   (:input :type "text" :name "nationality" 
                           :value (or (getf person :|nationality|) "")
                           :placeholder "e.g., UK, US, OCN"))
                 (:div :class "form-group"
                   (:label "Body Weight (lbs)")
                   (:input :type "number" :name "body_weight_lbs"
                           :value (or (getf person :|body_weight_lbs|) ""))))
               (:hr)
               (:h3 "Current Location")
               (:div :class "form-group"
                 (:label "Camp/Base")
                 (:select :name "current_camp_id" :required t
                   (:option :value "" "-- Select Location --")
                   (dolist (c camps)
                     (let ((selected (and (getf person :|current_camp_id|)
                                         (= (getf c :|id|) (getf person :|current_camp_id|)))))
                       (cl-who:htm 
                        (:option :value (getf c :|id|) :selected selected
                                 (cl-who:str (format nil "~A - ~A" 
                                                     (getf c :|country_name|)
                                                     (getf c :|name|)))))))))
               (:div :class "form-group"
                 (:label "Notes")
                 (:textarea :name "notes" :rows "3" (cl-who:str (or (getf person :|notes|) ""))))
               (:div :class "form-actions"
                 (:button :type "submit" :class "btn btn-primary" "Save Changes")
                 (:a :href "/perstat/personnel" :class "btn" "Cancel"))))))
        (progn
          (setf (hunchentoot:return-code*) 404)
          (html-response
           (render-page "Not Found"
             "<div class='empty-state'><h1>Personnel Not Found</h1><a href='/perstat/personnel' class='btn'>Back to Roster</a></div>"))))))

;;; API Handlers

(defun handle-api-perstat-personnel-update (id-str)
  "Update a personnel record."
  (let* ((id (parse-int id-str))
         (full-name (get-param "full_name"))
         (role-category (get-param "role_category"))
         (status (get-param "status"))
         (nationality (get-param "nationality"))
         (blood-type (get-param "blood_type"))
         (body-weight (parse-int (get-param "body_weight_lbs")))
         (camp-id (parse-int (get-param "current_camp_id")))
         (location (get-param "current_location"))
         (notes (get-param "notes")))
    (when id
      (update-personnel id
                        :full-name full-name
                        :role-category role-category
                        :status status
                        :nationality nationality
                        :blood-type blood-type
                        :body-weight-lbs body-weight
                        :current-camp-id camp-id
                        :current-location location
                        :notes notes))
    (redirect-to "/perstat/personnel")))

(defun handle-api-perstat-personnel-create ()
  "Create a new personnel record."
  (let ((full-name (get-param "full_name"))
        (role-category (get-param "role_category"))
        (nationality (get-param "nationality"))
        (blood-type (get-param "blood_type"))
        (body-weight (parse-int (get-param "body_weight_lbs")))
        (passport (get-param "passport_number"))
        (dodi (get-param "dodi_number"))
        (bag-types (get-param "bag_types"))
        (bag-count (parse-int (get-param "bag_count")))
        (bag-weight (parse-int (get-param "bag_weight_lbs")))
        (camp-id (parse-int (get-param "current_camp_id")))
        (location (get-param "current_location"))
        (notes (get-param "notes")))
    (create-personnel :full-name full-name
                      :role-category role-category
                      :nationality nationality
                      :blood-type blood-type
                      :body-weight-lbs body-weight
                      :passport-number passport
                      :dodi-number dodi
                      :bag-types bag-types
                      :bag-count bag-count
                      :bag-weight-lbs bag-weight
                      :current-camp-id camp-id
                      :current-location location
                      :notes notes)
    (redirect-to "/perstat/personnel")))

;;; Report Generation

(defvar *perstat-script-path*
  (merge-pathnames "scripts/generate_perstat.py" (get-app-directory))
  "Path to PERSTAT generation script.")

(defvar *perstat-reports-directory*
  (merge-pathnames "data/perstat/" (get-app-directory))
  "Directory for generated PERSTAT reports.")

(defun ensure-perstat-reports-directory ()
  "Ensure PERSTAT reports directory exists."
  (ensure-directories-exist *perstat-reports-directory*))

(defun get-python-path ()
  "Get the Python interpreter path - checks for venv or uses system python3."
  (let ((venv-python (merge-pathnames ".venv/bin/python3" (get-app-directory))))
    (if (probe-file venv-python)
        (namestring venv-python)
        "python3")))

(defun generate-perstat-report (format)
  "Generate a PERSTAT report in the specified format (pdf or excel).
   Returns the path to the generated file."
  (ensure-perstat-reports-directory)
  (let* ((timestamp (multiple-value-bind (sec min hour day month year)
                        (decode-universal-time (get-universal-time))
                      (declare (ignore sec min hour))
                      (format nil "~4,'0D~2,'0D~2,'0D" year month day)))
         (extension (if (string= format "pdf") "pdf" "xlsx"))
         (filename (format nil "PERSTAT_~A.~A" timestamp extension))
         (output-path (merge-pathnames filename *perstat-reports-directory*))
         (python-bin (get-python-path)))
    (let ((result (uiop:run-program 
                   (list (namestring python-bin)
                         (namestring *perstat-script-path*)
                         "--format" format
                         "--output" (namestring output-path))
                   :output :string
                   :error-output :string
                   :ignore-error-status t)))
      (declare (ignore result))
      (when (probe-file output-path)
        output-path))))

(defun handle-perstat-report-page ()
  "Page to generate PERSTAT reports."
  (html-response
   (render-page "Generate PERSTAT Report"
     (cl-who:with-html-output-to-string (s)
       (:div :class "page-header"
         (:h1 "Generate PERSTAT Report"))
       (:section :class "card"
         (:h2 "Report Options")
         (:p "Generate a Personnel Status (PERSTAT) report in your preferred format.")
         (:div :class "form-row" :style "gap: 1rem; margin-top: 1rem;"
           (:a :href "/perstat/report/generate?format=pdf" :class "btn btn-primary"
               "📄 Download PDF (Internal DSR)")
           (:a :href "/perstat/report/generate?format=excel" :class "btn btn-primary"
               "📊 Download Excel (USACE OCONUS)")))
       (:section :class "card"
         (:h2 "Report Formats")
         (:div :class "form-row"
           (:div :class "form-group"
             (:h3 "Internal PERSTAT-DSR (PDF)")
             (:p "Detailed daily status report with individual personnel records.")
             (:ul
               (:li "Name, location, status")
               (:li "Position and nationality")
               (:li "Master/Journeyman designation")
               (:li "Team assignment")
               (:li "Deploy dates")))
           (:div :class "form-group"
             (:h3 "USACE OCONUS PERSTAT (Excel)")
             (:p "Summary format for USACE reporting.")
             (:ul
               (:li "Personnel counts by location")
               (:li "Breakdown by role category")
               (:li "Detailed roster sheet")))))))))

(defun handle-perstat-report-generate ()
  "Generate and download a PERSTAT report."
  (let* ((format (or (get-param "format") "pdf"))
         (report-path (generate-perstat-report format)))
    (if report-path
        (progn
          (setf (hunchentoot:content-type*) 
                (if (string= format "pdf") 
                    "application/pdf" 
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
          (setf (hunchentoot:header-out :content-disposition)
                (format nil "attachment; filename=\"~A\"" (file-namestring report-path)))
          (with-open-file (stream report-path :element-type '(unsigned-byte 8))
            (let ((buffer (make-array (file-length stream) :element-type '(unsigned-byte 8))))
              (read-sequence buffer stream)
              buffer)))
        (progn
          (setf (hunchentoot:return-code*) 500)
          "Error generating report"))))
