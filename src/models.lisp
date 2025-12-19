(in-package #:tfs-cmms)

;;; Site operations

(defun create-site (code name)
  "Create a new site and initialize its WO sequence."
  (execute-sql "INSERT INTO sites (code, name) VALUES (?, ?)" code name)
  (let ((site-id (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))
    (execute-sql "INSERT INTO wo_sequences (site_id, last_number) VALUES (?, 0)" site-id)
    site-id))

(defun get-site (id)
  "Get site by ID."
  (fetch-one "SELECT * FROM sites WHERE id = ?" id))

(defun get-site-by-code (code)
  "Get site by code."
  (fetch-one "SELECT * FROM sites WHERE code = ?" code))

(defun list-sites ()
  "List all sites."
  (fetch-all "SELECT * FROM sites ORDER BY code"))

;;; Facility operations

(defun create-facility (site-id code name)
  "Create a new facility under a site."
  (execute-sql "INSERT INTO facilities (site_id, code, name) VALUES (?, ?, ?)" 
               site-id code name)
  (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))

(defun list-facilities (&optional site-id)
  "List facilities, optionally filtered by site."
  (if site-id
      (fetch-all "SELECT f.*, s.code as site_code FROM facilities f 
                  JOIN sites s ON f.site_id = s.id 
                  WHERE f.site_id = ? ORDER BY f.code" site-id)
      (fetch-all "SELECT f.*, s.code as site_code FROM facilities f 
                  JOIN sites s ON f.site_id = s.id ORDER BY s.code, f.code")))

;;; System operations

(defun create-system (name)
  "Create a new system category."
  (execute-sql "INSERT INTO systems (name) VALUES (?)" name)
  (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))

(defun list-systems ()
  "List all systems."
  (fetch-all "SELECT * FROM systems ORDER BY name"))

;;; Asset operations

(defun create-asset (facility-id name &key system-id description manufacturer 
                                        model serial-number warranty-start 
                                        warranty-end specs)
  "Create a new asset."
  (execute-sql "INSERT INTO assets (facility_id, system_id, name, description, 
                manufacturer, model, serial_number, warranty_start, warranty_end, specs)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
               facility-id system-id name description manufacturer model 
               serial-number warranty-start warranty-end specs)
  (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))

(defun get-asset (id)
  "Get asset by ID with full hierarchy."
  (fetch-one "SELECT a.*, f.name as facility_name, f.code as facility_code,
                     s.name as site_name, s.code as site_code, sys.name as system_name
              FROM assets a
              JOIN facilities f ON a.facility_id = f.id
              JOIN sites s ON f.site_id = s.id
              LEFT JOIN systems sys ON a.system_id = sys.id
              WHERE a.id = ?" id))

(defun list-assets (&key facility-id site-id system-id)
  "List assets with optional filters."
  (cond
    (facility-id
     (fetch-all "SELECT a.*, sys.name as system_name FROM assets a
                 LEFT JOIN systems sys ON a.system_id = sys.id
                 WHERE a.facility_id = ? ORDER BY a.name" facility-id))
    (site-id
     (fetch-all "SELECT a.*, f.code as facility_code, sys.name as system_name 
                 FROM assets a
                 JOIN facilities f ON a.facility_id = f.id
                 LEFT JOIN systems sys ON a.system_id = sys.id
                 WHERE f.site_id = ? ORDER BY f.code, a.name" site-id))
    (system-id
     (fetch-all "SELECT a.*, f.code as facility_code, s.code as site_code
                 FROM assets a
                 JOIN facilities f ON a.facility_id = f.id
                 JOIN sites s ON f.site_id = s.id
                 WHERE a.system_id = ? ORDER BY s.code, f.code, a.name" system-id))
    (t
     (fetch-all "SELECT a.*, f.code as facility_code, s.code as site_code, sys.name as system_name
                 FROM assets a
                 JOIN facilities f ON a.facility_id = f.id
                 JOIN sites s ON f.site_id = s.id
                 LEFT JOIN systems sys ON a.system_id = sys.id
                 ORDER BY s.code, f.code, a.name"))))

;;; Job Plan operations

(defun create-job-plan (name &key description steps)
  "Create a new job plan template."
  (execute-sql "INSERT INTO job_plans (name, description, steps) VALUES (?, ?, ?)"
               name description steps)
  (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))

(defun get-job-plan (id)
  "Get job plan by ID."
  (fetch-one "SELECT * FROM job_plans WHERE id = ?" id))

(defun list-job-plans ()
  "List all job plans."
  (fetch-all "SELECT * FROM job_plans ORDER BY name"))

(defun add-job-plan-material (job-plan-id stock-item-id qty &key (substitute nil))
  "Add a material to a job plan."
  (execute-sql "INSERT INTO job_plan_materials (job_plan_id, stock_item_id, qty_required, is_substitute)
                VALUES (?, ?, ?, ?)"
               job-plan-id stock-item-id qty (if substitute 1 0)))
