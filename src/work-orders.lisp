(in-package #:tfs-cmms)

;;; Work Order Number Generation

(defun next-wo-number (site-id)
  "Generate the next work order number for a site.
   Format: TFS-<SITE_CODE>-<YEAR>-<SEQUENCE>
   Example: TFS-AJ-2025-00001"
  (let* ((site (get-site site-id))
         (site-code (getf site :|code|))
         (year (local-time:format-timestring nil (local-time:now) :format '(:year)))
         (seq-row (fetch-one "SELECT last_number FROM wo_sequences WHERE site_id = ?" site-id))
         (next-num (1+ (or (getf seq-row :|last_number|) 0))))
    ;; Update the sequence
    (execute-sql "UPDATE wo_sequences SET last_number = ? WHERE site_id = ?" next-num site-id)
    ;; Return formatted WO number
    (format nil "~A-~A-~A-~5,'0D" *wo-prefix* site-code year next-num)))

;;; Work Order CRUD

(defun create-work-order (site-id work-type priority 
                          &key deficiency-id inspection-report-no bosi-ticket-no
                               task-order clin facility-id asset-id system-id
                               location-details code-basis deficiency-category
                               loto-required work-instructions target-start
                               target-completion assigned-to planned-hours job-plan-id)
  "Create a new work order with auto-generated WO number."
  (let ((wo-number (next-wo-number site-id)))
    (execute-sql 
     "INSERT INTO work_orders 
      (wo_number, deficiency_id, inspection_report_no, bosi_ticket_no, task_order, clin,
       site_id, facility_id, asset_id, system_id, location_details, work_type, priority,
       code_basis, deficiency_category, loto_required, work_instructions, target_start,
       target_completion, assigned_to, planned_hours, job_plan_id, status, progress_pct)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'New', 0)"
     wo-number deficiency-id inspection-report-no bosi-ticket-no task-order clin
     site-id facility-id asset-id system-id location-details work-type priority
     code-basis deficiency-category (if loto-required 1 0) work-instructions 
     target-start target-completion assigned-to planned-hours job-plan-id)
    (let ((wo-id (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))
      ;; Log creation in history
      (execute-sql "INSERT INTO wo_history (wo_id, field_changed, new_value) VALUES (?, 'created', ?)"
                   wo-id wo-number)
      (values wo-id wo-number))))

(defun get-work-order (id)
  "Get work order by ID with full details."
  (fetch-one 
   "SELECT wo.*, 
           s.code as site_code, s.name as site_name,
           f.code as facility_code, f.name as facility_name,
           a.name as asset_name,
           sys.name as system_name,
           jp.name as job_plan_name
    FROM work_orders wo
    JOIN sites s ON wo.site_id = s.id
    LEFT JOIN facilities f ON wo.facility_id = f.id
    LEFT JOIN assets a ON wo.asset_id = a.id
    LEFT JOIN systems sys ON wo.system_id = sys.id
    LEFT JOIN job_plans jp ON wo.job_plan_id = jp.id
    WHERE wo.id = ?" id))

(defun get-work-order-by-number (wo-number)
  "Get work order by WO number."
  (let ((wo (fetch-one "SELECT id FROM work_orders WHERE wo_number = ?" wo-number)))
    (when wo
      (get-work-order (getf wo :|id|)))))

(defun update-work-order (id &key status progress-pct blocker next-action
                                  actual-start actual-finish actual-hours
                                  assigned-to material-cost labor-cost
                                  changed-by)
  "Update work order fields and log changes to history."
  (let ((current (get-work-order id)))
    (when current
      ;; Track changes for audit
      (flet ((log-change (field old new)
               (when (and new (not (equal old new)))
                 (execute-sql "INSERT INTO wo_history (wo_id, field_changed, old_value, new_value, changed_by)
                               VALUES (?, ?, ?, ?, ?)"
                              id field (princ-to-string old) (princ-to-string new) changed-by))))
        
        (when status
          (log-change "status" (getf current :|status|) status))
        (when progress-pct
          (log-change "progress_pct" (getf current :|progress_pct|) progress-pct))
        (when blocker
          (log-change "blocker" (getf current :|blocker|) blocker))
        (when next-action
          (log-change "next_action" (getf current :|next_action|) next-action))
        (when assigned-to
          (log-change "assigned_to" (getf current :|assigned_to|) assigned-to)))
      
      ;; Build dynamic update
      (let ((updates '())
            (params '()))
        (macrolet ((add-update (field value)
                     `(when ,value
                        (push (format nil "~A = ?" ,field) updates)
                        (push ,value params))))
          (add-update "status" status)
          (add-update "progress_pct" progress-pct)
          (add-update "blocker" blocker)
          (add-update "next_action" next-action)
          (add-update "actual_start" actual-start)
          (add-update "actual_finish" actual-finish)
          (add-update "actual_hours" actual-hours)
          (add-update "assigned_to" assigned-to)
          (add-update "material_cost" material-cost)
          (add-update "labor_cost" labor-cost))
        
        (when updates
          (push "updated_at = datetime('now')" updates)
          (let ((sql (format nil "UPDATE work_orders SET ~{~A~^, ~} WHERE id = ?"
                             (nreverse updates))))
            (apply #'execute-sql sql (append (nreverse params) (list id)))))))))

(defun list-work-orders (&key site-id status priority date-from date-to limit offset)
  "List work orders with optional filters including date range."
  (let ((conditions (list "1=1"))
        (params (list)))
    (when site-id
      (setf conditions (append conditions (list "wo.site_id = ?")))
      (setf params (append params (list site-id))))
    (when status
      (setf conditions (append conditions (list "wo.status = ?")))
      (setf params (append params (list status))))
    (when priority
      (setf conditions (append conditions (list "wo.priority = ?")))
      (setf params (append params (list priority))))
    (when (and date-from (> (length date-from) 0))
      (setf conditions (append conditions (list "date(wo.created_at) >= ?")))
      (setf params (append params (list date-from))))
    (when (and date-to (> (length date-to) 0))
      (setf conditions (append conditions (list "date(wo.created_at) <= ?")))
      (setf params (append params (list date-to))))
    
    (let* ((sql (format nil 
                "SELECT wo.*, s.code as site_code, f.code as facility_code
                 FROM work_orders wo
                 JOIN sites s ON wo.site_id = s.id
                 LEFT JOIN facilities f ON wo.facility_id = f.id
                 WHERE ~{~A~^ AND ~}
                 ORDER BY wo.created_at DESC
                 ~@[LIMIT ~A~]~@[ OFFSET ~A~]"
                conditions limit offset))
           (query (dbi:prepare (connect-db) sql))
           (result (dbi:execute query params)))
      (dbi:fetch-all result))))

(defun get-wo-history (wo-id)
  "Get change history for a work order."
  (fetch-all "SELECT * FROM wo_history WHERE wo_id = ? ORDER BY changed_at DESC" wo-id))

;;; Work Order Statistics

(defun count-work-orders-by-status (site-id)
  "Get work order counts by status for a site."
  (fetch-all 
   "SELECT status, COUNT(*) as count 
    FROM work_orders 
    WHERE site_id = ? 
    GROUP BY status" site-id))

(defun count-open-work-orders (site-id)
  "Count open (non-closed) work orders for a site."
  (getf (fetch-one 
         "SELECT COUNT(*) as count FROM work_orders 
          WHERE site_id = ? AND status != 'Closed'" site-id)
        :|count|))

(defun get-aging-work-orders (site-id &key (days 30))
  "Get work orders older than specified days that are still open."
  (fetch-all 
   "SELECT wo.*, s.code as site_code, f.code as facility_code,
           julianday('now') - julianday(wo.created_at) as age_days
    FROM work_orders wo
    JOIN sites s ON wo.site_id = s.id
    LEFT JOIN facilities f ON wo.facility_id = f.id
    WHERE wo.site_id = ? 
      AND wo.status != 'Closed'
      AND julianday('now') - julianday(wo.created_at) > ?
    ORDER BY wo.created_at" site-id days))

(defun get-work-orders-opened-this-week (site-id)
  "Get work orders created in the last 7 days."
  (fetch-all 
   "SELECT wo.*, s.code as site_code
    FROM work_orders wo
    JOIN sites s ON wo.site_id = s.id
    WHERE wo.site_id = ? 
      AND wo.created_at >= datetime('now', '-7 days')
    ORDER BY wo.created_at DESC" site-id))

(defun get-work-orders-closed-this-week (site-id)
  "Get work orders closed in the last 7 days."
  (fetch-all 
   "SELECT wo.*, s.code as site_code
    FROM work_orders wo
    JOIN sites s ON wo.site_id = s.id
    WHERE wo.site_id = ? 
      AND wo.status = 'Closed'
      AND wo.updated_at >= datetime('now', '-7 days')
    ORDER BY wo.updated_at DESC" site-id))
