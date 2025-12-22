(in-package #:tfs-cmms)

(defvar *db* nil
  "Database connection handle.")

(defun connect-db ()
  "Connect to the SQLite database."
  (unless *db*
    (ensure-directories-exist *database-path*)
    (setf *db* (dbi:connect :sqlite3 :database-name (namestring *database-path*))))
  *db*)

(defun disconnect-db ()
  "Disconnect from the database."
  (when *db*
    (dbi:disconnect *db*)
    (setf *db* nil)))

(defun execute-sql (sql &rest params)
  "Execute SQL with parameters."
  (let ((query (dbi:prepare (connect-db) sql)))
    (dbi:execute query params)))

(defun fetch-all (sql &rest params)
  "Execute SQL and fetch all results as alists."
  (let* ((query (dbi:prepare (connect-db) sql))
         (result (dbi:execute query params)))
    (dbi:fetch-all result)))

(defun fetch-one (sql &rest params)
  "Execute SQL and fetch first result."
  (let* ((query (dbi:prepare (connect-db) sql))
         (result (dbi:execute query params)))
    (dbi:fetch result)))

(defun init-database ()
  "Initialize the database schema."
  (connect-db)
  
  ;; System settings table (for configurable values like contract number)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS system_settings (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      setting_key TEXT UNIQUE NOT NULL,
      setting_value TEXT,
      description TEXT,
      updated_at TEXT DEFAULT (datetime('now'))
    )")
  
  ;; Sites table
  (execute-sql "
    CREATE TABLE IF NOT EXISTS sites (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      code TEXT UNIQUE NOT NULL,
      name TEXT NOT NULL,
      created_at TEXT DEFAULT (datetime('now'))
    )")
  
  ;; Facilities table
  (execute-sql "
    CREATE TABLE IF NOT EXISTS facilities (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      site_id INTEGER NOT NULL,
      code TEXT NOT NULL,
      name TEXT NOT NULL,
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (site_id) REFERENCES sites(id),
      UNIQUE(site_id, code)
    )")
  
  ;; Systems table (parent systems like Electrical, HVAC, etc.)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS systems (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL
    )")
  
  ;; Assets table
  (execute-sql "
    CREATE TABLE IF NOT EXISTS assets (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      facility_id INTEGER NOT NULL,
      system_id INTEGER,
      name TEXT NOT NULL,
      description TEXT,
      manufacturer TEXT,
      model TEXT,
      serial_number TEXT,
      warranty_start TEXT,
      warranty_end TEXT,
      specs TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (facility_id) REFERENCES facilities(id),
      FOREIGN KEY (system_id) REFERENCES systems(id)
    )")
  
  ;; WO Number sequence table (for auto-increment per site)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS wo_sequences (
      site_id INTEGER PRIMARY KEY,
      last_number INTEGER DEFAULT 0,
      FOREIGN KEY (site_id) REFERENCES sites(id)
    )")
  
  ;; Work Orders table
  (execute-sql "
    CREATE TABLE IF NOT EXISTS work_orders (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      wo_number TEXT UNIQUE NOT NULL,
      deficiency_id TEXT,
      inspection_report_no TEXT,
      bosi_ticket_no TEXT,
      task_order TEXT,
      clin TEXT,
      site_id INTEGER NOT NULL,
      facility_id INTEGER,
      asset_id INTEGER,
      system_id INTEGER,
      location_details TEXT,
      work_type TEXT NOT NULL,
      priority TEXT NOT NULL,
      code_basis TEXT,
      deficiency_category TEXT,
      loto_required INTEGER DEFAULT 0,
      status TEXT DEFAULT 'New',
      progress_pct INTEGER DEFAULT 0,
      blocker TEXT,
      next_action TEXT,
      work_instructions TEXT,
      target_start TEXT,
      target_completion TEXT,
      actual_start TEXT,
      actual_finish TEXT,
      assigned_to TEXT,
      planned_hours REAL,
      actual_hours REAL,
      material_cost REAL,
      labor_cost REAL,
      job_plan_id INTEGER,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (site_id) REFERENCES sites(id),
      FOREIGN KEY (facility_id) REFERENCES facilities(id),
      FOREIGN KEY (asset_id) REFERENCES assets(id),
      FOREIGN KEY (system_id) REFERENCES systems(id),
      FOREIGN KEY (job_plan_id) REFERENCES job_plans(id)
    )")
  
  ;; Work Order status history (audit trail)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS wo_history (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      wo_id INTEGER NOT NULL,
      field_changed TEXT NOT NULL,
      old_value TEXT,
      new_value TEXT,
      changed_by TEXT,
      changed_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (wo_id) REFERENCES work_orders(id)
    )")
  
  ;; Work Order activity log (re-inspections, call-outs, etc.)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS wo_activities (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      wo_id INTEGER NOT NULL,
      activity_type TEXT NOT NULL,
      activity_date TEXT NOT NULL,
      description TEXT,
      performed_by TEXT,
      notes TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (wo_id) REFERENCES work_orders(id)
    )")
  
  ;; Users and Authentication
  (execute-sql "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      full_name TEXT NOT NULL,
      email TEXT,
      role TEXT NOT NULL DEFAULT 'Inspector',
      electrician_type TEXT,
      team_number TEXT,
      hire_date TEXT,
      bog_date TEXT,
      current_location TEXT,
      staff_category TEXT,
      active INTEGER DEFAULT 1,
      created_at TEXT DEFAULT (datetime('now')),
      last_login TEXT
    )")
  
  ;; Contract Periods (for R&R tracking)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS contract_periods (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      start_date TEXT NOT NULL,
      end_date TEXT NOT NULL,
      probation_days INTEGER DEFAULT 90,
      max_rr_days INTEGER DEFAULT 17,
      accrual_days_per_year INTEGER DEFAULT 30,
      active INTEGER DEFAULT 1
    )")
  
  ;; R&R Requests
  (execute-sql "
    CREATE TABLE IF NOT EXISTS rr_requests (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      contract_period_id INTEGER,
      start_date TEXT NOT NULL,
      end_date TEXT NOT NULL,
      total_days INTEGER NOT NULL,
      travel_to TEXT,
      travel_from TEXT,
      status TEXT DEFAULT 'Pending',
      requested_at TEXT DEFAULT (datetime('now')),
      reviewed_by INTEGER,
      reviewed_at TEXT,
      review_comments TEXT,
      FOREIGN KEY (user_id) REFERENCES users(id),
      FOREIGN KEY (contract_period_id) REFERENCES contract_periods(id),
      FOREIGN KEY (reviewed_by) REFERENCES users(id)
    )")
  
  ;; Staff Category Limits (max concurrent leave per category)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS staff_category_limits (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      category TEXT UNIQUE NOT NULL,
      max_concurrent_leave INTEGER NOT NULL,
      approver_role TEXT NOT NULL
    )")

  ;; Sessions
  (execute-sql "
    CREATE TABLE IF NOT EXISTS sessions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      session_token TEXT UNIQUE NOT NULL,
      expires_at TEXT NOT NULL,
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (user_id) REFERENCES users(id)
    )")

  ;; Notifications
  (execute-sql "
    CREATE TABLE IF NOT EXISTS notifications (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER,
      role_target TEXT,
      title TEXT NOT NULL,
      message TEXT NOT NULL,
      link TEXT,
      read INTEGER DEFAULT 0,
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (user_id) REFERENCES users(id)
    )")

  ;; Inspection Reports
  (execute-sql "
    CREATE TABLE IF NOT EXISTS inspection_reports (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      wo_id INTEGER NOT NULL,
      report_number TEXT NOT NULL UNIQUE,
      tag_id TEXT NOT NULL,
      site_id INTEGER NOT NULL,
      building_number TEXT NOT NULL,
      building_type TEXT,
      system_voltage TEXT NOT NULL,
      inspection_phase TEXT NOT NULL,
      previous_report_id INTEGER,
      location_description TEXT,
      building_image_path TEXT,
      overall_rating TEXT,
      summary_of_findings TEXT,
      mrf_needed TEXT DEFAULT 'No',
      inspector1_name TEXT,
      inspector1_signed_at TEXT,
      inspector2_name TEXT,
      inspector2_signed_at TEXT,
      qc_name TEXT,
      qc_signed_at TEXT,
      qc_comments TEXT,
      status TEXT DEFAULT 'Draft',
      team_number TEXT,
      inspection_date TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (wo_id) REFERENCES work_orders(id),
      FOREIGN KEY (site_id) REFERENCES sites(id),
      FOREIGN KEY (previous_report_id) REFERENCES inspection_reports(id)
    )")

  ;; Deficiencies (linked to inspection reports)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS deficiencies (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      report_id INTEGER NOT NULL,
      deficiency_number INTEGER NOT NULL,
      location_description TEXT,
      num_occurrences INTEGER DEFAULT 1,
      deficiency_category TEXT,
      equipment_category TEXT,
      imminent_danger TEXT DEFAULT 'No',
      action_taken TEXT,
      sor_issued_to TEXT,
      description TEXT,
      code_source TEXT,
      code_reference TEXT,
      deficiency_status TEXT DEFAULT 'Open',
      image_path TEXT,
      rac_score INTEGER,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (report_id) REFERENCES inspection_reports(id)
    )")

  ;; Stock items (inventory catalog)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS stock_items (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      part_number TEXT UNIQUE NOT NULL,
      description TEXT NOT NULL,
      uom TEXT DEFAULT 'EA',
      min_qty INTEGER DEFAULT 0,
      ideal_qty INTEGER DEFAULT 0,
      manufacturer TEXT,
      created_at TEXT DEFAULT (datetime('now'))
    )")
  
  ;; Legacy inventory by site - kept for backward compatibility with work orders
  ;; New EBOM inventory uses inventory_items table
  (execute-sql "
    CREATE TABLE IF NOT EXISTS inventory (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      stock_item_id INTEGER NOT NULL,
      site_id INTEGER NOT NULL,
      qty_on_hand INTEGER DEFAULT 0,
      FOREIGN KEY (stock_item_id) REFERENCES stock_items(id),
      FOREIGN KEY (site_id) REFERENCES sites(id),
      UNIQUE(stock_item_id, site_id)
    )")
  
  ;; Job plans (templates)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS job_plans (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      description TEXT,
      steps TEXT,
      created_at TEXT DEFAULT (datetime('now'))
    )")
  
  ;; Job plan materials (default BOM)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS job_plan_materials (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      job_plan_id INTEGER NOT NULL,
      stock_item_id INTEGER NOT NULL,
      qty_required INTEGER DEFAULT 1,
      is_substitute INTEGER DEFAULT 0,
      FOREIGN KEY (job_plan_id) REFERENCES job_plans(id),
      FOREIGN KEY (stock_item_id) REFERENCES stock_items(id)
    )")
  
  ;; WO attachments
  (execute-sql "
    CREATE TABLE IF NOT EXISTS wo_attachments (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      wo_id INTEGER NOT NULL,
      filename TEXT NOT NULL,
      filepath TEXT NOT NULL,
      attachment_type TEXT,
      uploaded_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (wo_id) REFERENCES work_orders(id)
    )")
  
  ;; Countries table (for master tracker)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS countries (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      code TEXT UNIQUE
    )")

  ;; Camps table (linked to countries and sites)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS camps (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      country_id INTEGER NOT NULL,
      site_id INTEGER,
      name TEXT NOT NULL,
      code TEXT,
      FOREIGN KEY (country_id) REFERENCES countries(id),
      FOREIGN KEY (site_id) REFERENCES sites(id),
      UNIQUE(country_id, name)
    )")

  ;; Master tracker deficiencies (imported from QA_Master.xlsx)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS master_deficiencies (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      camp_id INTEGER NOT NULL,
      building_number TEXT,
      deficiency_number TEXT,
      def_category TEXT,
      equipment TEXT,
      inspection_phase TEXT,
      repair_by TEXT,
      occurrences INTEGER DEFAULT 0,
      lhs_imminent TEXT DEFAULT 'No',
      om_sor_number TEXT,
      repair_team_number TEXT,
      parts_ordered TEXT,
      date_ordered TEXT,
      deficiency_status TEXT,
      inspection_date TEXT,
      rac INTEGER,
      is_test INTEGER DEFAULT 0,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (camp_id) REFERENCES camps(id)
    )")

  ;; Weekly report snapshots (for historical tracking)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS weekly_reports (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      week_number INTEGER NOT NULL,
      year INTEGER NOT NULL,
      week_start TEXT NOT NULL,
      week_end TEXT NOT NULL,
      report_data TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      UNIQUE(year, week_number)
    )")

  ;; Inventory locations (containers, crates, yards)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS inventory_locations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      location_code TEXT NOT NULL UNIQUE,
      location_name TEXT,
      location_type TEXT DEFAULT 'Container',
      site_id INTEGER,
      notes TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (site_id) REFERENCES sites(id)
    )")

  ;; Inventory items (EBOM - Electrical Bill of Materials)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS inventory_items (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      item_number INTEGER NOT NULL,
      description TEXT NOT NULL,
      make TEXT,
      part_number TEXT,
      uom TEXT NOT NULL,
      quantity REAL DEFAULT 0,
      unit_cost REAL DEFAULT 0,
      total_cost REAL DEFAULT 0,
      location_id INTEGER,
      property_type TEXT DEFAULT 'Material',
      property_usage TEXT DEFAULT 'Consume',
      min_stock_level REAL DEFAULT 0,
      reorder_point REAL DEFAULT 0,
      last_audit_date TEXT,
      last_audit_quantity REAL,
      notes TEXT,
      active INTEGER DEFAULT 1,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (location_id) REFERENCES inventory_locations(id)
    )")

  ;; Inventory audits (100% annual, 10% monthly)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS inventory_audits (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      audit_type TEXT NOT NULL,
      audit_date TEXT NOT NULL,
      audit_start_date TEXT,
      audit_end_date TEXT,
      audited_by TEXT,
      total_items INTEGER DEFAULT 0,
      items_audited INTEGER DEFAULT 0,
      percentage REAL DEFAULT 0,
      total_value REAL DEFAULT 0,
      status TEXT DEFAULT 'In Progress',
      notes TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now'))
    )")

  ;; Inventory audit line items
  (execute-sql "
    CREATE TABLE IF NOT EXISTS inventory_audit_items (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      audit_id INTEGER NOT NULL,
      item_id INTEGER NOT NULL,
      expected_quantity REAL DEFAULT 0,
      actual_quantity REAL DEFAULT 0,
      variance REAL DEFAULT 0,
      location_verified INTEGER DEFAULT 0,
      notes TEXT,
      audited_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (audit_id) REFERENCES inventory_audits(id),
      FOREIGN KEY (item_id) REFERENCES inventory_items(id)
    )")

  ;; Inventory transactions (issues, receipts, adjustments, transfers)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS inventory_transactions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      item_id INTEGER NOT NULL,
      transaction_type TEXT NOT NULL,
      quantity REAL NOT NULL,
      quantity_before REAL,
      quantity_after REAL,
      unit_cost REAL,
      total_cost REAL,
      reference_type TEXT,
      reference_id INTEGER,
      from_location_id INTEGER,
      to_location_id INTEGER,
      performed_by TEXT,
      transaction_date TEXT DEFAULT (datetime('now')),
      notes TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (item_id) REFERENCES inventory_items(id),
      FOREIGN KEY (from_location_id) REFERENCES inventory_locations(id),
      FOREIGN KEY (to_location_id) REFERENCES inventory_locations(id)
    )")

  ;; Material Request Forms (MRF)
  (execute-sql "
    CREATE TABLE IF NOT EXISTS material_requests (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      mrf_number TEXT NOT NULL UNIQUE,
      inspection_report_id INTEGER,
      request_date TEXT NOT NULL,
      team_number TEXT,
      base TEXT,
      camp_name TEXT,
      building_number TEXT,
      requestor_name TEXT,
      requestor_id INTEGER,
      contract_number TEXT DEFAULT 'W912DY24R0043',
      site_support_location TEXT,
      status TEXT DEFAULT 'Draft',
      approved_by TEXT,
      approved_at TEXT,
      issued_by TEXT,
      issued_at TEXT,
      notes TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (requestor_id) REFERENCES users(id),
      FOREIGN KEY (inspection_report_id) REFERENCES inspection_reports(id)
    )")

  ;; MRF line items
  (execute-sql "
    CREATE TABLE IF NOT EXISTS material_request_items (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      mrf_id INTEGER NOT NULL,
      line_number INTEGER NOT NULL,
      item_id INTEGER,
      part_number TEXT,
      description TEXT,
      quantity_requested REAL NOT NULL,
      quantity_issued REAL DEFAULT 0,
      uom TEXT,
      is_material INTEGER DEFAULT 0,
      is_tool INTEGER DEFAULT 0,
      remarks TEXT,
      web_link TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (mrf_id) REFERENCES material_requests(id),
      FOREIGN KEY (item_id) REFERENCES inventory_items(id)
    )")

  ;; Create indexes for common queries
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_wo_site ON work_orders(site_id)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_wo_status ON work_orders(status)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_wo_created ON work_orders(created_at)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_trans_date ON inventory_transactions(created_at)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_master_def_camp ON master_deficiencies(camp_id)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_master_def_date ON master_deficiencies(inspection_date)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_master_def_status ON master_deficiencies(deficiency_status)")
  
  ;; Inventory indexes
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_item_number ON inventory_items(item_number)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_part_number ON inventory_items(part_number)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_location ON inventory_items(location_id)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_property_type ON inventory_items(property_type)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_trans_item ON inventory_transactions(item_id)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_trans_type ON inventory_transactions(transaction_type)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_audit_date ON inventory_audits(audit_date)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_mrf_status ON material_requests(status)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_mrf_requestor ON material_requests(requestor_id)")
  
  ;; Seed default system settings
  (execute-sql "INSERT OR IGNORE INTO system_settings (setting_key, setting_value, description) 
                VALUES ('contract_number', 'W912DY24R0043', 'Current contract number for MRF forms')")
  
  (format t "~&Database initialized at ~A~%" *database-path*)
  t)

;;; System Settings Functions
(defun get-system-setting (key)
  "Get a system setting value by key."
  (let ((result (fetch-one "SELECT setting_value FROM system_settings WHERE setting_key = ?" key)))
    (when result (getf result :|setting_value|))))

(defun set-system-setting (key value &optional description)
  "Set a system setting value."
  (execute-sql 
   "INSERT INTO system_settings (setting_key, setting_value, description, updated_at) 
    VALUES (?, ?, ?, datetime('now'))
    ON CONFLICT(setting_key) DO UPDATE SET setting_value = ?, updated_at = datetime('now')"
   key value description value))

(defun get-all-system-settings ()
  "Get all system settings."
  (fetch-all "SELECT * FROM system_settings ORDER BY setting_key"))
