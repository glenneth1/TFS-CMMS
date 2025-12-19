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
  
  ;; Inventory by site (on-hand quantities)
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
  
  ;; Inventory transactions
  (execute-sql "
    CREATE TABLE IF NOT EXISTS inventory_transactions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      stock_item_id INTEGER NOT NULL,
      site_id INTEGER NOT NULL,
      wo_id INTEGER,
      transaction_type TEXT NOT NULL,
      qty INTEGER NOT NULL,
      notes TEXT,
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY (stock_item_id) REFERENCES stock_items(id),
      FOREIGN KEY (site_id) REFERENCES sites(id),
      FOREIGN KEY (wo_id) REFERENCES work_orders(id)
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
  
  ;; Create indexes for common queries
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_wo_site ON work_orders(site_id)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_wo_status ON work_orders(status)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_wo_created ON work_orders(created_at)")
  (execute-sql "CREATE INDEX IF NOT EXISTS idx_inv_trans_date ON inventory_transactions(created_at)")
  
  (format t "~&Database initialized at ~A~%" *database-path*)
  t)
