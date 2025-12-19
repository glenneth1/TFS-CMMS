(in-package #:tfs-cmms)

;;; Stock Item operations

(defun create-stock-item (part-number description &key uom min-qty ideal-qty manufacturer)
  "Create a new stock item in the catalog."
  (execute-sql 
   "INSERT INTO stock_items (part_number, description, uom, min_qty, ideal_qty, manufacturer)
    VALUES (?, ?, ?, ?, ?, ?)"
   part-number description (or uom "EA") (or min-qty 0) (or ideal-qty 0) manufacturer)
  (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))

(defun get-stock-item (id)
  "Get stock item by ID."
  (fetch-one "SELECT * FROM stock_items WHERE id = ?" id))

(defun get-stock-item-by-part-number (part-number)
  "Get stock item by part number."
  (fetch-one "SELECT * FROM stock_items WHERE part_number = ?" part-number))

(defun list-stock-items ()
  "List all stock items."
  (fetch-all "SELECT * FROM stock_items ORDER BY part_number"))

;;; Inventory operations

(defun get-inventory (stock-item-id site-id)
  "Get current inventory level for an item at a site."
  (fetch-one 
   "SELECT i.*, si.part_number, si.description, si.uom, si.min_qty, si.ideal_qty
    FROM inventory i
    JOIN stock_items si ON i.stock_item_id = si.id
    WHERE i.stock_item_id = ? AND i.site_id = ?" stock-item-id site-id))

(defun ensure-inventory-record (stock-item-id site-id)
  "Ensure an inventory record exists for item at site."
  (unless (get-inventory stock-item-id site-id)
    (execute-sql "INSERT INTO inventory (stock_item_id, site_id, qty_on_hand) VALUES (?, ?, 0)"
                 stock-item-id site-id)))

(defun record-transaction (stock-item-id site-id transaction-type qty &key wo-id notes)
  "Record an inventory transaction and update on-hand quantity.
   Transaction types: Issue, Receipt, Transfer, Adjustment"
  (ensure-inventory-record stock-item-id site-id)
  
  ;; Record the transaction
  (execute-sql 
   "INSERT INTO inventory_transactions (stock_item_id, site_id, wo_id, transaction_type, qty, notes)
    VALUES (?, ?, ?, ?, ?, ?)"
   stock-item-id site-id wo-id transaction-type qty notes)
  
  ;; Update on-hand quantity
  (let ((delta (case (intern (string-upcase transaction-type) :keyword)
                 (:issue (- qty))
                 (:receipt qty)
                 (:adjustment qty)
                 (t 0))))
    (execute-sql 
     "UPDATE inventory SET qty_on_hand = qty_on_hand + ? 
      WHERE stock_item_id = ? AND site_id = ?"
     delta stock-item-id site-id))
  
  (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))

(defun get-inventory-summary (site-id)
  "Get inventory summary for a site with reorder flags."
  (fetch-all 
   "SELECT si.part_number, si.description, si.uom, si.min_qty, si.ideal_qty,
           COALESCE(i.qty_on_hand, 0) as qty_on_hand,
           CASE WHEN COALESCE(i.qty_on_hand, 0) <= si.min_qty THEN 1 ELSE 0 END as reorder_flag
    FROM stock_items si
    LEFT JOIN inventory i ON si.id = i.stock_item_id AND i.site_id = ?
    ORDER BY si.part_number" site-id))

(defun get-low-stock-items (site-id)
  "Get items at or below minimum quantity for a site."
  (fetch-all 
   "SELECT si.*, i.qty_on_hand
    FROM stock_items si
    JOIN inventory i ON si.id = i.stock_item_id
    WHERE i.site_id = ? AND i.qty_on_hand <= si.min_qty
    ORDER BY si.part_number" site-id))

;;; Burn Rate calculations

(defun calculate-burn-rate (stock-item-id site-id &key (days 30))
  "Calculate burn rate (issues per day) for an item at a site over specified days."
  (let ((result (fetch-one 
                 "SELECT COALESCE(SUM(qty), 0) as total_issued,
                         COUNT(*) as transaction_count
                  FROM inventory_transactions
                  WHERE stock_item_id = ? 
                    AND site_id = ?
                    AND transaction_type = 'Issue'
                    AND created_at >= datetime('now', '-' || ? || ' days')"
                 stock-item-id site-id days)))
    (let ((total (getf result :|total_issued|)))
      (if (and total (> total 0))
          (/ (float total) days)
          0.0))))

(defun get-burn-rate-summary (site-id &key (days 30))
  "Get burn rate summary for all items at a site."
  (fetch-all 
   "SELECT si.part_number, si.description, si.uom,
           COALESCE(i.qty_on_hand, 0) as qty_on_hand,
           COALESCE(SUM(CASE WHEN it.transaction_type = 'Issue' THEN it.qty ELSE 0 END), 0) as issued_period,
           ROUND(COALESCE(SUM(CASE WHEN it.transaction_type = 'Issue' THEN it.qty ELSE 0 END), 0) * 1.0 / ?, 2) as daily_burn_rate,
           ROUND(COALESCE(SUM(CASE WHEN it.transaction_type = 'Issue' THEN it.qty ELSE 0 END), 0) * 7.0 / ?, 2) as weekly_burn_rate
    FROM stock_items si
    LEFT JOIN inventory i ON si.id = i.stock_item_id AND i.site_id = ?
    LEFT JOIN inventory_transactions it ON si.id = it.stock_item_id 
         AND it.site_id = ? 
         AND it.created_at >= datetime('now', '-' || ? || ' days')
    GROUP BY si.id
    HAVING issued_period > 0
    ORDER BY weekly_burn_rate DESC"
   days days site-id site-id days))

(defun get-issued-this-week (site-id)
  "Get items issued in the last 7 days for a site."
  (fetch-all 
   "SELECT si.part_number, si.description, si.uom,
           SUM(it.qty) as qty_issued,
           COUNT(*) as transaction_count
    FROM inventory_transactions it
    JOIN stock_items si ON it.stock_item_id = si.id
    WHERE it.site_id = ?
      AND it.transaction_type = 'Issue'
      AND it.created_at >= datetime('now', '-7 days')
    GROUP BY si.id
    ORDER BY qty_issued DESC" site-id))

(defun get-transactions-for-wo (wo-id)
  "Get all inventory transactions for a work order."
  (fetch-all 
   "SELECT it.*, si.part_number, si.description, si.uom
    FROM inventory_transactions it
    JOIN stock_items si ON it.stock_item_id = si.id
    WHERE it.wo_id = ?
    ORDER BY it.created_at" wo-id))
