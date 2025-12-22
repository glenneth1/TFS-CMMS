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

;;; ============================================================
;;; EBOM Inventory Functions (new inventory_items table)
;;; ============================================================

(defun get-inventory-items (&key location-id property-type search limit offset)
  "Get inventory items with optional filters."
  (let* ((conditions nil)
         (params nil)
         (sql "SELECT i.*, l.location_code, l.location_name
               FROM inventory_items i
               LEFT JOIN inventory_locations l ON i.location_id = l.id
               WHERE i.active = 1"))
    (when location-id
      (push "i.location_id = ?" conditions)
      (push location-id params))
    (when property-type
      (push "i.property_type = ?" conditions)
      (push property-type params))
    (when search
      (push "(i.description LIKE ? OR i.part_number LIKE ? OR i.make LIKE ?)" conditions)
      (let ((search-term (format nil "%~A%" search)))
        (push search-term params)
        (push search-term params)
        (push search-term params)))
    (when conditions
      (setf sql (format nil "~A AND ~{~A~^ AND ~}" sql (reverse conditions))))
    (setf sql (format nil "~A ORDER BY i.item_number" sql))
    (when limit
      (setf sql (format nil "~A LIMIT ~A" sql limit)))
    (when offset
      (setf sql (format nil "~A OFFSET ~A" sql offset)))
    (apply #'fetch-all sql (reverse params))))

(defun get-inventory-item (id)
  "Get a single inventory item by ID."
  (fetch-one 
   "SELECT i.*, l.location_code, l.location_name
    FROM inventory_items i
    LEFT JOIN inventory_locations l ON i.location_id = l.id
    WHERE i.id = ?" id))

(defun get-inventory-locations ()
  "Get all inventory locations."
  (fetch-all 
   "SELECT l.*, 
           COUNT(i.id) as item_count,
           COALESCE(SUM(i.total_cost), 0) as total_value
    FROM inventory_locations l
    LEFT JOIN inventory_items i ON i.location_id = l.id
    GROUP BY l.id
    ORDER BY l.location_code"))

(defun get-inventory-location (id)
  "Get a single inventory location by ID."
  (fetch-one "SELECT * FROM inventory_locations WHERE id = ?" id))

(defun get-inventory-summary-stats ()
  "Get summary statistics for inventory."
  (fetch-one 
   "SELECT 
      COUNT(*) as total_items,
      SUM(quantity) as total_quantity,
      SUM(total_cost) as total_value,
      SUM(CASE WHEN property_type = 'Material' THEN 1 ELSE 0 END) as material_count,
      SUM(CASE WHEN property_type = 'Equipment' THEN 1 ELSE 0 END) as equipment_count,
      (SELECT COUNT(*) FROM inventory_locations) as location_count
    FROM inventory_items
    WHERE active = 1"))

(defun get-inventory-by-location-summary ()
  "Get inventory summary grouped by location."
  (fetch-all 
   "SELECT l.location_code, l.location_type, l.id,
           COUNT(i.id) as item_count,
           SUM(i.quantity) as total_quantity,
           SUM(i.total_cost) as total_value
    FROM inventory_locations l
    LEFT JOIN inventory_items i ON i.location_id = l.id AND i.active = 1
    GROUP BY l.id
    ORDER BY total_value DESC"))

(defun update-inventory-quantity (item-id new-quantity &key performed-by notes)
  "Update inventory quantity and record transaction."
  (let* ((item (get-inventory-item item-id))
         (old-quantity (or (getf item :|quantity|) 0))
         (difference (- new-quantity old-quantity))
         (unit-cost (or (getf item :|unit_cost|) 0)))
    ;; Update the item
    (execute-sql 
     "UPDATE inventory_items 
      SET quantity = ?, total_cost = ? * ?, updated_at = datetime('now')
      WHERE id = ?"
     new-quantity new-quantity unit-cost item-id)
    ;; Record transaction
    (execute-sql 
     "INSERT INTO inventory_transactions 
      (item_id, transaction_type, quantity, quantity_before, quantity_after,
       unit_cost, total_cost, performed_by, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     item-id 
     (if (> difference 0) "Receipt" "Adjustment")
     (abs difference) old-quantity new-quantity
     unit-cost (* (abs difference) unit-cost)
     performed-by notes)))

;;; ============================================================
;;; Inventory UI Handlers
;;; ============================================================

(defun handle-inventory ()
  "Handle main inventory page."
  (let* ((search (hunchentoot:parameter "search"))
         (location-param (hunchentoot:parameter "location"))
         (location-id (when (and location-param (not (string= location-param "")))
                        (parse-integer location-param :junk-allowed t)))
         (type-param (hunchentoot:parameter "type"))
         (property-type (when (and type-param (not (string= type-param ""))) type-param))
         (page-param (hunchentoot:parameter "page"))
         (page (or (when page-param (parse-integer page-param :junk-allowed t)) 1))
         (per-page 50)
         (offset (* (1- page) per-page))
         (stats (get-inventory-summary-stats))
         (locations (get-inventory-locations))
         (items (get-inventory-items :location-id location-id 
                                     :property-type property-type
                                     :search search
                                     :limit per-page 
                                     :offset offset)))
    (html-response
     (render-page "EBOM Inventory"
       (cl-who:with-html-output-to-string (s)
         ;; Compact header with inline stats
         (:div :class "page-header" :style "margin-bottom: 1rem;"
           (:div :style "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 1rem;"
             (:h1 :style "margin: 0;" "EBOM Inventory")
             (:div :style "display: flex; gap: 0.5rem; align-items: center; flex-wrap: wrap;"
               (:span :class "badge badge-secondary" (cl-who:fmt "~:D Items" (or (getf stats :|total_items|) 0)))
               (:span :class "badge badge-success" (cl-who:fmt "$~:D" (round (or (getf stats :|total_value|) 0))))
               (:span :class "badge badge-info" (cl-who:fmt "~:D Materials" (or (getf stats :|material_count|) 0)))
               (:span :class "badge badge-warning" (cl-who:fmt "~:D Equipment" (or (getf stats :|equipment_count|) 0)))
               (:span :class "badge badge-secondary" (cl-who:fmt "~:D Locations" (or (getf stats :|location_count|) 0))))
             (:div :class "header-actions"
               (:a :href "/inventory/locations" :class "btn btn-secondary btn-sm" "Locations")
               (:a :href "/inventory/audit/new" :class "btn btn-primary btn-sm" "New Audit"))))
         
         ;; Filters - compact inline
         (:div :class "card" :style "padding: 0.75rem; margin-bottom: 1rem;"
           (:form :method "get" :action "/inventory" :style "display: flex; gap: 0.75rem; align-items: flex-end; flex-wrap: wrap;"
             (:div :style "flex: 1; min-width: 200px;"
               (:label :style "font-size: 0.85rem; margin-bottom: 0.25rem; display: block;" "Search")
               (:input :type "text" :name "search" :placeholder "Part #, Description, Make..."
                       :value (or search "") :style "width: 100%;"))
             (:div :style "min-width: 180px;"
               (:label :style "font-size: 0.85rem; margin-bottom: 0.25rem; display: block;" "Location")
               (:select :name "location" :style "width: 100%;"
                 (:option :value "" "All Locations")
                 (dolist (loc locations)
                   (let* ((loc-id (getf loc :|id|))
                          (loc-code (getf loc :|location_code|))
                          (item-count (or (getf loc :|item_count|) 0)))
                     (cl-who:htm
                      (:option :value (format nil "~A" loc-id)
                               :selected (when (and location-id (= location-id loc-id)) "selected")
                               (cl-who:str (format nil "~A (~A)" loc-code item-count))))))))
             (:div :style "min-width: 120px;"
               (:label :style "font-size: 0.85rem; margin-bottom: 0.25rem; display: block;" "Type")
               (:select :name "type" :style "width: 100%;"
                 (:option :value "" "All Types")
                 (:option :value "Material" :selected (when (equal property-type "Material") "selected") "Material")
                 (:option :value "Equipment" :selected (when (equal property-type "Equipment") "selected") "Equipment")))
             (:div :style "display: flex; gap: 0.5rem;"
               (:button :type "submit" :class "btn btn-primary btn-sm" "Filter")
               (:a :href "/inventory" :class "btn btn-secondary btn-sm" "Clear"))))
         
         ;; Items table
         (:div :class "card"
           (:h2 (cl-who:fmt "Inventory Items (~A)" (length items)))
           (:table :class "data-table"
             (:thead
               (:tr
                 (:th "#")
                 (:th "Description")
                 (:th "Make")
                 (:th "Part Number")
                 (:th "Location")
                 (:th "UOM")
                 (:th :class "text-right" "Qty")
                 (:th :class "text-right" "Unit Cost")
                 (:th :class "text-right" "Total")
                 (:th "Type")))
             (:tbody
               (if items
                   (dolist (item items)
                     (cl-who:htm
                      (:tr
                        (:td (:a :href (format nil "/inventory/item/~A" (getf item :|id|))
                                 (cl-who:str (getf item :|item_number|))))
                        (:td (cl-who:str (getf item :|description|)))
                        (:td (cl-who:str (or (getf item :|make|) "")))
                        (:td (:code (cl-who:str (or (getf item :|part_number|) ""))))
                        (:td (cl-who:str (or (getf item :|location_code|) "—")))
                        (:td (cl-who:str (getf item :|uom|)))
                        (:td :class "text-right" (cl-who:fmt "~:D" (round (or (getf item :|quantity|) 0))))
                        (:td :class "text-right" (cl-who:fmt "$~,2F" (or (getf item :|unit_cost|) 0)))
                        (:td :class "text-right" (cl-who:fmt "$~,2F" (or (getf item :|total_cost|) 0)))
                        (:td (:span :class (format nil "badge badge-~A" 
                                                   (if (string= (getf item :|property_type|) "Equipment")
                                                       "info" "secondary"))
                                    (cl-who:str (getf item :|property_type|)))))))
                   (cl-who:htm
                    (:tr (:td :colspan "10" :class "text-center" "No items found"))))))))))))

(defun handle-inventory-locations ()
  "Handle inventory locations page."
  (let ((locations (get-inventory-by-location-summary)))
    (html-response
     (render-page "Inventory Locations"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "Inventory Locations")
           (:a :href "/inventory" :class "btn btn-secondary" "Back to Inventory"))
         
         (:div :class "card"
           (:table :class "data-table"
             (:thead
               (:tr
                 (:th "Location Code")
                 (:th "Type")
                 (:th :class "text-right" "Items")
                 (:th :class "text-right" "Total Qty")
                 (:th :class "text-right" "Total Value")))
             (:tbody
               (dolist (loc locations)
                 (cl-who:htm
                  (:tr
                    (:td (:a :href (format nil "/inventory?location=~A" 
                                           (getf loc :|id|))
                             (cl-who:str (getf loc :|location_code|))))
                    (:td (cl-who:str (or (getf loc :|location_type|) "Container")))
                    (:td :class "text-right" (cl-who:fmt "~:D" (or (getf loc :|item_count|) 0)))
                    (:td :class "text-right" (cl-who:fmt "~:D" (round (or (getf loc :|total_quantity|) 0))))
                    (:td :class "text-right" (cl-who:fmt "$~,2F" (or (getf loc :|total_value|) 0))))))))))))))

(defun handle-inventory-item-detail (id-str)
  "Handle inventory item detail page."
  (let* ((id (parse-integer id-str :junk-allowed t))
         (item (when id (get-inventory-item id))))
    (if item
        (html-response
         (render-page (format nil "Item #~A" (getf item :|item_number|))
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 (cl-who:fmt "Item #~A: ~A" 
                                (getf item :|item_number|)
                                (getf item :|description|)))
               (:a :href "/inventory" :class "btn btn-secondary" "Back to Inventory"))
             
             (:div :class "card"
               (:h2 "Item Details")
               (:div :class "detail-grid"
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Item Number")
                   (:span :class "detail-value" (cl-who:str (getf item :|item_number|))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Description")
                   (:span :class "detail-value" (cl-who:str (getf item :|description|))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Make")
                   (:span :class "detail-value" (cl-who:str (or (getf item :|make|) "—"))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Part Number")
                   (:span :class "detail-value" (:code (cl-who:str (or (getf item :|part_number|) "—")))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Location")
                   (:span :class "detail-value" (cl-who:str (or (getf item :|location_code|) "—"))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Unit of Measure")
                   (:span :class "detail-value" (cl-who:str (getf item :|uom|))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Quantity")
                   (:span :class "detail-value" (cl-who:fmt "~:D" (round (or (getf item :|quantity|) 0)))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Unit Cost")
                   (:span :class "detail-value" (cl-who:fmt "$~,2F" (or (getf item :|unit_cost|) 0))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Total Cost")
                   (:span :class "detail-value" (cl-who:fmt "$~,2F" (or (getf item :|total_cost|) 0))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Property Type")
                   (:span :class "detail-value" 
                     (:span :class (format nil "badge badge-~A" 
                                           (if (string= (getf item :|property_type|) "Equipment")
                                               "info" "secondary"))
                            (cl-who:str (getf item :|property_type|)))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Property Usage")
                   (:span :class "detail-value" (cl-who:str (or (getf item :|property_usage|) "—"))))
                 (:div :class "detail-row"
                   (:span :class "detail-label" "Last Audit")
                   (:span :class "detail-value" (cl-who:str (or (getf item :|last_audit_date|) "Never")))))))))
        (html-response
         (render-page "Item Not Found"
           (cl-who:with-html-output-to-string (s)
             (:div :class "card"
               (:h2 "Item Not Found")
               (:p "The requested inventory item could not be found.")
               (:a :href "/inventory" :class "btn" "Back to Inventory"))))))))

(defun handle-inventory-audit-new ()
  "Handle new inventory audit page."
  (html-response
   (render-page "New Inventory Audit"
     (cl-who:with-html-output-to-string (s)
       (:div :class "page-header"
         (:h1 "New Inventory Audit")
         (:a :href "/inventory" :class "btn btn-secondary" "Cancel"))
       
       (:div :class "card"
         (:p "Inventory audit functionality is under development.")
         (:p "This will allow you to:")
         (:ul
           (:li "Create 100% annual audits")
           (:li "Create 10% monthly audits")
           (:li "Record actual counts vs expected")
           (:li "Generate audit reports")))))))

(defun handle-inventory-audit-detail (id-str)
  "Handle inventory audit detail page."
  (declare (ignore id-str))
  (html-response
   (render-page "Audit Details"
     (cl-who:with-html-output-to-string (s)
       (:div :class "card"
         (:h2 "Audit Details")
         (:p "Audit detail view is under development.")
         (:a :href "/inventory" :class "btn" "Back to Inventory"))))))
