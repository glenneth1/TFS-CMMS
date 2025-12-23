;;;; mrf.lisp - Material Request Form (MRF) functionality

(in-package #:tfs-cmms)

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun format-date-iso (universal-time)
  "Format a universal time as ISO date string YYYY-MM-DD."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month date)))

(defun escape-json-string (str)
  "Escape special characters in a string for JSON output."
  (if (null str)
      ""
      (with-output-to-string (out)
        (loop for char across str do
          (case char
            (#\" (write-string "\\\"" out))
            (#\\ (write-string "\\\\" out))
            (#\Newline (write-string "\\n" out))
            (#\Return (write-string "\\r" out))
            (#\Tab (write-string "\\t" out))
            (otherwise (write-char char out)))))))

;;; ============================================================
;;; MRF Database Functions
;;; ============================================================

(defun generate-mrf-number (base building-number date)
  "Generate MRF number in format: Base-Building-MRF-DD-MM-YY or Base-Building-MRF-DD-MM-YY-NN for duplicates"
  (let* ((date-str (if (stringp date)
                       date
                       (format nil "~A" date)))
         ;; Parse date and format as DD-MM-YY
         (formatted-date (if (and (>= (length date-str) 10)
                                  (char= (char date-str 4) #\-))
                             ;; ISO format YYYY-MM-DD
                             (format nil "~A-~A-~A"
                                     (subseq date-str 8 10)
                                     (subseq date-str 5 7)
                                     (subseq date-str 2 4))
                             date-str))
         ;; Clean up base and building names for the MRF number
         (clean-base (substitute #\- #\Space (string-trim " " (or base ""))))
         (clean-building (substitute #\- #\Space (string-trim " " (or building-number ""))))
         ;; Base MRF number without sequence
         (base-mrf-number (format nil "~A-~A-MRF-~A" clean-base clean-building formatted-date))
         ;; Check for existing MRFs with same base number
         (existing-count (getf (fetch-one 
                                "SELECT COUNT(*) as cnt FROM material_requests 
                                 WHERE mrf_number LIKE ?" 
                                (format nil "~A%" base-mrf-number))
                               :|cnt|)))
    (if (and existing-count (> existing-count 0))
        ;; Add sequence number
        (format nil "~A-~2,'0D" base-mrf-number (1+ existing-count))
        base-mrf-number)))

(defun create-mrf-from-inspection (report-id)
  "Create a new MRF linked to an inspection report."
  (let* ((report (fetch-one 
                  "SELECT ir.*, s.name as site_name
                   FROM inspection_reports ir
                   LEFT JOIN sites s ON ir.site_id = s.id
                   WHERE ir.id = ?" report-id))
         (base (or (getf report :|site_name|) ""))
         (building (or (getf report :|building_number|) ""))
         (team-number (or (getf report :|team_number|) ""))
         (inspector (or (getf report :|inspector1_name|) ""))
         (contract-number (or (get-system-setting "contract_number") "W912DY24R0043"))
         (today (format-date-iso (get-universal-time)))
         (mrf-number (generate-mrf-number base building today)))
    ;; Check if MRF already exists for this report
    (let ((existing (fetch-one 
                     "SELECT id FROM material_requests WHERE inspection_report_id = ?" 
                     report-id)))
      (if existing
          (getf existing :|id|)
          ;; Create new MRF
          (progn
            (execute-sql 
             "INSERT INTO material_requests 
              (mrf_number, inspection_report_id, request_date, team_number, 
               base, building_number, requestor_name, contract_number, site_support_location, status)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'Draft')"
             mrf-number report-id today team-number base building inspector contract-number base)
            (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|))))))

(defun get-mrf (id)
  "Get a single MRF by ID."
  (fetch-one 
   "SELECT mr.*, ir.report_number, ir.tag_id as report_tag_id
    FROM material_requests mr
    LEFT JOIN inspection_reports ir ON mr.inspection_report_id = ir.id
    WHERE mr.id = ?" id))

(defun get-mrf-by-report (report-id)
  "Get MRF linked to an inspection report."
  (fetch-one 
   "SELECT mr.*, 
           (SELECT COUNT(*) FROM material_request_items WHERE mrf_id = mr.id) as item_count
    FROM material_requests mr 
    WHERE mr.inspection_report_id = ?" report-id))

(defun get-all-mrfs (&key status limit offset)
  "Get all MRFs with optional filters."
  (let ((sql "SELECT mr.*, ir.report_number, ir.tag_id as report_tag_id,
                     (SELECT COUNT(*) FROM material_request_items WHERE mrf_id = mr.id) as item_count
              FROM material_requests mr
              LEFT JOIN inspection_reports ir ON mr.inspection_report_id = ir.id")
        (conditions nil)
        (params nil))
    (when status
      (push "mr.status = ?" conditions)
      (push status params))
    (when conditions
      (setf sql (format nil "~A WHERE ~{~A~^ AND ~}" sql (reverse conditions))))
    (setf sql (format nil "~A ORDER BY mr.created_at DESC" sql))
    (when limit
      (setf sql (format nil "~A LIMIT ~A" sql limit)))
    (when offset
      (setf sql (format nil "~A OFFSET ~A" sql offset)))
    (apply #'fetch-all sql (reverse params))))

(defun get-mrf-items (mrf-id)
  "Get all line items for an MRF."
  (fetch-all 
   "SELECT mri.*, ii.description as inv_description, ii.make, ii.uom as inv_uom
    FROM material_request_items mri
    LEFT JOIN inventory_items ii ON mri.item_id = ii.id
    WHERE mri.mrf_id = ?
    ORDER BY mri.line_number" mrf-id))

(defun add-mrf-item (mrf-id &key item-id part-number description quantity uom 
                            is-material is-tool remarks web-link)
  "Add a line item to an MRF."
  (let ((next-line (or (getf (fetch-one 
                              "SELECT COALESCE(MAX(line_number), 0) + 1 as next_line 
                               FROM material_request_items WHERE mrf_id = ?" mrf-id)
                             :|next_line|)
                       1)))
    (execute-sql 
     "INSERT INTO material_request_items 
      (mrf_id, line_number, item_id, part_number, description, quantity_requested, 
       uom, is_material, is_tool, remarks, web_link)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     mrf-id next-line item-id part-number description quantity uom
     (if is-material 1 0) (if is-tool 1 0) remarks web-link)))

(defun update-mrf-item (item-id &key part-number description quantity uom 
                                is-material is-tool remarks web-link)
  "Update an MRF line item."
  (execute-sql 
   "UPDATE material_request_items 
    SET part_number = ?, description = ?, quantity_requested = ?, uom = ?,
        is_material = ?, is_tool = ?, remarks = ?, web_link = ?
    WHERE id = ?"
   part-number description quantity uom
   (if is-material 1 0) (if is-tool 1 0) remarks web-link item-id))

(defun delete-mrf-item (item-id)
  "Delete an MRF line item."
  (execute-sql "DELETE FROM material_request_items WHERE id = ?" item-id))

(defun update-mrf-status (mrf-id new-status &key approved-by issued-by)
  "Update MRF status."
  (cond
    ((string= new-status "Approved")
     (execute-sql 
      "UPDATE material_requests SET status = ?, approved_by = ?, approved_at = datetime('now'), updated_at = datetime('now') WHERE id = ?"
      new-status approved-by mrf-id)
     ;; Deduct inventory for approved MRF items
     (deduct-inventory-for-mrf mrf-id))
    ((string= new-status "Issued")
     (execute-sql 
      "UPDATE material_requests SET status = ?, issued_by = ?, issued_at = datetime('now'), updated_at = datetime('now') WHERE id = ?"
      new-status issued-by mrf-id))
    (t
     (execute-sql 
      "UPDATE material_requests SET status = ?, updated_at = datetime('now') WHERE id = ?"
      new-status mrf-id))))

(defun deduct-inventory-for-mrf (mrf-id)
  "Deduct inventory quantities for all items in an approved MRF that are linked to inventory."
  (let ((items (fetch-all 
                "SELECT mri.id, mri.item_id, mri.quantity_requested, ii.part_number, ii.quantity_on_hand
                 FROM material_request_items mri
                 LEFT JOIN inventory_items ii ON mri.item_id = ii.id
                 WHERE mri.mrf_id = ? AND mri.item_id IS NOT NULL"
                mrf-id)))
    (dolist (item items)
      (let ((item-id (getf item :|item_id|))
            (qty-requested (or (getf item :|quantity_requested|) 0))
            (qty-on-hand (or (getf item :|quantity_on_hand|) 0)))
        (when (and item-id (> qty-requested 0))
          ;; Deduct from inventory (allow negative for backorder tracking)
          (execute-sql 
           "UPDATE inventory_items SET quantity_on_hand = quantity_on_hand - ?, updated_at = datetime('now') WHERE id = ?"
           qty-requested item-id)
          ;; Record the transaction
          (execute-sql
           "INSERT INTO inventory_transactions (item_id, transaction_type, quantity, reference_type, reference_id, notes, created_at)
            VALUES (?, 'OUT', ?, 'MRF', ?, 'Deducted on MRF approval', datetime('now'))"
           item-id qty-requested mrf-id))))))

(defun get-inventory-for-mrf-dropdown ()
  "Get inventory items for part number dropdown."
  (fetch-all 
   "SELECT id, part_number, description, make, uom 
    FROM inventory_items 
    WHERE active = 1 AND part_number IS NOT NULL AND part_number != ''
    ORDER BY part_number"))

;;; ============================================================
;;; MRF UI Handlers
;;; ============================================================

(defun handle-mrf-list ()
  "Handle MRF list page."
  (let* ((status-param (hunchentoot:parameter "status"))
         (mrfs (get-all-mrfs :status (when (and status-param (not (string= status-param ""))) 
                                       status-param))))
    (html-response
     (render-page "Material Request Forms"
       (cl-who:with-html-output-to-string (s)
         (:div :style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 1rem;"
           (:h1 :style "margin: 0;" "Material Request Forms")
           (:a :href "/mrf/new" :class "btn btn-primary" "New MRF"))
         
         ;; Filter by status
         (:div :class "card" :style "padding: 0.75rem; margin-bottom: 1rem;"
           (:form :method "get" :action "/mrf" :style "display: flex; gap: 0.75rem; align-items: center;"
             (:label "Status:")
             (:select :name "status" :style "min-width: 150px;"
               (:option :value "" "All")
               (:option :value "Draft" :selected (when (equal status-param "Draft") "selected") "Draft")
               (:option :value "Submitted" :selected (when (equal status-param "Submitted") "selected") "Submitted")
               (:option :value "Approved" :selected (when (equal status-param "Approved") "selected") "Approved")
               (:option :value "Issued" :selected (when (equal status-param "Issued") "selected") "Issued"))
             (:button :type "submit" :class "btn btn-sm btn-primary" "Filter")))
         
         ;; MRF table
         (:div :class "card"
           (:table :class "data-table"
             (:thead
               (:tr
                 (:th "MRF Number")
                 (:th "Date")
                 (:th "Base")
                 (:th "Building")
                 (:th "Requestor")
                 (:th "Items")
                 (:th "Status")
                 (:th "Actions")))
             (:tbody
               (if mrfs
                   (dolist (mrf mrfs)
                     (cl-who:htm
                      (:tr
                        (:td (:a :href (format nil "/mrf/~A" (getf mrf :|id|))
                                 (cl-who:str (getf mrf :|mrf_number|))))
                        (:td (cl-who:str (format-date-display (getf mrf :|request_date|))))
                        (:td (cl-who:str (or (getf mrf :|base|) "")))
                        (:td (cl-who:str (or (getf mrf :|building_number|) "")))
                        (:td (cl-who:str (or (getf mrf :|requestor_name|) "")))
                        (:td (cl-who:fmt "~A" (or (getf mrf :|item_count|) 0)))
                        (:td (:span :class (format nil "badge badge-~A"
                                                   (cond ((string= (getf mrf :|status|) "Draft") "secondary")
                                                         ((string= (getf mrf :|status|) "Submitted") "warning")
                                                         ((string= (getf mrf :|status|) "Approved") "info")
                                                         ((string= (getf mrf :|status|) "Issued") "success")
                                                         (t "secondary")))
                                    (cl-who:str (getf mrf :|status|))))
                        (:td 
                          (:a :href (format nil "/mrf/~A" (getf mrf :|id|)) :class "btn btn-sm" "View")
                          (:a :href (format nil "/mrf/~A/print" (getf mrf :|id|)) :class "btn btn-sm btn-secondary" "Print")))))
                   (cl-who:htm
                    (:tr (:td :colspan "8" :class "text-center" "No MRFs found"))))))))))))

(defun handle-mrf-detail (id-str)
  "Handle MRF detail/edit page."
  (let* ((id (parse-integer id-str :junk-allowed t))
         (mrf (when id (get-mrf id)))
         (items (when id (get-mrf-items id)))
         (inventory-parts (get-inventory-for-mrf-dropdown)))
    (if mrf
        (html-response
         (render-page (format nil "MRF: ~A" (getf mrf :|mrf_number|))
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header" :style "margin-bottom: 1rem;"
               (:div :style "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 1rem;"
                 (:h1 :style "margin: 0;" (cl-who:str (getf mrf :|mrf_number|)))
                 (:div :style "display: flex; gap: 0.5rem;"
                   (:span :class (format nil "badge badge-~A"
                                         (cond ((string= (getf mrf :|status|) "Draft") "secondary")
                                               ((string= (getf mrf :|status|) "Submitted") "warning")
                                               ((string= (getf mrf :|status|) "Approved") "info")
                                               ((string= (getf mrf :|status|) "Issued") "success")
                                               (t "secondary")))
                          (cl-who:str (getf mrf :|status|)))
                   (:a :href "/mrf" :class "btn btn-secondary btn-sm" "Back to List")
                   (:a :href (format nil "/mrf/~A/print" id) :class "btn btn-sm" "Print"))))
             
             ;; MRF Header Info
             (:div :class "card" :style "margin-bottom: 1rem;"
               (:div :style "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem;"
                 (:div
                   (:label :style "font-weight: bold; display: block;" "Date")
                   (:span (cl-who:str (format-date-display (getf mrf :|request_date|)))))
                 (:div
                   (:label :style "font-weight: bold; display: block;" "Team Number")
                   (:span (cl-who:str (or (getf mrf :|team_number|) ""))))
                 (:div
                   (:label :style "font-weight: bold; display: block;" "Base")
                   (:span (cl-who:str (or (getf mrf :|base|) ""))))
                 (:div
                   (:label :style "font-weight: bold; display: block;" "Building")
                   (:span (cl-who:str (or (getf mrf :|building_number|) ""))))
                 (:div
                   (:label :style "font-weight: bold; display: block;" "Requestor")
                   (:span (cl-who:str (or (getf mrf :|requestor_name|) ""))))
                 (:div
                   (:label :style "font-weight: bold; display: block;" "Contract")
                   (:span (cl-who:str (or (getf mrf :|contract_number|) ""))))))
             
             ;; Line Items
             (:div :class "card"
               (:h2 "Line Items")
               (:table :class "data-table"
                 (:thead
                   (:tr
                     (:th "#")
                     (:th "Part Number")
                     (:th "Description")
                     (:th "Qty")
                     (:th "U/I")
                     (:th "Material")
                     (:th "Tool")
                     (:th "Remarks")
                     (:th "Web Link")
                     (when (string= (getf mrf :|status|) "Draft")
                       (cl-who:htm (:th "Actions")))))
                 (:tbody
                   (if items
                       (dolist (item items)
                         (cl-who:htm
                          (:tr
                            (:td (cl-who:fmt "~A" (getf item :|line_number|)))
                            (:td (:code (cl-who:str (or (getf item :|part_number|) "N/A"))))
                            (:td (cl-who:str (or (getf item :|description|) "")))
                            (:td (cl-who:fmt "~A" (or (getf item :|quantity_requested|) 0)))
                            (:td (cl-who:str (or (getf item :|uom|) "")))
                            (:td (if (= (or (getf item :|is_material|) 0) 1) "Yes" "No"))
                            (:td (if (= (or (getf item :|is_tool|) 0) 1) "Yes" "No"))
                            (:td (cl-who:str (or (getf item :|remarks|) "")))
                            (:td (let ((link (getf item :|web_link|)))
                                   (when (and link (not (string= link "")))
                                     (cl-who:htm
                                      (:a :href link :target "_blank" "Link")))))
                            (when (string= (getf mrf :|status|) "Draft")
                              (cl-who:htm
                               (:td 
                                 (:form :method "post" :action (format nil "/api/mrf/~A/item/~A/delete" id (getf item :|id|))
                                        :style "display: inline;"
                                   (:button :type "submit" :class "btn btn-sm btn-danger" 
                                            :onclick "return confirm('Delete this item?');" "×"))))))))
                       (cl-who:htm
                        (:tr (:td :colspan "10" :class "text-center" "No items added yet"))))))
               
               ;; Add new item form (only for Draft status)
               (when (string= (getf mrf :|status|) "Draft")
                 (cl-who:htm
                  (:h3 :style "margin-top: 1.5rem;" "Add Item")
                  (:form :method "post" :action (format nil "/api/mrf/~A/item" id)
                    (:div :style "display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 0.75rem; margin-bottom: 1rem;"
                      (:div :style "position: relative;"
                        (:label "Part Number")
                        (:input :type "text" :name "part_number" :id "part-input" 
                                :style "width: 100%;" :autocomplete "off"
                                :placeholder "Type to search...")
                        (:div :id "part-suggestions" :class "autocomplete-suggestions"))
                      (:div :style "position: relative;"
                        (:label "Description")
                        (:input :type "text" :name "description" :id "description-input" 
                                :style "width: 100%;" :required t :autocomplete "off"
                                :placeholder "Type to search or enter manually...")
                        (:div :id "desc-suggestions" :class "autocomplete-suggestions"))
                      (:div
                        (:label "Quantity")
                        (:input :type "number" :name "quantity" :min "1" :value "1" 
                                :style "width: 100%;" :required t))
                      (:div
                        (:label "U/I")
                        (:input :type "text" :name "uom" :id "uom-input" :placeholder "EA, ROLL, etc."
                                :style "width: 100%;"))
                      (:div
                        (:label "Material?")
                        (:select :name "is_material" :style "width: 100%;"
                          (:option :value "1" :selected "selected" "Yes")
                          (:option :value "0" "No")))
                      (:div
                        (:label "Tool?")
                        (:select :name "is_tool" :style "width: 100%;"
                          (:option :value "0" :selected "selected" "No")
                          (:option :value "1" "Yes")))
                      (:div
                        (:label "Remarks")
                        (:input :type "text" :name "remarks" :style "width: 100%;"))
                      (:div
                        (:label "Web Link")
                        (:input :type "url" :name "web_link" :placeholder "https://..." 
                                :style "width: 100%;")))
                    (:input :type "hidden" :name "item_id" :id "item-id-input" :value "")
                    (:button :type "submit" :class "btn btn-primary" "Add Item"))
                  
                  ;; Inventory data for autocomplete
                  (:script 
                   (cl-who:str 
                    (format nil "var inventoryItems = ~A;" 
                            (with-output-to-string (out)
                              (format out "[")
                              (loop for part in inventory-parts
                                    for i from 0
                                    do (when (> i 0) (format out ","))
                                       (format out "{\"id\":~A,\"pn\":~S,\"desc\":~S,\"uom\":~S}"
                                               (getf part :|id|)
                                               (escape-json-string (or (getf part :|part_number|) ""))
                                               (escape-json-string (or (getf part :|description|) ""))
                                               (escape-json-string (or (getf part :|uom|) ""))))
                              (format out "]")))))
                  
                  ;; JavaScript for autocomplete
                  (:script "
var partInput = document.getElementById('part-input');
var descInput = document.getElementById('description-input');
var uomInput = document.getElementById('uom-input');
var itemIdInput = document.getElementById('item-id-input');
var partSuggestions = document.getElementById('part-suggestions');
var descSuggestions = document.getElementById('desc-suggestions');

function showSuggestions(input, container, searchField) {
  var query = input.value.toLowerCase();
  container.innerHTML = '';
  if (query.length < 2) { container.style.display = 'none'; return; }
  
  var matches = inventoryItems.filter(function(item) {
    return item[searchField].toLowerCase().indexOf(query) !== -1;
  }).slice(0, 10);
  
  if (matches.length === 0) { container.style.display = 'none'; return; }
  
  matches.forEach(function(item) {
    var div = document.createElement('div');
    div.className = 'autocomplete-item';
    div.innerHTML = '<strong>' + item.pn + '</strong> - ' + item.desc;
    div.onclick = function() {
      partInput.value = item.pn;
      descInput.value = item.desc;
      uomInput.value = item.uom;
      itemIdInput.value = item.id;
      partSuggestions.style.display = 'none';
      descSuggestions.style.display = 'none';
    };
    container.appendChild(div);
  });
  container.style.display = 'block';
}

partInput.addEventListener('input', function() {
  itemIdInput.value = '';
  showSuggestions(partInput, partSuggestions, 'pn');
});

descInput.addEventListener('input', function() {
  itemIdInput.value = '';
  showSuggestions(descInput, descSuggestions, 'desc');
});

document.addEventListener('click', function(e) {
  if (!partInput.contains(e.target) && !partSuggestions.contains(e.target)) {
    partSuggestions.style.display = 'none';
  }
  if (!descInput.contains(e.target) && !descSuggestions.contains(e.target)) {
    descSuggestions.style.display = 'none';
  }
});
"))))
             
             ;; Navigation - Return to Report if linked
             (let ((report-id (getf mrf :|inspection_report_id|)))
               (when report-id
                 (cl-who:htm
                  (:div :class "card" :style "margin-top: 1rem;"
                    (:p :class "text-muted" "This MRF is linked to an inspection report. Submit both from the report page.")
                    (:a :href (format nil "/inspection-reports/~A" report-id)
                        :class "btn btn-primary" "← Return to Inspection Report"))))))))
        (html-response
         (render-page "MRF Not Found"
           (cl-who:with-html-output-to-string (s)
             (:div :class "card"
               (:h2 "MRF Not Found")
               (:p "The requested MRF could not be found.")
               (:a :href "/mrf" :class "btn" "Back to MRF List"))))))))

(defun handle-mrf-new ()
  "Handle new MRF creation page."
  (let ((sites (fetch-all "SELECT id, code, name FROM sites ORDER BY name")))
    (html-response
     (render-page "New Material Request Form"
       (cl-who:with-html-output-to-string (s)
         (:div :class "page-header"
           (:h1 "New Material Request Form")
           (:a :href "/mrf" :class "btn btn-secondary" "Cancel"))
         
         (:div :class "card"
           (:form :method "post" :action "/api/mrf/create"
             (:div :style "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem;"
               (:div :class "form-group"
                 (:label "Base *")
                 (:select :name "base" :required t
                   (:option :value "" "-- Select Base --")
                   (dolist (site sites)
                     (cl-who:htm
                      (:option :value (getf site :|name|)
                               (cl-who:str (getf site :|name|)))))))
               (:div :class "form-group"
                 (:label "Building # / Name *")
                 (:input :type "text" :name "building_number" :required t))
               (:div :class "form-group"
                 (:label "Camp Name")
                 (:input :type "text" :name "camp_name"))
               (:div :class "form-group"
                 (:label "Team Number")
                 (:input :type "text" :name "team_number"))
               (:div :class "form-group"
                 (:label "Requestor Name *")
                 (:input :type "text" :name "requestor_name" :required t)))
             (:button :type "submit" :class "btn btn-primary" "Create MRF"))))))))

(defun handle-mrf-print (id-str)
  "Handle printable MRF view."
  (let* ((id (parse-integer id-str :junk-allowed t))
         (mrf (when id (get-mrf id)))
         (items (when id (get-mrf-items id))))
    (if mrf
        (progn
          (hunchentoot:no-cache)
          (html-response
         (cl-who:with-html-output-to-string (s nil :prologue t)
           (:html
             (:head
               (:title (cl-who:str (getf mrf :|mrf_number|)))
               (:style "
@media print {
  body { margin: 0; padding: 20px; }
  .no-print { display: none; }
}
body { font-family: Arial, sans-serif; font-size: 11pt; max-width: 800px; margin: 0 auto; padding: 20px; }
.header { display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 20px; border-bottom: 2px solid #333; padding-bottom: 10px; }
.logo { height: 60px; }
.title { text-align: center; flex: 1; }
.title h1 { margin: 0; font-size: 16pt; }
.title h2 { margin: 5px 0 0 0; font-size: 12pt; font-weight: normal; }
.info-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 20px; }
.info-row { display: flex; }
.info-label { font-weight: bold; min-width: 120px; }
table { width: 100%; border-collapse: collapse; margin-top: 20px; }
th, td { border: 1px solid #333; padding: 6px 8px; text-align: left; font-size: 10pt; }
th { background: #f0f0f0; font-weight: bold; }
.text-center { text-align: center; }
.mrf-number { font-size: 14pt; font-weight: bold; }
"))
             (:body
               (:div :class "no-print" :style "margin-bottom: 20px;"
                 (:button :onclick "window.print()" :style "padding: 10px 20px; cursor: pointer;" "Print")
                 (:a :href (format nil "/mrf/~A" id) :style "margin-left: 10px;" "Back to MRF"))
               
               (:div :class "header"
                 (:img :src "/static/img/TFS_Logo.png" :class "logo" :alt "TFS Logo")
                 (:div :class "title"
                   (:h1 "Materials / Tools Request")
                   (:h2 :class "mrf-number" (cl-who:str (getf mrf :|mrf_number|))))
                 (:div :style "text-align: right;"
                   (:div (:strong "Date: ") (cl-who:str (format-date-display (getf mrf :|request_date|))))
                   (:div (:strong "Contract: ") (cl-who:str (or (getf mrf :|contract_number|) "")))))
               
               (:div :class "info-grid"
                 (:div :class "info-row"
                   (:span :class "info-label" "Team Number:")
                   (:span (cl-who:str (or (getf mrf :|team_number|) ""))))
                 (:div :class "info-row"
                   (:span :class "info-label" "Site Support:")
                   (:span (cl-who:str (or (getf mrf :|site_support_location|) ""))))
                 (:div :class "info-row"
                   (:span :class "info-label" "Base:")
                   (:span (cl-who:str (or (getf mrf :|base|) ""))))
                 (:div :class "info-row"
                   (:span :class "info-label" "Tag ID:")
                   (:span (cl-who:str (getf mrf :|mrf_number|))))
                 (:div :class "info-row"
                   (:span :class "info-label" "Camp Name:")
                   (:span (cl-who:str (or (getf mrf :|camp_name|) "N/A"))))
                 (:div :class "info-row"
                   (:span :class "info-label" "Status:")
                   (:span (cl-who:str (getf mrf :|status|))))
                 (:div :class "info-row"
                   (:span :class "info-label" "BLD # / Name:")
                   (:span (cl-who:str (or (getf mrf :|building_number|) ""))))
                 (:div :class "info-row"
                   (:span :class "info-label" "Inspector:")
                   (:span (cl-who:str (or (getf mrf :|requestor_name|) "")))))
               
               (:table
                 (:thead
                   (:tr
                     (:th "#")
                     (:th "Part Number")
                     (:th "Nomenclature")
                     (:th "Qty")
                     (:th "U/I")
                     (:th :colspan "2" "Type")
                     (:th "Remarks")
                     (:th "Web Link"))
                   (:tr
                     (:th)
                     (:th)
                     (:th)
                     (:th)
                     (:th)
                     (:th "Mat")
                     (:th "Tool")
                     (:th)
                     (:th)))
                 (:tbody
                   (dolist (item items)
                     (cl-who:htm
                      (:tr
                        (:td :class "text-center" (cl-who:fmt "~A" (getf item :|line_number|)))
                        (:td (cl-who:str (or (getf item :|part_number|) "")))
                        (:td (cl-who:str (or (getf item :|description|) "")))
                        (:td :class "text-center" (cl-who:fmt "~A" (or (getf item :|quantity_requested|) 0)))
                        (:td (cl-who:str (or (getf item :|uom|) "")))
                        (:td :class "text-center" (if (= (or (getf item :|is_material|) 0) 1) "X" ""))
                        (:td :class "text-center" (if (= (or (getf item :|is_tool|) 0) 1) "X" ""))
                        (:td (cl-who:str (or (getf item :|remarks|) "")))
                        (:td (let ((link (getf item :|web_link|)))
                               (when (and link (not (string= link "")))
                                 (cl-who:htm (:a :href link :target "_blank" "Link"))))))))
                   ;; Empty rows for manual additions
                   (dotimes (i (max 0 (- 20 (length items))))
                     (cl-who:htm
                      (:tr
                        (:td :class "text-center" (cl-who:fmt "~A" (+ (length items) i 1)))
                        (:td :style "height: 25px;")
                        (:td)
                        (:td)
                        (:td)
                        (:td)
                        (:td)
                        (:td)
                        (:td)))))))))))
        (html-response
         (render-page "MRF Not Found"
           (cl-who:with-html-output-to-string (s)
             (:div :class "card"
               (:h2 "MRF Not Found")
               (:a :href "/mrf" :class "btn" "Back to MRF List"))))))))

;;; ============================================================
;;; MRF API Handlers
;;; ============================================================

(defun handle-api-mrf-create ()
  "Handle MRF creation API."
  (let* ((base (hunchentoot:parameter "base"))
         (building (hunchentoot:parameter "building_number"))
         (camp-name (hunchentoot:parameter "camp_name"))
         (team-number (hunchentoot:parameter "team_number"))
         (requestor (hunchentoot:parameter "requestor_name"))
         (contract-number (or (get-system-setting "contract_number") "W912DY24R0043"))
         (today (format-date-iso (get-universal-time)))
         (mrf-number (generate-mrf-number base building today)))
    (execute-sql 
     "INSERT INTO material_requests 
      (mrf_number, request_date, team_number, base, camp_name, building_number, 
       requestor_name, contract_number, site_support_location, status)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'Draft')"
     mrf-number today team-number base camp-name building requestor contract-number base)
    (let ((new-id (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))
      (hunchentoot:redirect (format nil "/mrf/~A" new-id)))))

(defun handle-api-mrf-add-item (mrf-id-str)
  "Handle adding item to MRF."
  (let* ((mrf-id (parse-integer mrf-id-str :junk-allowed t))
         (item-id-str (hunchentoot:parameter "item_id"))
         (item-id (when (and item-id-str (not (string= item-id-str "")))
                    (parse-integer item-id-str :junk-allowed t)))
         (part-number (hunchentoot:parameter "part_number"))
         (description (hunchentoot:parameter "description"))
         (quantity (parse-integer (or (hunchentoot:parameter "quantity") "1") :junk-allowed t))
         (uom (hunchentoot:parameter "uom"))
         (is-material (string= (hunchentoot:parameter "is_material") "1"))
         (is-tool (string= (hunchentoot:parameter "is_tool") "1"))
         (remarks (hunchentoot:parameter "remarks"))
         (web-link (hunchentoot:parameter "web_link")))
    (add-mrf-item mrf-id 
                  :item-id item-id
                  :part-number (if (string= part-number "") nil part-number)
                  :description description
                  :quantity (or quantity 1)
                  :uom uom
                  :is-material is-material
                  :is-tool is-tool
                  :remarks remarks
                  :web-link web-link)
    (hunchentoot:redirect (format nil "/mrf/~A" mrf-id))))

(defun handle-api-mrf-delete-item (mrf-id-str item-id-str)
  "Handle deleting item from MRF."
  (let ((item-id (parse-integer item-id-str :junk-allowed t)))
    (when item-id
      (delete-mrf-item item-id)))
  (hunchentoot:redirect (format nil "/mrf/~A" mrf-id-str)))

(defun handle-api-mrf-submit (mrf-id-str)
  "Handle MRF submission."
  (let ((mrf-id (parse-integer mrf-id-str :junk-allowed t)))
    (when mrf-id
      (update-mrf-status mrf-id "Submitted")))
  (hunchentoot:redirect (format nil "/mrf/~A" mrf-id-str)))
