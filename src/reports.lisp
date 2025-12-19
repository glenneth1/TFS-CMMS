(in-package #:tfs-cmms)

;;; Weekly Report Generation (per PWS Section 17.0 requirements)

(defun generate-weekly-report (site-id &key (output-format :alist))
  "Generate the weekly per-site CMMS report as required by PWS.
   Returns structured data that can be rendered as HTML, PDF, or Excel."
  (let* ((site (get-site site-id))
         (site-code (getf site :|code|))
         (site-name (getf site :|name|))
         ;; Work order statistics
         (status-counts (count-work-orders-by-status site-id))
         (open-count (count-open-work-orders site-id))
         (opened-this-week (get-work-orders-opened-this-week site-id))
         (closed-this-week (get-work-orders-closed-this-week site-id))
         ;; Aging analysis
         (aging-0-7 (length (fetch-all 
                             "SELECT id FROM work_orders 
                              WHERE site_id = ? AND status != 'Closed'
                              AND julianday('now') - julianday(created_at) <= 7" site-id)))
         (aging-8-30 (length (fetch-all 
                              "SELECT id FROM work_orders 
                               WHERE site_id = ? AND status != 'Closed'
                               AND julianday('now') - julianday(created_at) > 7
                               AND julianday('now') - julianday(created_at) <= 30" site-id)))
         (aging-31-60 (length (fetch-all 
                               "SELECT id FROM work_orders 
                                WHERE site_id = ? AND status != 'Closed'
                                AND julianday('now') - julianday(created_at) > 30
                                AND julianday('now') - julianday(created_at) <= 60" site-id)))
         (aging-60-plus (length (fetch-all 
                                 "SELECT id FROM work_orders 
                                  WHERE site_id = ? AND status != 'Closed'
                                  AND julianday('now') - julianday(created_at) > 60" site-id)))
         ;; All open work orders with details
         (open-work-orders (fetch-all 
                            "SELECT wo.*, 
                                    s.code as site_code,
                                    f.code as facility_code, f.name as facility_name,
                                    a.name as asset_name,
                                    ROUND(julianday('now') - julianday(wo.created_at), 0) as age_days
                             FROM work_orders wo
                             JOIN sites s ON wo.site_id = s.id
                             LEFT JOIN facilities f ON wo.facility_id = f.id
                             LEFT JOIN assets a ON wo.asset_id = a.id
                             WHERE wo.site_id = ? AND wo.status != 'Closed'
                             ORDER BY 
                               CASE wo.priority 
                                 WHEN 'RAC1' THEN 1 
                                 WHEN 'RAC2' THEN 2 
                                 WHEN 'RAC3' THEN 3 
                                 ELSE 4 
                               END,
                               wo.created_at" site-id))
         ;; Inventory summary
         (inventory-summary (get-inventory-summary site-id))
         (issued-this-week (get-issued-this-week site-id))
         (low-stock (get-low-stock-items site-id))
         (burn-rates (get-burn-rate-summary site-id))
         ;; CLIN tracking
         (clin-summary (fetch-all 
                        "SELECT clin, 
                                COUNT(*) as wo_count,
                                SUM(COALESCE(actual_hours, 0)) as total_hours,
                                SUM(COALESCE(labor_cost, 0) + COALESCE(material_cost, 0)) as total_cost
                         FROM work_orders
                         WHERE site_id = ? AND clin IS NOT NULL
                         GROUP BY clin" site-id))
         ;; Report timestamp
         (report-date (local-time:format-timestring 
                       nil (local-time:now) 
                       :format '(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))))
    
    ;; Return structured report data
    (list :report-date report-date
          :site-code site-code
          :site-name site-name
          :summary (list :total-open open-count
                         :opened-this-week (length opened-this-week)
                         :closed-this-week (length closed-this-week)
                         :aging (list :days-0-7 aging-0-7
                                      :days-8-30 aging-8-30
                                      :days-31-60 aging-31-60
                                      :days-60-plus aging-60-plus))
          :status-breakdown status-counts
          :open-work-orders open-work-orders
          :opened-this-week opened-this-week
          :closed-this-week closed-this-week
          :inventory (list :summary inventory-summary
                           :issued-this-week issued-this-week
                           :low-stock low-stock
                           :burn-rates burn-rates)
          :clin-summary clin-summary)))

(defun format-report-text (report)
  "Format report as plain text for email/console output."
  (with-output-to-string (s)
    (format s "~%========================================~%")
    (format s "TF SAFE CMMS WEEKLY STATUS REPORT~%")
    (format s "========================================~%")
    (format s "Site: ~A (~A)~%" (getf report :site-name) (getf report :site-code))
    (format s "Report Date: ~A~%" (getf report :report-date))
    (format s "~%--- EXECUTIVE SUMMARY ---~%")
    (let ((summary (getf report :summary)))
      (format s "Total Open WOs: ~A~%" (getf summary :total-open))
      (format s "Opened This Week: ~A~%" (getf summary :opened-this-week))
      (format s "Closed This Week: ~A~%" (getf summary :closed-this-week))
      (let ((aging (getf summary :aging)))
        (format s "~%Aging:~%")
        (format s "  0-7 days: ~A~%" (getf aging :days-0-7))
        (format s "  8-30 days: ~A~%" (getf aging :days-8-30))
        (format s "  31-60 days: ~A~%" (getf aging :days-31-60))
        (format s "  60+ days: ~A~%" (getf aging :days-60-plus))))
    
    (format s "~%--- STATUS BREAKDOWN ---~%")
    (dolist (status-row (getf report :status-breakdown))
      (format s "  ~A: ~A~%" (getf status-row :|status|) (getf status-row :|count|)))
    
    (format s "~%--- OPEN WORK ORDERS ---~%")
    (format s "~A~%" (make-string 80 :initial-element #\-))
    (format s "~12A ~10A ~8A ~15A ~5A ~20A~%" 
            "WO#" "Priority" "Status" "Facility" "Age" "Blocker")
    (format s "~A~%" (make-string 80 :initial-element #\-))
    (dolist (wo (getf report :open-work-orders))
      (format s "~12A ~10A ~8A ~15A ~3Dd ~20A~%"
              (getf wo :|wo_number|)
              (getf wo :|priority|)
              (getf wo :|status|)
              (or (getf wo :|facility_code|) "-")
              (or (getf wo :|age_days|) 0)
              (or (getf wo :|blocker|) "-")))
    
    (format s "~%--- INVENTORY ISSUED THIS WEEK ---~%")
    (dolist (item (getf (getf report :inventory) :issued-this-week))
      (format s "  ~A: ~A ~A~%" 
              (getf item :|part_number|)
              (getf item :|qty_issued|)
              (getf item :|uom|)))
    
    (format s "~%--- LOW STOCK ALERTS ---~%")
    (let ((low-stock (getf (getf report :inventory) :low-stock)))
      (if low-stock
          (dolist (item low-stock)
            (format s "  ~A: ~A on-hand (min: ~A)~%"
                    (getf item :|part_number|)
                    (getf item :|qty_on_hand|)
                    (getf item :|min_qty|)))
          (format s "  No items below minimum stock level.~%")))
    
    (format s "~%========================================~%")))

(defun report-to-json (report)
  "Convert report to JSON string for API responses."
  (cl-json:encode-json-to-string report))
