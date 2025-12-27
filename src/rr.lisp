;;;; rr.lisp - Rest & Recuperation (R&R) Leave Management

(in-package #:tfs-cmms)

;;; ============================================================
;;; R&R Database Functions
;;; ============================================================

(defun get-active-contract-period ()
  "Get the currently active contract period."
  (fetch-one "SELECT * FROM contract_periods WHERE active = 1 ORDER BY start_date DESC LIMIT 1"))

(defun get-contract-period (id)
  "Get a contract period by ID."
  (fetch-one "SELECT * FROM contract_periods WHERE id = ?" id))

(defun get-staff-category-limit (category)
  "Get the max concurrent leave limit for a staff category."
  (fetch-one "SELECT * FROM staff_category_limits WHERE category = ?" category))

(defun get-user-rr-requests (user-id &key status)
  "Get R&R requests for a user, optionally filtered by status."
  (if status
      (fetch-all 
       "SELECT r.*, u.full_name as reviewer_name 
        FROM rr_requests r 
        LEFT JOIN users u ON r.reviewed_by = u.id
        WHERE r.user_id = ? AND r.status = ?
        ORDER BY r.start_date DESC" 
       user-id status)
      (fetch-all 
       "SELECT r.*, u.full_name as reviewer_name 
        FROM rr_requests r 
        LEFT JOIN users u ON r.reviewed_by = u.id
        WHERE r.user_id = ?
        ORDER BY r.start_date DESC" 
       user-id)))

(defun get-pending-rr-requests (&key category)
  "Get all pending R&R requests, optionally filtered by staff category."
  (if category
      (fetch-all 
       "SELECT r.*, u.full_name, u.staff_category,
               COALESCE(co.name || ' - ' || ca.name, u.current_location) as current_location
        FROM rr_requests r 
        JOIN users u ON r.user_id = u.id
        LEFT JOIN camps ca ON u.current_camp_id = ca.id
        LEFT JOIN countries co ON ca.country_id = co.id
        WHERE r.status = 'Pending' AND u.staff_category = ?
        ORDER BY r.requested_at ASC"
       category)
      (fetch-all 
       "SELECT r.*, u.full_name, u.staff_category,
               COALESCE(co.name || ' - ' || ca.name, u.current_location) as current_location
        FROM rr_requests r 
        JOIN users u ON r.user_id = u.id
        LEFT JOIN camps ca ON u.current_camp_id = ca.id
        LEFT JOIN countries co ON ca.country_id = co.id
        WHERE r.status = 'Pending'
        ORDER BY r.requested_at ASC")))

(defun get-approved-rr-in-range (start-date end-date &key category)
  "Get approved R&R requests that overlap with a date range."
  (if category
      (fetch-all 
       "SELECT r.*, u.full_name, u.staff_category
        FROM rr_requests r 
        JOIN users u ON r.user_id = u.id
        WHERE r.status = 'Approved' 
          AND u.staff_category = ?
          AND r.start_date <= ? AND r.end_date >= ?
        ORDER BY r.start_date"
       category end-date start-date)
      (fetch-all 
       "SELECT r.*, u.full_name, u.staff_category
        FROM rr_requests r 
        JOIN users u ON r.user_id = u.id
        WHERE r.status = 'Approved' 
          AND r.start_date <= ? AND r.end_date >= ?
        ORDER BY r.start_date"
       end-date start-date)))

(defun count-approved-on-date (date category)
  "Count how many staff of a category have approved leave on a specific date."
  (let ((result (fetch-one 
                 "SELECT COUNT(*) as cnt 
                  FROM rr_requests r 
                  JOIN users u ON r.user_id = u.id
                  WHERE r.status = 'Approved' 
                    AND u.staff_category = ?
                    AND r.start_date <= ? AND r.end_date >= ?"
                 category date date)))
    (or (getf result :|cnt|) 0)))

(defun get-max-approved-in-range (start-date end-date category)
  "Get the maximum number of approved leaves on any single day in a date range."
  (let ((max-count 0)
        (current-date (parse-date-string start-date)))
    (loop while (<= current-date (parse-date-string end-date))
          do (let ((count (count-approved-on-date (format-date-for-db current-date) category)))
               (when (> count max-count)
                 (setf max-count count)))
             (setf current-date (+ current-date 86400))) ; Add one day in seconds
    max-count))

(defun parse-date-string (date-str)
  "Parse YYYY-MM-DD string to universal time."
  (when (and date-str (>= (length date-str) 10))
    (let ((year (parse-integer (subseq date-str 0 4)))
          (month (parse-integer (subseq date-str 5 7)))
          (day (parse-integer (subseq date-str 8 10))))
      (encode-universal-time 0 0 12 day month year))))

(defun format-date-for-db (universal-time)
  "Format universal time as YYYY-MM-DD for database."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore sec min hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun format-date-display (date-str)
  "Format YYYY-MM-DD as DD-MMM-YYYY for display (e.g., 21-DEC-2025)."
  (if (and date-str (stringp date-str) (>= (length date-str) 10))
      (let* ((year (subseq date-str 0 4))
             (month-num (parse-integer (subseq date-str 5 7)))
             (day (subseq date-str 8 10))
             (month-abbrev (nth (1- month-num) '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" 
                                                  "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))))
        (format nil "~A-~A-~A" day month-abbrev year))
      (or date-str "-")))

(defun calculate-accrued-rr (bog-date-str)
  "Calculate accrued R&R days based on BOG date. 30 days per year."
  (when (and bog-date-str (>= (length bog-date-str) 10))
    (let* ((bog (parse-date-string bog-date-str))
           (now (get-universal-time))
           (days-employed (/ (- now bog) 86400.0))
           (accrued (* (/ days-employed 365.0) 30.0)))
      ;; Round to 1 decimal place
      (/ (round (* accrued 10)) 10.0))))

(defun calculate-rr-balance (user-id bog-date-str)
  "Calculate R&R balance (accrued minus used)."
  (let* ((accrued (or (calculate-accrued-rr bog-date-str) 0))
         (used-result (fetch-one 
                       "SELECT COALESCE(SUM(total_days), 0) as used 
                        FROM rr_requests 
                        WHERE user_id = ? AND status IN ('Approved', 'Completed')"
                       user-id))
         (used (or (getf used-result :|used|) 0)))
    (- accrued used)))

(defun is-in-probation-p (bog-date-str)
  "Check if user is still in 90-day probation period."
  (when bog-date-str
    (let* ((bog (parse-date-string bog-date-str))
           (probation-end (+ bog (* 90 86400)))
           (now (get-universal-time)))
      (< now probation-end))))

(defun get-probation-end-date (bog-date-str)
  "Get the date when probation ends (BOG + 90 days)."
  (when bog-date-str
    (let ((bog (parse-date-string bog-date-str)))
      (format-date-for-db (+ bog (* 90 86400))))))

(defun create-rr-request (user-id start-date end-date travel-to travel-from)
  "Create a new R&R request. Returns the new request ID or NIL on error."
  (let* ((start (parse-date-string start-date))
         (end (parse-date-string end-date))
         (total-days (1+ (round (/ (- end start) 86400))))
         (contract (get-active-contract-period)))
    (execute-sql 
     "INSERT INTO rr_requests (user_id, contract_period_id, start_date, end_date, total_days, travel_to, travel_from, status)
      VALUES (?, ?, ?, ?, ?, ?, ?, 'Pending')"
     user-id (when contract (getf contract :|id|)) start-date end-date total-days travel-to travel-from)
    (getf (fetch-one "SELECT last_insert_rowid() as id") :|id|)))

(defun approve-rr-request (request-id reviewer-id &optional comments)
  "Approve an R&R request."
  (execute-sql 
   "UPDATE rr_requests SET status = 'Approved', reviewed_by = ?, reviewed_at = datetime('now'), review_comments = ?
    WHERE id = ?"
   reviewer-id comments request-id))

(defun reject-rr-request (request-id reviewer-id comments)
  "Reject an R&R request."
  (execute-sql 
   "UPDATE rr_requests SET status = 'Rejected', reviewed_by = ?, reviewed_at = datetime('now'), review_comments = ?
    WHERE id = ?"
   reviewer-id comments request-id))

(defun get-rr-request (id)
  "Get a single R&R request by ID."
  (fetch-one 
   "SELECT r.*, u.full_name, u.staff_category, u.bog_date,
           COALESCE(co.name || ' - ' || ca.name, u.current_location) as current_location,
           rev.full_name as reviewer_name
    FROM rr_requests r 
    JOIN users u ON r.user_id = u.id
    LEFT JOIN camps ca ON u.current_camp_id = ca.id
    LEFT JOIN countries co ON ca.country_id = co.id
    LEFT JOIN users rev ON r.reviewed_by = rev.id
    WHERE r.id = ?" id))

(defun get-all-approved-rr-for-calendar (start-date end-date)
  "Get all approved R&R for calendar display."
  (fetch-all 
   "SELECT r.*, u.full_name, u.staff_category
    FROM rr_requests r 
    JOIN users u ON r.user_id = u.id
    WHERE r.status = 'Approved' 
      AND r.start_date <= ? AND r.end_date >= ?
    ORDER BY r.start_date"
   end-date start-date))

(defun find-next-available-slot (category start-from-date &optional (days-needed 17))
  "Find the next available date range where fewer than max staff are on leave."
  (let* ((limit-info (get-staff-category-limit category))
         (max-concurrent (or (getf limit-info :|max_concurrent_leave|) 3))
         (current-date (parse-date-string start-from-date))
         (max-search-days 180)) ; Search up to 6 months ahead
    (loop for day-offset from 0 below max-search-days
          for check-date = (+ current-date (* day-offset 86400))
          for date-str = (format-date-for-db check-date)
          when (< (count-approved-on-date date-str category) max-concurrent)
          do (let ((end-date (format-date-for-db (+ check-date (* (1- days-needed) 86400)))))
               ;; Check if entire range is available
               (when (< (get-max-approved-in-range date-str end-date category) max-concurrent)
                 (return (values date-str end-date))))
          finally (return nil))))

;;; ============================================================
;;; R&R Page Handlers
;;; ============================================================

(defun handle-rr-dashboard ()
  "R&R dashboard - redirects admins to calendar, shows personal dashboard for electricians."
  (let* ((user (get-current-user))
         (user-role (getf user :|role|))
         (staff-category (getf user :|staff_category|)))
    ;; Redirect admins/managers to calendar view
    (when (or (user-is-admin-p user)
              (string= user-role "ao_lead")
              (string= user-role "program_manager")
              (string= user-role "pmo"))
      (return-from handle-rr-dashboard (redirect-to "/rr/calendar")))
    
    ;; Personal dashboard for electricians
    (let* ((user-id (getf user :|id|))
           (bog-date (getf user :|bog_date|))
           (category (or staff-category "Electrician"))
           (accrued (calculate-accrued-rr bog-date))
           (balance (calculate-rr-balance user-id bog-date))
           (in-probation (is-in-probation-p bog-date))
           (probation-end (get-probation-end-date bog-date))
           (pending-requests (get-user-rr-requests user-id :status "Pending"))
           (approved-requests (get-user-rr-requests user-id :status "Approved"))
           (past-requests (fetch-all 
                           "SELECT r.*, u.full_name as reviewer_name 
                            FROM rr_requests r 
                            LEFT JOIN users u ON r.reviewed_by = u.id
                            WHERE r.user_id = ? AND r.status IN ('Completed', 'Rejected')
                            ORDER BY r.start_date DESC"
                           user-id))
           (limit-info (get-staff-category-limit category))
           (max-concurrent (or (getf limit-info :|max_concurrent_leave|) 3)))
      (html-response
       (render-page "R&R Leave Management"
         (cl-who:with-html-output-to-string (s)
           (:div :class "page-header"
             (:h1 "R&R Leave Management")
             (:p :class "text-muted" "Rest & Recuperation Leave Requests"))
         
         ;; Status Cards
         (:div :style "display: grid; grid-template-columns: repeat(4, 1fr); gap: 1rem; margin-bottom: 1.5rem;"
           (:div :class "card" :style "text-align: center; padding: 1rem;"
             (:div :style "font-size: 2rem; font-weight: bold;" (cl-who:fmt "~,1F" (or accrued 0)))
             (:div :style "color: #666; font-size: 0.9rem;" "Days Accrued"))
           (:div :class "card" :style "text-align: center; padding: 1rem;"
             (:div :style (format nil "font-size: 2rem; font-weight: bold;~A" 
                                  (if (and balance (< balance 0)) " color: red;" ""))
                   (cl-who:fmt "~,1F" (or balance 0)))
             (:div :style "color: #666; font-size: 0.9rem;" "Days Balance"))
           (:div :class "card" :style "text-align: center; padding: 1rem;"
             (:div :style "font-size: 2rem; font-weight: bold;" (cl-who:str (length pending-requests)))
             (:div :style "color: #666; font-size: 0.9rem;" "Pending Requests"))
           (:div :class "card" :style "text-align: center; padding: 1rem;"
             (:div :style "font-size: 2rem; font-weight: bold;" (cl-who:str (length approved-requests)))
             (:div :style "color: #666; font-size: 0.9rem;" "Approved Upcoming")))
         
         ;; Probation Warning
         (when in-probation
           (cl-who:htm
            (:div :class "alert alert-warning"
              (:strong "Probation Period: ")
              "You are currently in your 90-day probation period. "
              "R&R requests cannot be submitted until after "
              (:strong (cl-who:str probation-end)) ".")))
         
         ;; New Request Form
         (unless in-probation
           (cl-who:htm
            (:section :class "card"
              (:h2 "Submit New R&R Request")
              (:p :class "text-muted" 
                  "R&R allowance is 15 days plus 1 travel day each way (17 days total). "
                  "Maximum " (cl-who:str max-concurrent) " staff may be on leave simultaneously.")
              (:form :method "post" :action "/api/rr/request" :id "rr-request-form"
                (:div :class "form-row"
                  (:div :class "form-group"
                    (:label "Start Date (including travel) *")
                    (:input :type "date" :name "start_date" :required t
                            :id "rr-start-date"
                            :min (format-date-for-db (get-universal-time))))
                  (:div :class "form-group"
                    (:label "End Date (including travel) *")
                    (:input :type "date" :name "end_date" :required t
                            :id "rr-end-date")))
                (:div :class "form-row"
                  (:div :class "form-group"
                    (:label "Traveling To (Destination) *")
                    (:input :type "text" :name "travel_to" :required t
                            :placeholder "e.g., London, UK"))
                  (:div :class "form-group"
                    (:label "Returning From *")
                    (:input :type "text" :name "travel_from" :required t
                            :placeholder "e.g., London, UK")))
                (:div :id "date-validation-message" :style "margin-bottom: 1rem;")
                (:button :type "submit" :class "btn btn-primary" "Submit R&R Request"))
              (:script "
// R&R Date Validation
(function() {
  var startInput = document.getElementById('rr-start-date');
  var endInput = document.getElementById('rr-end-date');
  var msgDiv = document.getElementById('date-validation-message');
  var form = document.getElementById('rr-request-form');
  
  // Set minimum end date when start date changes
  startInput.addEventListener('change', function() {
    if (this.value) {
      endInput.min = this.value;
      // If end date is before start date, clear it
      if (endInput.value && endInput.value < this.value) {
        endInput.value = '';
      }
    }
    validateDates();
  });
  
  endInput.addEventListener('change', validateDates);
  
  function validateDates() {
    var start = startInput.value;
    var end = endInput.value;
    msgDiv.innerHTML = '';
    
    if (start && end) {
      var startDate = new Date(start);
      var endDate = new Date(end);
      
      if (endDate < startDate) {
        msgDiv.innerHTML = '<p style=\"color: red;\">⚠ End date must be after start date</p>';
        return false;
      }
      
      var days = Math.ceil((endDate - startDate) / (1000 * 60 * 60 * 24)) + 1;
      if (days > 17) {
        msgDiv.innerHTML = '<p style=\"color: orange;\">⚠ R&R request cannot exceed 17 days (currently ' + days + ' days)</p>';
        return false;
      }
      
      msgDiv.innerHTML = '<p style=\"color: green;\">✓ ' + days + ' day(s) requested</p>';
    }
    return true;
  }
  
  form.addEventListener('submit', function(e) {
    if (!validateDates()) {
      e.preventDefault();
      alert('Please correct the date errors before submitting.');
    }
  });
})();
"))))
         
         ;; Who's On Leave Calendar Preview (visible to all users including those in probation)
         (:section :class "card"
           (:h2 "Who's On Leave")
           (:p :class "text-muted" "Select a month to see who is already scheduled for R&R.")
           (:div :class "form-row" :style "margin-bottom: 1rem;"
             (:select :id "leave-month-select" :class "form-control" :style "max-width: 200px;"
               :onchange "loadLeaveCalendar(this.value)"
               (:option :value "" "-- Select Month --")
               (let ((now (get-decoded-time)))
                 (declare (ignore now))
                 (multiple-value-bind (sec min hour day month year) (get-decoded-time)
                   (declare (ignore sec min hour day))
                   (loop for i from 0 to 11
                         for m = (1+ (mod (+ month i -1) 12))
                         for y = (if (> (+ month i -1) 12) (1+ year) year)
                         do (cl-who:htm
                             (:option :value (format nil "~4,'0D-~2,'0D" y m)
                                      (cl-who:str (format nil "~A ~D" (get-month-name m) y)))))))))
           (:div :id "leave-calendar-preview" :style "min-height: 100px;")
           (:script "
function loadLeaveCalendar(monthStr) {
  if (!monthStr) {
    document.getElementById('leave-calendar-preview').innerHTML = '';
    return;
  }
  fetch('/api/rr/month-leave?month=' + monthStr)
    .then(r => r.json())
    .then(data => {
      let html = '';
      if (data.length === 0) {
        html = '<p style=\"color: green;\">✓ No one scheduled for R&R this month</p>';
      } else {
        html = '<table class=\"data-table\"><thead><tr><th>Name</th><th>Dates</th><th>Days</th></tr></thead><tbody>';
        data.forEach(r => {
          html += '<tr><td>' + r.name + '</td><td>' + r.start_date + ' to ' + r.end_date + '</td><td>' + r.days + '</td></tr>';
        });
        html += '</tbody></table>';
        html += '<p style=\"margin-top: 0.5rem; color: #666;\">Total: ' + data.length + ' person(s) on leave</p>';
      }
      document.getElementById('leave-calendar-preview').innerHTML = html;
    })
    .catch(e => {
      document.getElementById('leave-calendar-preview').innerHTML = '<p style=\"color: red;\">Error loading calendar</p>';
    });
}
"))
         
         ;; Pending Requests
         (when pending-requests
           (cl-who:htm
            (:section :class "card"
              (:h2 "Pending Requests")
              (:table :class "data-table"
                (:thead
                  (:tr (:th "Dates") (:th "Days") (:th "Destination") (:th "Status") (:th "Submitted") (:th "Actions")))
                (:tbody
                  (dolist (req pending-requests)
                    (cl-who:htm
                     (:tr
                       (:td (cl-who:str (format nil "~A to ~A" 
                                                (format-date-display (getf req :|start_date|)) 
                                                (format-date-display (getf req :|end_date|)))))
                       (:td (cl-who:str (getf req :|total_days|)))
                       (:td (cl-who:str (or (getf req :|travel_to|) "-")))
                       (:td (:span :class "badge badge-warning" "Pending"))
                       (:td (cl-who:str (format-date-display (subseq (or (getf req :|requested_at|) "") 0 10))))
                       (:td 
                         (:form :method "post" :action (format nil "/api/rr/~A/cancel" (getf req :|id|))
                                :style "display: inline;"
                                :onsubmit "return confirm('Are you sure you want to cancel this R&R request?');"
                           (:button :type "submit" :class "btn btn-sm btn-danger" "Cancel")))))))))))
         
         ;; Approved Upcoming
         (when approved-requests
           (cl-who:htm
            (:section :class "card"
              (:h2 "Approved Upcoming R&R")
              (:table :class "data-table"
                (:thead
                  (:tr (:th "Dates") (:th "Days") (:th "Destination") (:th "Approved By")))
                (:tbody
                  (dolist (req approved-requests)
                    (cl-who:htm
                     (:tr
                       (:td (cl-who:str (format nil "~A to ~A" 
                                                (format-date-display (getf req :|start_date|)) 
                                                (format-date-display (getf req :|end_date|)))))
                       (:td (cl-who:str (getf req :|total_days|)))
                       (:td (cl-who:str (or (getf req :|travel_to|) "-")))
                       (:td (cl-who:str (or (getf req :|reviewer_name|) "-")))))))))))
         
         ;; Past Requests History
         (:section :class "card"
           (:h2 "R&R History")
           (if past-requests
               (cl-who:htm
                (:table :class "data-table"
                  (:thead
                    (:tr (:th "Dates") (:th "Days") (:th "Destination") (:th "Status") (:th "Reviewed By")))
                  (:tbody
                    (dolist (req past-requests)
                      (cl-who:htm
                       (:tr
                         (:td (cl-who:str (format nil "~A to ~A" 
                                                  (format-date-display (getf req :|start_date|)) 
                                                  (format-date-display (getf req :|end_date|)))))
                         (:td (cl-who:str (getf req :|total_days|)))
                         (:td (cl-who:str (or (getf req :|travel_to|) "-")))
                         (:td (:span :class (format nil "badge badge-~A" 
                                                    (if (string= (getf req :|status|) "Completed")
                                                        "success" "danger"))
                                     (cl-who:str (getf req :|status|))))
                         (:td (cl-who:str (or (getf req :|reviewer_name|) "-")))))))))
               (cl-who:htm
                (:p :class "text-muted" "No previous R&R requests."))))))))))

(defun handle-rr-approval-queue ()
  "R&R approval queue for AO Lead only (Program Manager is read-only)."
  (let* ((user (get-current-user))
         (user-role (getf user :|role|))
         (can-approve-electricians (or (string= user-role "ao_lead") 
                                       (string= user-role "Admin")))
         (can-approve-pmo-qc (string= user-role "Admin"))
         (electrician-requests (when can-approve-electricians
                                 (get-pending-rr-requests :category "Electrician")))
         (pmo-requests (when can-approve-pmo-qc
                         (get-pending-rr-requests :category "PMO")))
         (qc-requests (when can-approve-pmo-qc
                        (get-pending-rr-requests :category "QC")))
         (limit-elec (get-staff-category-limit "Electrician"))
         (max-elec (or (getf limit-elec :|max_concurrent_leave|) 3)))
    (if (or can-approve-electricians can-approve-pmo-qc)
        (html-response
         (render-page "R&R Approval Queue"
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 "R&R Approval Queue")
               (:a :href "/rr/calendar" :class "btn" "View Calendar"))
             
             ;; Electrician Requests
             (when can-approve-electricians
               (cl-who:htm
                (:section :class "card"
                  (:h2 (cl-who:str (format nil "Electrician Requests (~A pending)" 
                                           (length electrician-requests))))
                  (:p :class "text-muted" 
                      "Maximum " (cl-who:str max-elec) " electricians may be on leave at any time.")
                  (if electrician-requests
                      (cl-who:htm
                       (:table :class "data-table"
                         (:thead
                           (:tr (:th "Name") (:th "Location") (:th "Dates") (:th "Days") 
                                (:th "Destination") (:th "Conflicts") (:th "Actions")))
                         (:tbody
                           (dolist (req electrician-requests)
                             (let* ((start (getf req :|start_date|))
                                    (end (getf req :|end_date|))
                                    (conflicts (get-max-approved-in-range start end "Electrician")))
                               (cl-who:htm
                                (:tr :class (if (>= conflicts max-elec) "row-warning" "")
                                  (:td (cl-who:str (getf req :|full_name|)))
                                  (:td (cl-who:str (or (getf req :|current_location|) "-")))
                                  (:td (cl-who:str (format nil "~A to ~A" (format-date-display start) (format-date-display end))))
                                  (:td (cl-who:str (getf req :|total_days|)))
                                  (:td (cl-who:str (or (getf req :|travel_to|) "-")))
                                  (:td 
                                    (if (>= conflicts max-elec)
                                        (cl-who:htm 
                                         (:span :class "badge badge-danger" 
                                                (cl-who:str (format nil "~A already approved" conflicts))))
                                        (cl-who:htm
                                         (:span :class "badge badge-success"
                                                (cl-who:str (format nil "~A/~A slots used" conflicts max-elec))))))
                                  (:td
                                    (:form :method "post" :action (format nil "/api/rr/~A/approve" (getf req :|id|))
                                           :style "display: inline;"
                                      (:button :type "submit" :class "btn btn-sm btn-success" "Approve"))
                                    " "
                                    (:button :type "button" :class "btn btn-sm btn-danger"
                                             :onclick (format nil "showRejectModal(~A)" (getf req :|id|))
                                             "Reject")))))))))
                      (cl-who:htm
                       (:p :class "text-muted" "No pending electrician requests."))))))
             
             ;; PMO Requests
             (when (and can-approve-pmo-qc pmo-requests)
               (cl-who:htm
                (:section :class "card"
                  (:h2 "PMO Requests")
                  (:p :class "text-muted" "Maximum 1 PMO staff may be on leave at any time.")
                  (:table :class "data-table"
                    (:thead
                      (:tr (:th "Name") (:th "Dates") (:th "Days") (:th "Destination") (:th "Actions")))
                    (:tbody
                      (dolist (req pmo-requests)
                        (cl-who:htm
                         (:tr
                           (:td (cl-who:str (getf req :|full_name|)))
                           (:td (cl-who:str (format nil "~A to ~A" 
                                                    (format-date-display (getf req :|start_date|)) 
                                                    (format-date-display (getf req :|end_date|)))))
                           (:td (cl-who:str (getf req :|total_days|)))
                           (:td (cl-who:str (or (getf req :|travel_to|) "-")))
                           (:td
                             (:form :method "post" :action (format nil "/api/rr/~A/approve" (getf req :|id|))
                                    :style "display: inline;"
                               (:button :type "submit" :class "btn btn-sm btn-success" "Approve"))
                             " "
                             (:button :type "button" :class "btn btn-sm btn-danger"
                                      :onclick (format nil "showRejectModal(~A)" (getf req :|id|))
                                      "Reject"))))))))))
             
             ;; QC Requests
             (when (and can-approve-pmo-qc qc-requests)
               (cl-who:htm
                (:section :class "card"
                  (:h2 "QC Requests")
                  (:p :class "text-muted" "Maximum 1 QC staff may be on leave at any time.")
                  (:table :class "data-table"
                    (:thead
                      (:tr (:th "Name") (:th "Dates") (:th "Days") (:th "Destination") (:th "Actions")))
                    (:tbody
                      (dolist (req qc-requests)
                        (cl-who:htm
                         (:tr
                           (:td (cl-who:str (getf req :|full_name|)))
                           (:td (cl-who:str (format nil "~A to ~A" 
                                                    (format-date-display (getf req :|start_date|)) 
                                                    (format-date-display (getf req :|end_date|)))))
                           (:td (cl-who:str (getf req :|total_days|)))
                           (:td (cl-who:str (or (getf req :|travel_to|) "-")))
                           (:td
                             (:form :method "post" :action (format nil "/api/rr/~A/approve" (getf req :|id|))
                                    :style "display: inline;"
                               (:button :type "submit" :class "btn btn-sm btn-success" "Approve"))
                             " "
                             (:button :type "button" :class "btn btn-sm btn-danger"
                                      :onclick (format nil "showRejectModal(~A)" (getf req :|id|))
                                      "Reject"))))))))))
             
             ;; Reject Modal
             (:div :id "reject-modal" :class "modal" :style "display: none;"
               (:div :class "modal-content"
                 (:h3 "Reject R&R Request")
                 (:form :method "post" :id "reject-form"
                   (:div :class "form-group"
                     (:label "Reason for Rejection *")
                     (:textarea :name "comments" :required t :rows "3"
                                :placeholder "Please provide a reason for rejection..."))
                   (:div :class "form-actions"
                     (:button :type "submit" :class "btn btn-danger" "Reject Request")
                     (:button :type "button" :class "btn" :onclick "hideRejectModal()" "Cancel")))))
             
             (:script "
function showRejectModal(requestId) {
  document.getElementById('reject-form').action = '/api/rr/' + requestId + '/reject';
  document.getElementById('reject-modal').style.display = 'flex';
}
function hideRejectModal() {
  document.getElementById('reject-modal').style.display = 'none';
}
"))))
        (redirect-to "/unauthorized"))))

(defun get-all-rr-for-period (start-date end-date)
  "Get all R&R requests (any status) that overlap with a date range."
  (fetch-all 
   "SELECT r.*, u.full_name, u.staff_category, u.electrician_type
    FROM rr_requests r 
    JOIN users u ON r.user_id = u.id
    WHERE r.start_date <= ? AND r.end_date >= ?
    ORDER BY r.start_date, u.full_name"
   end-date start-date))

(defun get-month-name (month-num)
  "Get month name from number."
  (nth (1- month-num) '("January" "February" "March" "April" "May" "June" 
                        "July" "August" "September" "October" "November" "December")))

(defun handle-rr-calendar ()
  "Calendar view of all R&R leave with month filter and travel details.
   Only accessible by Admin, AO Lead, Program Manager, PMO."
  (let* ((user (get-current-user))
         (user-role (getf user :|role|))
         (is-admin (or (user-is-admin-p user)
                       (string= user-role "ao_lead")
                       (string= user-role "program_manager")
                       (string= user-role "pmo"))))
    ;; Restrict access to admins/managers only
    (unless is-admin
      (return-from handle-rr-calendar (redirect-to "/rr")))
    (let* ((month-param (hunchentoot:parameter "month"))
           (year-param (hunchentoot:parameter "year"))
           (now (get-universal-time)))
      (multiple-value-bind (sec min hour day month year)
        (decode-universal-time now)
      (declare (ignore sec min hour day))
      (let* ((selected-year (if year-param (parse-integer year-param :junk-allowed t) year))
             (selected-month (if month-param (parse-integer month-param :junk-allowed t) month))
             ;; Calculate date range for selected month
             (start-date (format nil "~4,'0D-~2,'0D-01" selected-year selected-month))
             (end-date (format nil "~4,'0D-~2,'0D-~2,'0D" selected-year selected-month
                               (if (member selected-month '(4 6 9 11)) 30
                                   (if (= selected-month 2) 28 31))))
             (all-rr (get-all-rr-for-period start-date end-date))
             (month-name (get-month-name selected-month)))
        (html-response
         (render-page (format nil "R&R Schedule - ~A ~A" month-name selected-year)
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header" :style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 1rem;"
               (:h1 :style "margin: 0;" (cl-who:str (format nil "R&R Schedule - ~A ~A" month-name selected-year)))
               (:div
                 (:a :href (format nil "/rr/calendar/print?month=~A&year=~A" selected-month selected-year)
                     :class "btn btn-primary" :target "_blank" "Print for Admin")
                 (:a :href "/rr/approve" :class "btn" :style "margin-left: 0.5rem;" "Approval Queue")))
             
             ;; Month Selector
             (:div :class "card" :style "padding: 1rem; margin-bottom: 1rem;"
               (:form :method "get" :action "/rr/calendar" :style "display: flex; gap: 1rem; align-items: center;"
                 (:div :class "form-group" :style "margin: 0;"
                   (:label :style "margin-right: 0.5rem;" "Month:")
                   (:select :name "month" :class "status-select"
                     (loop for m from 1 to 12
                           do (cl-who:htm
                               (:option :value m :selected (= m selected-month)
                                        (cl-who:str (get-month-name m)))))))
                 (:div :class "form-group" :style "margin: 0;"
                   (:label :style "margin-right: 0.5rem;" "Year:")
                   (:select :name "year" :class "status-select"
                     (loop for y from 2024 to 2027
                           do (cl-who:htm
                               (:option :value y :selected (= y selected-year)
                                        (cl-who:str y))))))
                 (:button :type "submit" :class "btn" "View Month")))
             
             ;; Legend
             (:div :class "rr-legend" :style "display: flex; gap: 1.5rem; margin-bottom: 1rem; font-size: 0.9rem;"
               (:span (:span :class "legend-box master") " Master Electrician")
               (:span (:span :class "legend-box journeyman") " Journeyman")
               (:span (:span :class "legend-box pending") " Pending Approval"))
             
             ;; R&R Table with Travel Details
             (:section :class "card" :style "padding: 1rem;"
               (:h2 :style "margin-top: 0;" (cl-who:str (format nil "R&R for ~A ~A (~A requests)" month-name selected-year (length all-rr))))
               (if all-rr
                   (cl-who:htm
                    (:table :class "gantt-table"
                      (:thead
                        (:tr
                          (:th "Name")
                          (:th "Type")
                          (:th "Start Date")
                          (:th "End Date")
                          (:th "Days")
                          (:th "Flying To")
                          (:th "Returning From")
                          (:th "Status")
                          (when is-admin
                            (cl-who:htm (:th "Actions")))))
                      (:tbody
                        (dolist (rr all-rr)
                          (let* ((elec-type (or (getf rr :|electrician_type|) ""))
                                 (status (getf rr :|status|))
                                 (row-class (cond
                                              ((string= status "Pending") "rr-pending")
                                              ((string= elec-type "Master") "rr-master")
                                              (t "rr-journeyman"))))
                            (cl-who:htm
                             (:tr :class row-class
                               (:td (if is-admin
                                        (cl-who:htm (:a :href (format nil "/rr/~A/edit" (getf rr :|id|)) 
                                                        (cl-who:str (getf rr :|full_name|))))
                                        (cl-who:htm (cl-who:str (getf rr :|full_name|)))))
                               (:td (cl-who:str (or elec-type "-")))
                               (:td (cl-who:str (format-date-display (getf rr :|start_date|))))
                               (:td (cl-who:str (format-date-display (getf rr :|end_date|))))
                               (:td (cl-who:str (getf rr :|total_days|)))
                               (:td (cl-who:str (or (getf rr :|travel_to|) "-")))
                               (:td (cl-who:str (or (getf rr :|travel_from|) "-")))
                               (:td (:span :class (format nil "badge badge-~A"
                                                          (cond ((string= status "Approved") "success")
                                                                ((string= status "Pending") "warning")
                                                                ((string= status "Rejected") "danger")
                                                                (t "secondary")))
                                           (cl-who:str status)))
                               (when is-admin
                                 (cl-who:htm
                                  (:td
                                    (:form :method "post" :action (format nil "/api/rr/~A/status" (getf rr :|id|))
                                           :style "display: inline;"
                                      (:select :name "status" :onchange "this.form.submit()" :class "status-select"
                                        (:option :value "Pending" :selected (string= status "Pending") "Pending")
                                        (:option :value "Approved" :selected (string= status "Approved") "Approved")
                                        (:option :value "Rejected" :selected (string= status "Rejected") "Rejected")
                                        (:option :value "Completed" :selected (string= status "Completed") "Completed")))))))))))))
                   (cl-who:htm
                    (:p :class "text-muted" "No R&R scheduled for this month."))))
             
             ;; Capacity Summary for the month
             (:section :class "card" :style "padding: 1rem;"
               (:h2 :style "margin-top: 0;" "Daily Capacity")
               (:div :class "capacity-grid"
                 (let ((month-start (parse-date-string start-date))
                       (month-end (parse-date-string end-date)))
                   (loop for date-time = month-start then (+ date-time 86400)
                         while (<= date-time month-end)
                         for date-str = (format-date-for-db date-time)
                         for count = (count-approved-on-date date-str "Electrician")
                         do (cl-who:htm
                             (:div :class (format nil "capacity-day ~A" 
                                                  (cond ((>= count 3) "cap-full")
                                                        ((= count 2) "cap-warn")
                                                        ((= count 1) "cap-ok")
                                                        (t "cap-empty")))
                                   :title (format nil "~A: ~A on leave" date-str count)
                               (:span :class "cap-date" (cl-who:str (subseq date-str 8)))
                               (:span :class "cap-count" (cl-who:str count))))))))))))))))

(defun handle-rr-calendar-print ()
  "Printable R&R schedule for admin to book tickets.
   Shows: 1) People departing this month (start_date in month)
          2) People returning this month who left previous month (end_date in month, start_date before month)"
  (let* ((month-param (hunchentoot:parameter "month"))
         (year-param (hunchentoot:parameter "year"))
         (now (get-universal-time)))
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time now)
      (declare (ignore sec min hour day))
      (let* ((selected-year (if year-param (parse-integer year-param :junk-allowed t) year))
             (selected-month (if month-param (parse-integer month-param :junk-allowed t) month))
             (start-date (format nil "~4,'0D-~2,'0D-01" selected-year selected-month))
             (end-date (format nil "~4,'0D-~2,'0D-~2,'0D" selected-year selected-month
                               (if (member selected-month '(4 6 9 11)) 30
                                   (if (= selected-month 2) 28 31))))
             ;; Get people DEPARTING this month (start_date within month)
             (departing-rr (fetch-all 
                            "SELECT r.*, u.full_name, u.electrician_type, 
                                    COALESCE(co.name || ' - ' || ca.name, u.current_location) as current_location,
                                    'departing' as booking_type
                             FROM rr_requests r 
                             JOIN users u ON r.user_id = u.id
                             LEFT JOIN camps ca ON u.current_camp_id = ca.id
                             LEFT JOIN countries co ON ca.country_id = co.id
                             WHERE r.status = 'Approved'
                               AND r.start_date >= ? AND r.start_date <= ?
                             ORDER BY r.start_date, u.full_name"
                            start-date end-date))
             ;; Get people RETURNING this month who left BEFORE this month
             (returning-rr (fetch-all 
                            "SELECT r.*, u.full_name, u.electrician_type, 
                                    COALESCE(co.name || ' - ' || ca.name, u.current_location) as current_location,
                                    'returning' as booking_type
                             FROM rr_requests r 
                             JOIN users u ON r.user_id = u.id
                             LEFT JOIN camps ca ON u.current_camp_id = ca.id
                             LEFT JOIN countries co ON ca.country_id = co.id
                             WHERE r.status = 'Approved'
                               AND r.end_date >= ? AND r.end_date <= ?
                               AND r.start_date < ?
                             ORDER BY r.end_date, u.full_name"
                            start-date end-date start-date))
             (month-name (get-month-name selected-month)))
        (hunchentoot:no-cache)
        (html-response
         (cl-who:with-html-output-to-string (s nil :prologue t)
           (:html
             (:head
               (:title (cl-who:str (format nil "R&R Schedule - ~A ~A" month-name selected-year)))
               (:style "
@media print {
  body { margin: 0; padding: 20px; }
  .no-print { display: none; }
}
body { font-family: Arial, sans-serif; font-size: 11pt; max-width: 1000px; margin: 0 auto; padding: 20px; }
h1 { text-align: center; margin-bottom: 5px; }
h2 { text-align: center; color: #666; font-size: 14pt; margin-top: 0; }
table { width: 100%; border-collapse: collapse; margin-top: 20px; }
th, td { border: 1px solid #333; padding: 8px; text-align: left; }
th { background: #f0f0f0; font-weight: bold; }
.master { background-color: rgba(37, 99, 235, 0.15); }
.journeyman { background-color: rgba(22, 163, 74, 0.15); }
.print-btn { padding: 10px 20px; font-size: 14pt; cursor: pointer; margin-bottom: 20px; }
.header-info { text-align: center; margin-bottom: 20px; }
"))
             (:body
               (:div :class "no-print" :style "text-align: center; margin-bottom: 20px;"
                 (:button :class "print-btn" :onclick "window.print()" "Print This Page")
                 (:a :href (format nil "/rr/calendar?month=~A&year=~A" selected-month selected-year)
                     :style "margin-left: 20px;" "Back to Calendar"))
               
               (:h1 "R&R Travel Schedule")
               (:h2 (cl-who:str (format nil "~A ~A" month-name selected-year)))
               (:div :class "header-info"
                 (:p "For Admin Use - Flight Booking Reference")
                 (:p (cl-who:str (format nil "Generated: ~A" (format-date-for-db (get-universal-time))))))
               
               ;; OUTBOUND FLIGHTS - People departing this month
               (:h3 :style "margin-top: 30px; border-bottom: 2px solid #333; padding-bottom: 5px;"
                    (cl-who:str (format nil "OUTBOUND FLIGHTS - Departing in ~A (~A)" month-name (length departing-rr))))
               (if departing-rr
                   (cl-who:htm
                    (:table
                      (:thead
                        (:tr
                          (:th "Name")
                          (:th "Type")
                          (:th "Departing From")
                          (:th "Depart Date")
                          (:th "Flying To")
                          (:th "Return Date")))
                      (:tbody
                        (dolist (rr departing-rr)
                          (let ((elec-type (or (getf rr :|electrician_type|) "")))
                            (cl-who:htm
                             (:tr :class (if (string= elec-type "Master") "master" "journeyman")
                               (:td (cl-who:str (getf rr :|full_name|)))
                               (:td (cl-who:str (or elec-type "-")))
                               (:td (cl-who:str (or (getf rr :|current_location|) "-")))
                               (:td (:strong (cl-who:str (format-date-display (getf rr :|start_date|)))))
                               (:td (cl-who:str (or (getf rr :|travel_to|) "TBD")))
                               (:td (cl-who:str (format-date-display (getf rr :|end_date|)))))))))))
                   (cl-who:htm
                    (:p :style "color: #666; margin: 10px 0;" "No outbound flights this month.")))
               
               ;; RETURN FLIGHTS - People returning this month (who left before this month)
               (:h3 :style "margin-top: 30px; border-bottom: 2px solid #333; padding-bottom: 5px;"
                    (cl-who:str (format nil "RETURN FLIGHTS - Returning in ~A (~A)" month-name (length returning-rr))))
               (if returning-rr
                   (cl-who:htm
                    (:table
                      (:thead
                        (:tr
                          (:th "Name")
                          (:th "Type")
                          (:th "Returning From")
                          (:th "Return Date")
                          (:th "Flying To (Work Location)")
                          (:th "Originally Departed")))
                      (:tbody
                        (dolist (rr returning-rr)
                          (let ((elec-type (or (getf rr :|electrician_type|) "")))
                            (cl-who:htm
                             (:tr :class (if (string= elec-type "Master") "master" "journeyman")
                               (:td (cl-who:str (getf rr :|full_name|)))
                               (:td (cl-who:str (or elec-type "-")))
                               (:td (cl-who:str (or (getf rr :|travel_from|) "TBD")))
                               (:td (:strong (cl-who:str (format-date-display (getf rr :|end_date|)))))
                               (:td (cl-who:str (or (getf rr :|current_location|) "-")))
                               (:td :style "color: #666;" (cl-who:str (format-date-display (getf rr :|start_date|)))))))))))
                   (cl-who:htm
                    (:p :style "color: #666; margin: 10px 0;" "No return flights this month.")))
               
               (:div :style "margin-top: 40px; border-top: 1px solid #ccc; padding-top: 20px;"
                 (:p (:strong "Notes:"))
                 (:ul
                   (:li "TBD = Travel destination not yet specified by employee")
                   (:li "Depart Date includes 1 travel day")
                   (:li "Return Date includes 1 travel day")
                   (:li "Total R&R allowance: 15 days + 2 travel days = 17 days")))))))))))

(defun handle-api-rr-status-update (id-str)
  "Update R&R request status - Admin/AO Lead only."
  (let* ((user (get-current-user))
         (is-admin (or (user-is-admin-p user)
                       (string= (getf user :|role|) "ao_lead")))
         (request-id (parse-integer id-str :junk-allowed t))
         (new-status (hunchentoot:parameter "status")))
    (if is-admin
        (progn
          (execute-sql "UPDATE rr_requests SET status = ? WHERE id = ?" new-status request-id)
          (redirect-to "/rr/calendar"))
        (redirect-to "/unauthorized"))))

;;; ============================================================
;;; R&R API Handlers
;;; ============================================================

(defun handle-api-rr-request ()
  "Handle new R&R request submission."
  (let* ((user (get-current-user))
         (user-id (getf user :|id|))
         (bog-date (getf user :|bog_date|))
         (category (or (getf user :|staff_category|) "Electrician"))
         (start-date (hunchentoot:parameter "start_date"))
         (end-date (hunchentoot:parameter "end_date"))
         (travel-to (hunchentoot:parameter "travel_to"))
         (travel-from (hunchentoot:parameter "travel_from")))
    
    ;; Validate probation
    (when (is-in-probation-p bog-date)
      (return-from handle-api-rr-request
        (redirect-to "/rr?error=You%20are%20still%20in%20probation%20period")))
    
    ;; Check for existing pending request (limit to one at a time)
    (let ((pending (fetch-one 
                    "SELECT id FROM rr_requests WHERE user_id = ? AND status = 'Pending'"
                    user-id)))
      (when pending
        (return-from handle-api-rr-request
          (redirect-to "/rr?error=You%20already%20have%20a%20pending%20R%26R%20request.%20Please%20wait%20for%20it%20to%20be%20reviewed%20or%20cancel%20it%20first."))))
    
    ;; Check for approved R&R that hasn't ended yet (must be back in theater first)
    (let ((active-rr (fetch-one 
                      "SELECT id, end_date FROM rr_requests 
                       WHERE user_id = ? AND status = 'Approved' AND end_date >= date('now')
                       ORDER BY end_date DESC LIMIT 1"
                      user-id)))
      (when active-rr
        (return-from handle-api-rr-request
          (redirect-to (format nil "/rr?error=You%20have%20an%20approved%20R%26R%20ending%20~A.%20You%20cannot%20submit%20a%20new%20request%20until%20after%20your%20return%20date."
                               (getf active-rr :|end_date|))))))
    
    ;; Validate dates
    (let* ((start (parse-date-string start-date))
           (end (parse-date-string end-date))
           (total-days (1+ (round (/ (- end start) 86400)))))
      
      ;; Check end date is after start date
      (when (< end start)
        (return-from handle-api-rr-request
          (redirect-to "/rr?error=End%20date%20must%20be%20after%20start%20date")))
      
      ;; Check 17-day limit
      (when (> total-days 17)
        (return-from handle-api-rr-request
          (redirect-to "/rr?error=R%26R%20request%20cannot%20exceed%2017%20days")))
      
      ;; Check for conflicts (warn but allow submission)
      (let* ((limit-info (get-staff-category-limit category))
             (max-concurrent (or (getf limit-info :|max_concurrent_leave|) 3))
             (conflicts (get-max-approved-in-range start-date end-date category)))
        
        ;; Create the request
        (create-rr-request user-id start-date end-date travel-to travel-from)
        
        (if (>= conflicts max-concurrent)
            (redirect-to "/rr?warning=Request%20submitted%20but%20dates%20conflict%20with%20existing%20approved%20leave")
            (redirect-to "/rr?success=R%26R%20request%20submitted%20successfully"))))))

(defun handle-api-rr-approve (id-str)
  "Handle R&R approval."
  (let* ((user (get-current-user))
         (user-id (getf user :|id|))
         (request-id (parse-integer id-str :junk-allowed t))
         (request (get-rr-request request-id))
         (category (getf request :|staff_category|))
         (user-role (getf user :|role|)))
    
    ;; Check authorization (Program Manager is read-only, cannot approve)
    (let ((can-approve (or (string= user-role "Admin")
                           (and (string= category "Electrician")
                                (string= user-role "ao_lead")))))
      (if can-approve
          (progn
            (approve-rr-request request-id user-id nil)
            (redirect-to "/rr/approve?success=Request%20approved"))
          (redirect-to "/unauthorized")))))

(defun handle-api-rr-reject (id-str)
  "Handle R&R rejection."
  (let* ((user (get-current-user))
         (user-id (getf user :|id|))
         (request-id (parse-integer id-str :junk-allowed t))
         (request (get-rr-request request-id))
         (category (getf request :|staff_category|))
         (user-role (getf user :|role|))
         (comments (hunchentoot:parameter "comments")))
    
    ;; Check authorization (Program Manager is read-only, cannot reject)
    (let ((can-reject (or (string= user-role "Admin")
                          (and (string= category "Electrician")
                               (string= user-role "ao_lead")))))
      (if can-reject
          (progn
            (reject-rr-request request-id user-id comments)
            (redirect-to "/rr/approve?success=Request%20rejected"))
          (redirect-to "/unauthorized")))))

(defun handle-api-rr-cancel (id-str)
  "Handle R&R cancellation by the user who submitted it."
  (let* ((user (get-current-user))
         (user-id (getf user :|id|))
         (request-id (parse-integer id-str :junk-allowed t))
         (request (when request-id (get-rr-request request-id))))
    (cond
      ;; Request not found
      ((not request)
       (redirect-to "/rr?error=Request%20not%20found"))
      ;; Not the owner
      ((not (= (getf request :|user_id|) user-id))
       (redirect-to "/rr?error=You%20can%20only%20cancel%20your%20own%20requests"))
      ;; Not pending
      ((not (string= (getf request :|status|) "Pending"))
       (redirect-to "/rr?error=Only%20pending%20requests%20can%20be%20cancelled"))
      ;; All good - delete the request
      (t
       (execute-sql "DELETE FROM rr_requests WHERE id = ?" request-id)
       (redirect-to "/rr?success=R%26R%20request%20cancelled")))))

(defun handle-api-rr-check-dates ()
  "API to check date availability (for AJAX validation)."
  (let* ((start-date (hunchentoot:parameter "start_date"))
         (end-date (hunchentoot:parameter "end_date"))
         (category (or (hunchentoot:parameter "category") "Electrician"))
         (limit-info (get-staff-category-limit category))
         (max-concurrent (or (getf limit-info :|max_concurrent_leave|) 3))
         (conflicts (when (and start-date end-date)
                      (get-max-approved-in-range start-date end-date category))))
    (json-response 
     (list :available (< (or conflicts 0) max-concurrent)
           :conflicts conflicts
           :max_allowed max-concurrent))))

(defun handle-api-rr-month-leave ()
  "API to get all approved R&R for a given month (for calendar preview)."
  (let* ((month-str (hunchentoot:parameter "month"))  ; Format: YYYY-MM
         (year-month (when month-str (format nil "~A-%" month-str)))
         (results (when month-str
                    (fetch-all 
                     "SELECT u.full_name, r.start_date, r.end_date, r.total_days
                      FROM rr_requests r
                      JOIN users u ON r.user_id = u.id
                      WHERE r.status = 'Approved'
                        AND ((r.start_date LIKE ? OR r.end_date LIKE ?)
                             OR (r.start_date < ? AND r.end_date > ?))
                      ORDER BY r.start_date"
                     year-month year-month
                     (format nil "~A-01" month-str)
                     (format nil "~A-31" month-str)))))
    (setf (hunchentoot:content-type*) "application/json")
    (format nil "[~{~A~^,~}]"
            (mapcar (lambda (r)
                      (format nil "{\"name\":\"~A\",\"start_date\":\"~A\",\"end_date\":\"~A\",\"days\":~A}"
                              (getf r :|full_name|)
                              (format-date-display (getf r :|start_date|))
                              (format-date-display (getf r :|end_date|))
                              (or (getf r :|total_days|) 0)))
                    (or results '())))))

(defun handle-rr-edit (id-str)
  "Edit R&R request page - Admin/AO Lead/Program Manager only."
  (let* ((user (get-current-user))
         (user-role (getf user :|role|))
         (is-admin (or (user-is-admin-p user)
                       (string= user-role "ao_lead")))
         (request-id (parse-integer id-str :junk-allowed t))
         (rr (when request-id (get-rr-request request-id))))
    ;; Program Manager is read-only, cannot edit R&R requests
    (if (and is-admin rr)
        (html-response
         (render-page "Edit R&R Request"
           (cl-who:with-html-output-to-string (s)
             (:div :class "page-header"
               (:h1 "Edit R&R Request")
               (:a :href "/rr/calendar" :class "btn" "Back to Calendar"))
             (:section :class "card" :style "max-width: 600px;"
               (:h2 (cl-who:str (getf rr :|full_name|)))
               (:form :method "post" :action (format nil "/api/rr/~A/update" request-id)
                 (:div :class "form-group"
                   (:label "Status")
                   (:select :name "status" :class "form-control"
                     (:option :value "Pending" :selected (string= (getf rr :|status|) "Pending") "Pending")
                     (:option :value "Approved" :selected (string= (getf rr :|status|) "Approved") "Approved")
                     (:option :value "Rejected" :selected (string= (getf rr :|status|) "Rejected") "Rejected")
                     (:option :value "Completed" :selected (string= (getf rr :|status|) "Completed") "Completed")))
                 (:div :class "form-group"
                   (:label "Start Date")
                   (:input :type "date" :name "start_date" :class "form-control"
                           :value (getf rr :|start_date|)))
                 (:div :class "form-group"
                   (:label "End Date")
                   (:input :type "date" :name "end_date" :class "form-control"
                           :value (getf rr :|end_date|)))
                 (:div :class "form-group"
                   (:label "Flying To (Destination)")
                   (:input :type "text" :name "travel_to" :class "form-control"
                           :value (or (getf rr :|travel_to|) "")
                           :placeholder "e.g., London, UK"))
                 (:div :class "form-group"
                   (:label "Returning From")
                   (:input :type "text" :name "travel_from" :class "form-control"
                           :value (or (getf rr :|travel_from|) "")
                           :placeholder "e.g., London, UK"))
                 (:div :style "margin-top: 1rem;"
                   (:button :type "submit" :class "btn btn-primary" "Save Changes")
                   (:a :href "/rr/calendar" :class "btn" :style "margin-left: 0.5rem;" "Cancel")))))))
        (redirect-to "/unauthorized"))))

(defun handle-api-rr-update (id-str)
  "Update R&R request details - Admin/AO Lead only (Program Manager is read-only)."
  (let* ((user (get-current-user))
         (user-id (getf user :|id|))
         (user-role (getf user :|role|))
         (is-admin (or (user-is-admin-p user)
                       (string= user-role "ao_lead")))
         (request-id (parse-integer id-str :junk-allowed t))
         (status (hunchentoot:parameter "status"))
         (start-date (hunchentoot:parameter "start_date"))
         (end-date (hunchentoot:parameter "end_date"))
         (travel-to (hunchentoot:parameter "travel_to"))
         (travel-from (hunchentoot:parameter "travel_from")))
    (if is-admin
        (let* ((start (parse-date-string start-date))
               (end (parse-date-string end-date))
               (total-days (1+ (round (/ (- end start) 86400)))))
          (execute-sql 
           "UPDATE rr_requests SET status = ?, start_date = ?, end_date = ?, total_days = ?,
            travel_to = ?, travel_from = ?, reviewed_by = ?, reviewed_at = datetime('now')
            WHERE id = ?"
           status start-date end-date total-days travel-to travel-from user-id request-id)
          (redirect-to "/rr/calendar"))
        (redirect-to "/unauthorized"))))
