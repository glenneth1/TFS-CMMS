(asdf:defsystem #:tfs-cmms
  :description "Task Force SAFE Computerized Maintenance Management System"
  :author "TFS Team"
  :license "Proprietary"
  :version "0.1.0"
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:dbi
               #:dbd-sqlite3
               #:local-time
               #:alexandria
               #:cl-json
               #:cl-ppcre
               #:ironclad
               #:parse-float)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "config")
                             (:file "database")
                             (:file "models")
                             (:file "work-orders")
                             (:file "inspection-reports")
                             (:file "auth")
                             (:file "inventory")
                             (:file "mrf")
                             (:file "rr")
                             (:file "dar")
                             (:file "reports")
                             (:file "admin")
                             (:file "master-tracker")
                             (:file "perstat")
                             (:file "sor-reports")
                             (:file "backup")
                             (:file "system-monitor")
                             (:file "routes")))))
