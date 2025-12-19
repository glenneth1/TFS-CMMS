(in-package #:tfs-cmms)

(defun get-app-directory ()
  "Get the application directory - works for both development and standalone executable."
  (or (ignore-errors (asdf:system-source-directory :tfs-cmms))
      (uiop:getcwd)
      *default-pathname-defaults*))

(defvar *database-path* 
  (merge-pathnames "data/tfs-cmms.db" (get-app-directory))
  "Path to the SQLite database file.")

(defvar *server-port* 8080
  "Port for the web server.")

(defvar *wo-prefix* "TFS"
  "Prefix for work order numbers.")

(defvar *wo-statuses* 
  '("New" "Awaiting Approval" "Approved" "Assigned" "In Progress" 
    "Waiting Parts" "Waiting Access" "QAQC" "Complete" "Closed")
  "Valid work order status values.")

(defvar *priorities*
  '("High" "Medium" "Low" "Routine")
  "Work order priority levels.")

(defvar *work-types*
  '("Initial Inspection" "Re-inspection/Repair" "Corrective Maintenance" 
    "Immediate Corrective Action" "BOS-I Coordination")
  "Types of work orders.")

(defvar *building-types*
  '("LSAU" "LSA" "ACCOMM" "BDOC" "TOC" "OFFICE" "HALLWAY" "CORRIDOR" 
    "MWR" "COMMON AREA" "GENERATOR" "MTS" "MDP" "SDP" "CDP")
  "Building/facility types for inspections.")

(defvar *default-sites*
  '(("H4QXY" "Camp Arifjan" "Kuwait")
    ("R5SPE" "Camp Buehring" "Kuwait")
    ("SMMAD" "AMOD" "Kuwait")
    ("S6CYH" "Ali Al Salem Air Base" "Kuwait")
    ("446PD" "KNB" "Kuwait")
    ("SPART" "Sparta" "Kuwait")
    ("9R7UA" "SPOD" "Kuwait")
    ("EURGL" "Camp KASOTC" "Jordan")
    ("5VN8M" "JTC" "Jordan")
    ("CTP5R" "PHRB" "Jordan")
    ("969ZA" "MSAB" "Jordan")
    ("7NX32" "H-5" "Jordan")
    ("LTRYC" "HTC" "Jordan")
    ("GCP23" "Titin" "Jordan")
    ("CP2FL" "Tower 22" "Jordan")
    ("75KR8" "New JTC" "Jordan")
    ("JAVRH" "Azraq Air Base" "Jordan")
    ("8AD6R" "Badger" "Jordan")
    ("1T4JI" "Taji" "Iraq")
    ("F797V" "Erbil" "Iraq")
    ("KIRKU" "Kirkuk" "Iraq")
    ("QWEST" "Qwest" "Iraq")
    ("XZ722" "Al Asad" "Iraq")
    ("2R6S8" "Baghdad Diplomatic Support Center" "Iraq")
    ("LSCNF" "EIA" "Iraq")
    ("BXPEZ" "CTSOTF" "Iraq")
    ("EYVPF" "MARSOC" "Iraq")
    ("B62XQ" "Canadians" "Iraq")
    ("TQIRA" "TQ" "Iraq")
    ("MANIO" "Camp Manion, TQ" "Iraq")
    ("A1QUM" "Al Qa'im" "Iraq")
    ("NOC-E" "NOC-E" "Iraq")
    ("THERA" "The Ranch, TQ" "Iraq")
    ("A88T2" "KSB" "Iraq")
    ("P9P4U" "Al Riffa Air Base" "Bahrain")
    ("69FKG" "Isa Air Base" "Bahrain")
    ("CASAB" "CAS Air Base" "Qatar")
    ("CASSO" "CAS South" "Qatar")
    ("8MER1" "Al Udeid Air Base" "Qatar")
    ("2XTAU" "Al Dhafra Air Base" "UAE")
    ("KATAB" "KA2AB" "Jordan")
    ("ATGXX" "ATG" "Syria")
    ("RIYAN" "Camp Owens" "Yemen")
    ("GV021" "Green Village" "Syria")
    ("CPGO1" "Camp Pongo" "Iraq")
    ("NAR21" "Camp Narvik" "Iraq")
    ("MON20" "Camp Monschke" "Iraq")
    ("AREA4" "Area IV" "Iraq")
    ("CONO" "MSSE/Conoco" "Syria")
    ("RUMLZ" "Rumalyn LZ" "Syria")
    ("T2XX" "T2" "Syria")
    ("KSA01" "Sharura" "KSA")
    ("DAD22" "Shaddadi" "Syria")
    ("TA1" "Training Area 1" "Jordan")
    ("HAM22" "Al-Hamra" "UAE")
    ("KFAB22" "KFAB" "Jordan")
    ("BASAIR" "Bashur Airfield" "Iraq")
    ("MNMB" "MNMB" "Egypt")
    ("TAR23" "Camp Tarlavsky" "Iraq")
    ("SUL23" "Camp Sulay" "Iraq")
    ("MTH23" "Moore Team House" "Syria")
    ("SAS24" "Sas Al Nakhl Air Force Base" "UAE")
    ("RDL24" "Redleg" "UAE")
    ("NLZ24" "Northern LZ" "Syria")
    ("KLZ24" "Kurdish LZ" "Syria"))
  "Default sites to seed the database.")
