// TFS-CMMS User Guide - Admin and QC Roles

#show heading: set text(16pt)

#set text(
  font: "Carlito",
  size: 11pt
)

#set page(
  paper: "a4",
  margin: (top: 2.5cm, bottom: 2.5cm, x: 2.5cm),
  header: context {
    if counter(page).get().first() > 1 [
      #set text(9pt, fill: gray)
      TFS-CMMS User Guide — Admin & QC Roles
      #h(1fr)
      Version 1.0 — December 2025
    ]
  },
  footer: context [
    #set text(9pt)
    Task Force SAFE — Computerized Maintenance Management System
    #h(1fr)
    Page #counter(page).display("1 of 1", both: true)
  ]
)

#set par(justify: true)
#set heading(numbering: "1.")

// Title Page
#align(center)[
  #v(2cm)
  #image("TFS_Logo.png", width: 40%)
  #v(1cm)
  #text(28pt, weight: "bold")[TFS-CMMS]
  #v(0.5cm)
  #text(18pt)[Computerized Maintenance Management System]
  #v(1cm)
  #line(length: 60%, stroke: 2pt + rgb("#003366"))
  #v(1cm)
  #text(22pt, weight: "bold")[User Guide]
  #v(0.5cm)
  #text(16pt)[Administrator & Quality Control Roles]
  #v(2cm)
  #text(12pt)[
    Version 1.0 \
    December 2025
  ]
  #v(1fr)
  #image("company_logo.png", width: 30%)
  #v(0.5cm)
  #text(10pt)[
    Versar, Inc. \
    1025 Vermont Ave NW, Suite 500 \
    Washington DC, 20005
  ]
]

#pagebreak()

// Table of Contents
#outline(
  title: [Table of Contents],
  indent: auto,
  depth: 3,
)

#pagebreak()

= Introduction

== Purpose

This guide provides comprehensive instructions for Administrators and Quality Control (QC) personnel using the Task Force SAFE Computerized Maintenance Management System (TFS-CMMS). The system is designed to manage electrical inspection reports, track deficiencies, generate weekly reports, and maintain a master tracker of all inspection data across multiple countries and camps.

== System Overview

TFS-CMMS is a web-based application that supports the following core functions:

- *User Management* — Create and manage user accounts with role-based access
- *Site Management* — Configure inspection sites across multiple countries
- *Inspection Reports* — Create, review, and approve electrical inspection reports
- *Deficiency Tracking* — Track and manage electrical deficiencies from identification to closure
- *Master Tracker* — Centralized database of all deficiencies across all sites
- *Weekly Reports* — Generate client-ready reports with charts and statistics
- *Work Orders* — Manage corrective maintenance work orders

== User Roles

The system supports the following user roles:

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Role*], [*Description*],
  [Administrator], [Full system access including user management, site configuration, Master Tracker, and all QC functions],
  [Program Manager], [Program oversight, R&R approval, Master Tracker access, weekly report generation],
  [AO Lead], [Area of Operations Lead - R&R approval for electricians, Master Tracker access],
  [QC Manager], [QC oversight, report assignment, Master Tracker access, weekly report generation],
  [QC], [Quality Control personnel who review and approve/reject inspection reports, Master Tracker access],
  [Inspector], [Field personnel who create and submit inspection reports (covered in separate guide)],
  [Electrician], [Field electricians who can submit R&R requests and view their own R&R status],
)

#pagebreak()

= Getting Started

== Accessing the System

+ Open a web browser (Chrome, Firefox, or Edge recommended)
+ Navigate to the TFS-CMMS URL: `http://localhost:8080` (or your configured server address)
+ You will be presented with the login screen

== Logging In

+ Enter your *Username* in the first field
+ Enter your *Password* in the second field
+ Click the *Login* button
+ Upon successful authentication, you will be directed to the Dashboard

#block(
  fill: rgb("#fff3cd"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Security Note:* Sessions expire after 24 hours of inactivity. Always log out when leaving your workstation unattended.
]

== Dashboard Overview

The Dashboard provides a quick overview of system status and pending actions:

*For Administrators:*
- Total counts of work orders, inspection reports, and deficiencies
- Quick links to all system modules
- Recent activity summary

*For QC Personnel:*
- Reports pending QC review
- Recently approved/rejected reports
- Quick access to the QC queue

#pagebreak()

= Administrator Functions

== User Management

Administrators can create, modify, and deactivate user accounts.

=== Creating a New User

+ From the Dashboard, click *Admin* in the navigation menu
+ Select *Users* from the dropdown
+ Click the *Add User* button
+ Complete the user form:
  - *Username* — Unique login identifier (required)
  - *Full Name* — User's display name (required)
  - *Email* — User's email address (required)
  - *Password* — Initial password (required, minimum 8 characters)
  - *Role* — Select from Administrator, QC, or Inspector
  - *Active* — Check to enable the account
+ Click *Save* to create the user

#block(
  fill: rgb("#d4edda"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Best Practice:* Instruct new users to change their password upon first login.
]

=== Modifying a User

+ Navigate to *Admin* → *Users*
+ Click on the user's name or the *Edit* button
+ Modify the required fields:
  - *Full Name* — User's display name
  - *Email* — Contact email address
  - *Role* — User's access level
  - *Electrician Type* — Master or Journeyman (for electricians)
  - *Team Number* — Assigned team
  - *Hire Date* — Employment start date
  - *BOG Date* — Boots on Ground date (used for R&R accrual calculations)
  - *Status* — Active or Inactive
+ Click *Save* to apply changes

=== Deactivating a User

+ Navigate to *Admin* → *Users*
+ Click *Edit* next to the user
+ Uncheck the *Active* checkbox
+ Click *Save*

#block(
  fill: rgb("#f8d7da"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Important:* Deactivated users cannot log in but their historical data is preserved.
]

== Site Management

Sites represent physical locations where inspections are conducted.

=== Adding a New Site

+ Navigate to *Sites* from the main menu
+ Click *Add Site*
+ Enter the site details:
  - *Site Code* — Unique identifier (e.g., "F797V")
  - *Site Name* — Full name with country (e.g., "Erbil (Iraq)")
+ Click *Save*

=== Linking Sites to Master Tracker Camps

Sites in the CMMS are linked to Camps in the Master Tracker for reporting purposes. This linkage is established automatically when camp names match site names, or can be configured manually in the database.

#pagebreak()

= Quality Control Functions

== QC Dashboard

When logged in as a QC user, the Dashboard displays:

- *Reports Pending QC* — Count of reports awaiting review
- *Recently Reviewed* — Your recent approval/rejection activity
- *Quick Actions* — Direct links to the QC queue

== Reviewing Inspection Reports

=== Accessing the QC Queue

+ From the Dashboard, click *Inspection Reports*
+ Use the *Status* filter and select "Pending QC"
+ Click *Filter* to display reports awaiting review

=== Reviewing a Report

+ Click on a report number to open the full report
+ Review the following sections:

*Header Information:*
- Site and building details
- Inspection date and team number
- System voltage and inspection phase
- Inspector signatures

*Deficiencies:*
- Location descriptions
- Deficiency categories and equipment types
- Code references (NEC or BS7671)
- Photographs (if attached)
- RAC scores
- Current status

*Summary:*
- Overall rating
- Summary of findings
- MRF requirement indication

=== Approving a Report

If the report meets quality standards:

+ Scroll to the bottom of the report
+ In the *QC Review* section, optionally enter comments
+ Click the *Approve* button
+ Confirm the approval when prompted

#block(
  fill: rgb("#d4edda"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Note:* Upon approval, all deficiencies from the report are automatically synced to the Master Tracker database.
]

=== Rejecting a Report

If the report requires corrections:

+ Scroll to the *QC Review* section
+ Enter detailed comments explaining the required corrections (required)
+ Click the *Reject* button
+ Confirm the rejection when prompted

The report will be returned to the inspector with your comments displayed on their dashboard.

== QC Review Checklist

Use this checklist when reviewing reports:

#block(
  stroke: 0.5pt,
  inset: 10pt,
  radius: 4pt,
)[
  *Report Header*
  - [ ] Correct site and building information
  - [ ] Valid inspection date
  - [ ] Team number assigned
  - [ ] Correct system voltage selected
  - [ ] Appropriate inspection phase

  *Deficiencies*
  - [ ] Clear location descriptions
  - [ ] Appropriate deficiency categories
  - [ ] Correct code references for voltage/frequency
  - [ ] RAC scores assigned where required
  - [ ] Photos attached for significant deficiencies
  - [ ] Status correctly set (Open/Closed)

  *Summary*
  - [ ] Overall rating reflects findings
  - [ ] Summary accurately describes conditions
  - [ ] MRF indication correct

  *Signatures*
  - [ ] Inspector 1 signed
  - [ ] Inspector 2 signed (if applicable)
]

#pagebreak()

= Master Tracker

The Master Tracker is the central database for all deficiency data across all sites and countries.

== Accessing the Master Tracker

+ From the main menu, click *Master Tracker*
+ The Master Tracker dashboard displays summary statistics

== Filtering Data

Use the filter controls to narrow down the displayed data:

=== By Country
+ Click the *Country* dropdown
+ Select the desired country
+ Click *Filter*

=== By Camp
+ First select a Country
+ Click the *Camp* dropdown (populated based on country selection)
+ Select the desired camp
+ Click *Filter*

=== By Status
+ Click the *Status* dropdown
+ Select from:
  - Open
  - Closed / Repaired
  - ReInsp-Open
  - Closed Previously
  - Mitigated
  - N/A
+ Click *Filter*

=== By Date Range
+ Enter a *Start Date*
+ Enter an *End Date*
+ Click *Filter*

== Understanding the Data

The Master Tracker displays the following columns:

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Column*], [*Description*],
  [Camp], [Location/site name],
  [Building], [Building number or identifier],
  [Def \#], [Unique deficiency number],
  [Category], [Deficiency category (e.g., Grounding and Bonding)],
  [Equipment], [Equipment type code],
  [Phase], [Inspection phase (Initial/Re-Inspection)],
  [Status], [Current deficiency status],
  [Date], [Inspection date],
  [RAC], [Risk Assessment Code (1-5)],
)

== Data Synchronization

When a QC user approves an inspection report, the system automatically:

+ Validates all required fields are present
+ Checks for existing records (updates if found)
+ Creates new records in the Master Tracker
+ Links the data to the appropriate camp and country

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Validation Checks:* The system validates the following before syncing:
  - Building number is present
  - Inspection date is set
  - Inspection phase is specified
  - Site is mapped to a camp
  - Each deficiency has a category and status
]

#pagebreak()

= Weekly Reports

The Weekly Report generator creates client-ready slides and charts for operational updates.

== Accessing Weekly Reports

+ From the main menu, click *Master Tracker*
+ Click *Weekly Report* or navigate to the Weekly Report page

== Generating Reports

=== Selecting the Date Range

+ Enter the *Week Start* date (typically Monday)
+ Enter the *Week End* date (typically Saturday)
+ Click *Generate Report Slides*

The system will process the data and generate five PNG images:

=== Generated Slides

+ *Operational Updates* — REDi format summary showing:
  - Number of facilities inspected
  - Total reports uploaded
  - Breakdown of inspections vs. re-inspections

+ *Weekly by Country* — Bar chart showing:
  - Facilities inspected per country
  - Deficiency counts per country

+ *Deficiency Types* — Pie/bar chart showing:
  - Distribution of deficiency categories
  - Top deficiency types identified

+ *Cumulative Stats* — Overall program statistics:
  - Total facilities inspected (all-time)
  - Total deficiencies identified
  - Open vs. closed status breakdown

+ *90-Day Stats* — Rolling 90-day statistics:
  - Recent activity trends
  - Country-by-country breakdown

== Downloading Reports

Each generated slide can be downloaded individually:

+ After generation, slides are displayed on the page
+ Click the *Download* button below each slide
+ The PNG file will be saved to your downloads folder
+ Files are named descriptively (e.g., `01_operational_updates.png`)

== Using Reports in PowerPoint

The generated PNG images are sized for direct insertion into PowerPoint:

+ Open your PowerPoint presentation
+ Navigate to the appropriate slide
+ Insert → Pictures → Select the downloaded PNG
+ The image is pre-sized for standard slide dimensions

#pagebreak()

= Work Orders

Work orders track corrective maintenance activities.

== Creating a Work Order

+ Navigate to *Work Orders* from the main menu
+ Click *Create Work Order*
+ Complete the form:
  - *Site* — Select the location
  - *Priority* — High, Medium, Low, or Routine
  - *Work Type* — Type of maintenance activity
  - *Description* — Detailed description of work required
  - *Assigned To* — Team or individual responsible
+ Click *Save*

== Work Order Statuses

Work orders progress through the following statuses:

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Status*], [*Description*],
  [New], [Just created, not yet reviewed],
  [Awaiting Approval], [Pending management approval],
  [Approved], [Approved for execution],
  [Assigned], [Assigned to a team],
  [In Progress], [Work is underway],
  [Waiting Parts], [On hold pending parts delivery],
  [Waiting Access], [On hold pending site access],
  [QAQC], [Work complete, pending quality review],
  [Complete], [All work and QC complete],
  [Closed], [Administratively closed],
)

== Updating Work Orders

+ Open the work order by clicking its number
+ Click *Edit*
+ Update the status and add activity notes
+ Click *Save*

#pagebreak()

= Reports and Analytics

== Available Reports

The system provides several reporting capabilities:

=== Inspection Report PDF

+ Open any inspection report
+ Click *Generate PDF*
+ The system creates a formatted PDF suitable for client delivery

=== Master Tracker Export

+ Navigate to the Master Tracker
+ Apply desired filters
+ Data can be exported for external analysis

=== Weekly Statistics

+ Use the Weekly Report generator for formatted statistics
+ Historical data is preserved for trend analysis

== Key Metrics

Monitor these key performance indicators:

- *Facilities Inspected* — Unique buildings inspected per period
- *Reports Uploaded* — Total inspection reports completed
- *Open Deficiencies* — Deficiencies requiring attention
- *Closure Rate* — Percentage of deficiencies resolved
- *Re-inspection Rate* — Ratio of re-inspections to initial inspections

#pagebreak()

= Troubleshooting

== Common Issues

=== Cannot Log In

- Verify username and password are correct
- Check if your account is active (contact Administrator)
- Clear browser cache and cookies
- Try a different browser

=== Report Not Syncing to Master Tracker

Check that:
- The site is linked to a camp in the Master Tracker
- All required fields are populated
- The report was approved (not just submitted)

=== Weekly Report Shows Incorrect Numbers

- Verify the date range is correct (use YYYY-MM-DD format)
- Check that the year is correct (2024 vs 2025)
- Ensure data exists for the selected period

=== PDF Generation Fails

- Ensure the Python environment is properly configured
- Check that required fonts are installed
- Verify the reports directory is writable

== Getting Help

For technical support:
- Contact your system administrator
- Email: #link("mailto:GThompson@versar.com")

#pagebreak()

= R&R (Rest and Recuperation) Management

The R&R module allows management of employee leave requests and travel arrangements.

== Accessing R&R

+ From the main menu, click *R&R*
+ Electricians see their personal R&R dashboard
+ Managers see the R&R calendar and approval queue

== R&R Dashboard (Electricians)

The personal dashboard displays:
- *R&R Balance* — Accrued days minus used days
- *Pending Requests* — Requests awaiting approval
- *Approved Upcoming* — Confirmed future R&R
- *R&R History* — Past requests and their status

=== Submitting an R&R Request

+ Click *Request R&R*
+ Enter the requested dates (including travel days)
+ Specify travel destination (Flying To)
+ Specify return location (Returning From)
+ Click *Submit Request*

#block(
  fill: rgb("#fff3cd"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Note:* R&R allowance is 15 days plus 2 travel days (17 total). Requests are subject to approval based on operational requirements.
]

== R&R Calendar (Managers)

The R&R Calendar provides a monthly view of all approved and pending R&R.

=== Viewing the Calendar

+ Navigate to *R&R* → *Calendar*
+ Use the month/year selectors to navigate
+ The calendar displays:
  - Employee names (clickable to edit)
  - Electrician type (Master/Journeyman)
  - Start and end dates
  - Travel destinations
  - Current status

=== Editing R&R Details

Administrators, AO Leads, and Program Managers can edit R&R requests:

+ Click on the employee's name in the calendar
+ Update the following fields:
  - Status (Pending/Approved/Rejected/Completed)
  - Start and End dates
  - Flying To destination
  - Returning From location
+ Click *Save Changes*

=== Printable Flight Schedule

+ From the R&R Calendar, click *Print View*
+ The print view shows:
  - Outbound flights (departures this month)
  - Return flights (arrivals this month)
  - Employee details and travel information
+ Use browser print function or save as PDF

== R&R Approval Queue

AO Leads and Program Managers review and approve R&R requests:

+ Navigate to *R&R* → *Approval Queue*
+ Review pending requests by category:
  - Electrician requests (max 3 concurrent)
  - PMO requests (max 1 concurrent)
  - QC requests (max 1 concurrent)
+ Check for scheduling conflicts
+ Click *Approve* or *Reject* for each request

#block(
  fill: rgb("#f8d7da"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Important:* The system displays conflict warnings when approving would exceed maximum concurrent leave limits.
]

#pagebreak()

= Daily Activity Reports (DAR)

The Daily Activity Report system allows electrician teams to document their daily activities and submit reports to QA/QC.

== Accessing DAR

Navigate to *DAR* in the main navigation menu.

== Creating a New DAR

+ Click *New DAR*
+ Fill in the report details:
  - *Date* — Report date
  - *Team Number* — Your assigned team number
  - *Team Member Names* — Names of team members
  - *Camp/Location* — Work location
  - *Site Lead* — Name of site lead
+ Complete the Safety section:
  - *Toolbox Topic* — Safety topic discussed
  - *Attended Safety Brief* — Yes/No
  - *HECP Package Submitted* — Checkbox
+ Add Activity Records:
  - Select *Time* from dropdown (30-minute increments)
  - Enter *Activity* description
  - Add optional *Notes*
  - Click *+ Add Activity* for additional rows
+ Add Reports Submitted to QA/QC:
  - Enter *Report TAG ID*
  - Select *Submitted* status (Yes/No)
  - Click *+ Add Report* for additional rows
+ Click *Submit DAR*

== Viewing and Downloading DAR

+ Navigate to *DAR* list
+ Click *View* on any report
+ Click *Download PDF* to generate a PDF with naming convention: `DAR_SITE_DATE_TEAM.pdf`

== Access Control

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Role*], [*Access*],
  [Electrician], [Create and view own team's DARs],
  [Master Electrician], [View all DARs],
  [Admin], [Full access to all DARs],
  [AO Lead], [View all DARs],
)

#pagebreak()

= Immediate Repair Package (IRP)

The IRP system tracks standard inventory items issued to electrician teams.

== Accessing IRP

Navigate to *IRP* in the main navigation menu.

== Creating a New IRP Submission

+ Click *New IRP*
+ Fill in submission details:
  - *Date* — Submission date
  - *Team Number* — Your assigned team number
  - *Camp/Location* — Work location
+ Review the standard items list (37 items)
+ For each item, enter:
  - *Issued Qty* — Quantity originally issued
  - *Current Qty* — Current quantity on hand
+ Click *Submit IRP*

== Viewing IRP Submissions

+ Navigate to *IRP* list
+ Click *View* on any submission
+ Items with discrepancies (issued ≠ current) are highlighted in yellow

#pagebreak()

= Site Activity Report (SAR)

The Site Activity Report is a consolidated multi-page PDF showing all inspection activity across all sites for a given date.

== Accessing SAR

Navigate to *SAR* in the main navigation menu. This link is only visible to authorized roles.

== Generating a SAR

+ Select the *Report Date*
+ Click *Generate SAR PDF*
+ The PDF downloads with naming convention: `DAR_ALL_SITES_DD-MMM-YY.pdf`

== SAR Content

Each page of the SAR shows one site with:
- Army logo (top left) and TFS logo (top right)
- Site name banner
- Table with columns:
  - *TEAM* — Team number
  - *Date* — Inspection date
  - *Location* — Site/camp name
  - *Facility/Building Number* — Building inspected
  - *Report TAG ID* — Full inspection report number
  - *Reports Submitted* — YES (green) or NO (red)
  - *SPLO* — Safety status
  - *EWP* — Safety status

== Access Control

Only the following roles can generate SAR reports:
- Admin
- AO Lead
- Program Manager
- Project Manager
- PMO

#pagebreak()

= Appendix A: Deficiency Categories

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Category*], [*Description*],
  [Grounding and Bonding], [Issues with electrical grounding or bonding connections],
  [Improper Terminations], [Incorrect or unsafe wire terminations],
  [Improper Use / Damaged], [Equipment used incorrectly or showing damage],
  [Poor Workmanship], [Installation quality issues],
  [Unlisted Equipment], [Equipment not properly listed/certified],
)

= Appendix B: Risk Assessment Codes (RAC)

#table(
  columns: (auto, auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*RAC*], [*Severity*], [*Description*],
  [1], [Critical], [Immediate danger to life, requires immediate action],
  [2], [Serious], [Significant hazard, requires prompt attention],
  [3], [Moderate], [Moderate risk, should be addressed soon],
  [4], [Minor], [Low risk, schedule for routine maintenance],
  [5], [Negligible], [Minimal risk, address during normal operations],
)

= Appendix C: Code References

== NEC (National Electrical Code) — 60Hz Systems

Used for sites with 120/208V or 277/480V systems:
- Article 110: General Rules
- Article 250: Grounding/Bonding
- Article 406: Receptacles
- Article 408: Panelboards

== BS7671 (British Standard) — 50Hz Systems

Used for sites with 230/400V systems:
- Regulation 132: Protection against electric shock
- Regulation 526: Electrical connections
- Regulation 542: Earthing arrangements

#pagebreak()

= Appendix D: Date Format

All dates in the system are displayed in the international format *DD-MMM-YYYY* (e.g., 21-DEC-2025) to avoid confusion between US (MM/DD/YYYY) and UK (DD/MM/YYYY) formats.

Examples:
- 15-JAN-2025
- 03-MAR-2025
- 22-DEC-2025

= Document Control

#table(
  columns: (auto, auto, auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Version*], [*Date*], [*Author*], [*Changes*],
  [1.0], [Dec 2025], [TFS Team], [Initial release],
  [1.1], [Dec 2025], [TFS Team], [Added R&R Management section, updated user roles, added date format appendix],
  [1.2], [Dec 2025], [TFS Team], [Added DAR, IRP, and SAR documentation sections],
)
