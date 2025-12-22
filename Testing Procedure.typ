// TFS-CMMS System Testing Procedure

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
      TFS-CMMS Testing Procedure
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
  #text(22pt, weight: "bold")[System Testing Procedure]
  #v(0.5cm)
  #text(16pt)[End-to-End Workflow Validation]
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

This document provides step-by-step procedures for testing the TFS-CMMS system workflows. It covers the complete inspection report lifecycle from creation through QC approval, including Material Request Form (MRF) generation and the rejection/correction workflow.

== Test Environment

Before beginning testing, ensure:

- The TFS-CMMS server is running and accessible
- Test user accounts are configured for each role
- The database contains sample sites and work orders

== Roles Involved

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Role*], [*Responsibilities in Testing*],
  [Inspector], [Creates inspection reports, adds deficiencies, generates MRFs, submits for QC],
  [QC Manager], [Assigns reports to QC reviewers, monitors workflow],
  [QC Reviewer], [Reviews reports and MRFs, approves or rejects with comments],
  [Administrator], [System configuration, user management, troubleshooting],
)

#pagebreak()

= Test 1: Inspection Report Creation

== Objective

Verify that an Inspector can create a complete inspection report with deficiencies and an MRF.

== Prerequisites

- Inspector account is active
- At least one work order exists in the system
- Site configuration is complete

== Procedure

=== Step 1.1: Login as Inspector

+ Navigate to the TFS-CMMS login page
+ Enter Inspector credentials
+ Click *Login*
+ Verify the Dashboard displays correctly

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Dashboard shows Inspector-specific view with pending work and recent activity.
]

=== Step 1.2: Access Work Orders

+ Click *Work Orders* in the navigation menu
+ Locate an existing work order or create a new one
+ Click on the work order number to open details

=== Step 1.3: Create Inspection Report

+ From the work order detail page, click *Create Inspection Report*
+ Complete the report header:
  - Select *Site* from dropdown
  - Enter *Building Number*
  - Select *Building Type*
  - Select *System Voltage* (determines code standard)
  - Select *Inspection Phase* (Initial Inspection or Re-Inspection)
  - Enter *Team Number*
  - Set *Inspection Date*
+ Click *Create Report*

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Report is created with status "Draft" and unique report number generated.
]

=== Step 1.4: Upload Building Image

+ In the Building Picture section, click *Choose File*
+ Select a building photograph
+ Click *Upload Image*

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Image displays in the report and is stored in the uploads directory.
]

=== Step 1.5: Add Deficiencies

+ Click *Add Deficiency*
+ Complete the deficiency form:
  - Enter *Location Description*
  - Select *Deficiency Category*
  - Select *Equipment Category*
  - Select *Code Reference* (auto-filtered by voltage)
  - Set *RAC Score* (1-5)
  - Enter *Description* of the deficiency
  - Upload deficiency photograph (optional)
+ Click *Save Deficiency*
+ Repeat for additional deficiencies as needed

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Each deficiency appears in the deficiencies table with correct details.
]

=== Step 1.6: Set MRF Required

+ In the Update Report section, locate *MRF Needed?* dropdown
+ Select *Yes*
+ Enter *Overall Rating*
+ Enter *Summary of Findings*
+ Click *Save Changes*

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* MRF section appears showing auto-generated MRF with View/Edit and Print buttons.
]

=== Step 1.7: Add Items to MRF

+ Click *View/Edit MRF*
+ Review MRF header information (auto-populated from report)
+ Click *Add Item*
+ Search for inventory item or enter manually:
  - Part Number
  - Description/Nomenclature
  - Quantity Requested
  - Unit of Measure
  - Type (Material or Tool)
  - Remarks (optional)
+ Click *Add Item*
+ Repeat for additional materials needed

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* MRF items table shows all added materials with correct details.
]

=== Step 1.8: Submit Report for QC

+ Return to the inspection report (click report link or use breadcrumb)
+ Scroll to *Signatures & QC* section
+ Enter *Inspector 1 Name*
+ Enter *Inspector 2 Name* (if applicable)
+ Click *Submit for QC Review*

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Report status changes to "Pending QC". Report is no longer editable by Inspector.
]

#pagebreak()

= Test 2: QC Manager Assignment

== Objective

Verify that a QC Manager can view pending reports and assign them to QC reviewers.

== Prerequisites

- QC Manager account is active
- At least one report is in "Pending QC" status
- QC Reviewer accounts exist

== Procedure

=== Step 2.1: Login as QC Manager

+ Log out of Inspector account
+ Login with QC Manager credentials
+ Verify Dashboard displays QC Manager view

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Dashboard shows pending QC count and unassigned reports prominently displayed.
]

=== Step 2.2: View Pending Reports

+ The Dashboard displays reports pending QC review
+ Unassigned reports appear at the top of the list
+ Each report shows:
  - Report number
  - Site name
  - Building number
  - Submission date
  - Current assignment status

=== Step 2.3: Assign Report to QC Reviewer

+ Locate the report submitted in Test 1
+ In the *Assigned To* column, click the dropdown
+ Select a QC Reviewer from the list
+ The form auto-submits on selection

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Report shows assigned QC Reviewer name. Report moves down in the list (assigned reports appear after unassigned).
]

=== Step 2.4: Verify Assignment

+ Refresh the page
+ Confirm the assignment persists
+ The assigned QC Reviewer's name displays in the table

#pagebreak()

= Test 3: QC Review and Approval

== Objective

Verify that a QC Reviewer can review an assigned report and approve it.

== Prerequisites

- QC Reviewer account is active
- Report has been assigned to this QC Reviewer

== Procedure

=== Step 3.1: Login as QC Reviewer

+ Log out of QC Manager account
+ Login with QC Reviewer credentials
+ Verify Dashboard displays assigned reports

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Dashboard shows only reports assigned to this QC Reviewer.
]

=== Step 3.2: Open Report for Review

+ Click *Process* button next to the assigned report
+ Review all report sections:
  - Header information (site, building, dates)
  - Building photograph
  - Deficiencies list with photos
  - Summary and overall rating

=== Step 3.3: Review Linked MRF

+ Locate the *Material Request Form (MRF)* section
+ Verify MRF details display:
  - MRF Number
  - Status
  - Item count
+ Click *View/Edit MRF* to review full MRF
+ Verify materials listed match deficiencies found
+ Click *Print MRF* to verify print layout (opens in new tab)
+ Return to inspection report

#block(
  fill: rgb("#fff3cd"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *QC Check:* Ensure MRF materials are appropriate for the deficiencies identified. Materials should match the scope of repairs needed.
]

=== Step 3.4: Approve Report

+ Scroll to *Signatures & QC* section
+ Enter *QC Name* (may be pre-filled)
+ Optionally enter *Comments*
+ Click *Approve*

#block(
  fill: rgb("#d4edda"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* 
  - Report status changes to "Complete"
  - Deficiencies sync to Master Tracker
  - PDF download button becomes available
  - Re-inspection option appears (for Initial Inspections)
]

=== Step 3.5: Verify Master Tracker Sync

+ Navigate to *Master Tracker*
+ Filter by the site/building from the approved report
+ Verify all deficiencies appear with correct:
  - Camp assignment
  - Building number
  - Deficiency numbers
  - Categories and statuses
  - Inspection date and phase

#pagebreak()

= Test 4: QC Review and Rejection

== Objective

Verify that a QC Reviewer can reject a report with comments and the Inspector can correct and resubmit.

== Prerequisites

- A new report in "Pending QC" status (repeat Test 1 Steps 1.1-1.8)
- Report assigned to QC Reviewer

== Procedure

=== Step 4.1: Login as QC Reviewer

+ Login with QC Reviewer credentials
+ Open the assigned report for review

=== Step 4.2: Identify Issues

For testing purposes, identify issues such as:
- Missing or unclear location descriptions
- Incorrect code references
- Missing photographs
- MRF materials don't match deficiencies
- Incomplete summary

=== Step 4.3: Reject Report

+ Scroll to *Signatures & QC* section
+ Enter *QC Name*
+ Enter detailed *Comments* explaining required corrections:
  - Be specific about what needs to be fixed
  - Reference specific deficiency numbers if applicable
  - Note any MRF discrepancies
+ Click *Reject*

#block(
  fill: rgb("#f8d7da"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Report status changes to "QC Rejected". Rejection count increments.
]

=== Step 4.4: Login as Inspector

+ Log out of QC Reviewer account
+ Login with Inspector credentials
+ Dashboard should indicate rejected report

=== Step 4.5: View Rejection Comments

+ Open the rejected report
+ Locate the rejection notice showing:
  - QC name who rejected
  - Rejection count
  - Detailed comments

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Report is editable again. All QC comments are visible.
]

=== Step 4.6: Make Corrections

+ Address each issue noted in QC comments:
  - Edit deficiencies as needed
  - Update report details
  - Modify MRF items if required
+ Save all changes

=== Step 4.7: Resubmit for QC

+ Scroll to *Signatures & QC* section
+ Inspector names may be pre-filled from previous submission
+ Click *Resubmit for QC Review*

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Report status returns to "Pending QC". Report appears in QC queue again.
]

=== Step 4.8: QC Re-review and Approve

+ Login as QC Reviewer
+ Open the resubmitted report
+ Verify corrections were made
+ Approve the report

#pagebreak()

= Test 5: MRF Standalone Creation

== Objective

Verify that an MRF can be created independently (not linked to an inspection report).

== Procedure

=== Step 5.1: Access MRF Module

+ Login as Inspector or authorized user
+ Click *MRF* in the navigation menu
+ Click *New MRF*

=== Step 5.2: Create MRF

+ Select *Base* from dropdown (populated from sites)
+ Enter *Building Number/Name*
+ Enter *Camp Name* (optional)
+ Enter *Team Number*
+ Enter *Requestor Name*
+ Click *Create MRF*

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* MRF created with auto-generated MRF number in format: Base-Building-MRF-DD-MM-YY
]

=== Step 5.3: Add Items and Print

+ Add material items as in Test 1, Step 1.7
+ Click *Print MRF* to generate printable version
+ Verify print layout includes:
  - TFS logo
  - Contract number (from system settings)
  - All header fields
  - Items table with line numbers

#pagebreak()

= Test 6: Sequential MRF Numbering

== Objective

Verify that multiple MRFs created on the same day for the same building receive sequential suffixes.

== Procedure

=== Step 6.1: Create First MRF

+ Create an MRF for a specific base and building
+ Note the MRF number (e.g., "Camp Arifjan-Bldg123-MRF-22-12-24")

=== Step 6.2: Create Second MRF Same Day

+ Create another MRF for the *same* base and building
+ Note the MRF number

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* Second MRF has suffix "-01" (e.g., "Camp Arifjan-Bldg123-MRF-22-12-24-01")
]

=== Step 6.3: Create Third MRF Same Day

+ Create a third MRF for the same base and building
+ Verify MRF number has suffix "-02"

#pagebreak()

= Test 7: Admin Settings

== Objective

Verify Administrator can configure system settings including contract number.

== Procedure

=== Step 7.1: Access Admin Settings

+ Login as Administrator
+ Click *Admin* in navigation
+ Click *Settings*

=== Step 7.2: Update Contract Number

+ Locate *Contract Number* field
+ Enter new contract number
+ Click *Save Settings*

=== Step 7.3: Verify Contract Number Usage

+ Create a new MRF
+ Print the MRF
+ Verify the updated contract number appears on the printed form

#pagebreak()

= Test 8: User Management

== Objective

Verify Administrator can manage user accounts including electrician type and team assignments.

== Procedure

=== Step 8.1: Edit User

+ Navigate to *Admin* → *Users*
+ Click *Edit* next to an Inspector account

=== Step 8.2: Set Electrician Fields

+ Select *Electrician Type*:
  - Master Electrician
  - Journeyman Electrician
+ Enter *Team Number*
+ Click *Save*

=== Step 8.3: Verify Fields Saved

+ Refresh the page
+ Re-open the user edit form
+ Verify electrician type and team number are preserved

#pagebreak()

= Test 9: Re-inspection Workflow

== Objective

Verify that a re-inspection report can be created from a completed initial inspection.

== Prerequisites

- A completed Initial Inspection report exists

== Procedure

=== Step 9.1: Access Completed Report

+ Navigate to *Inspection Reports*
+ Filter by Status: Complete
+ Open an Initial Inspection report

=== Step 9.2: Create Re-inspection

+ Click *Create Re-inspection*
+ Enter re-inspection details:
  - Team Number (may differ from original)
  - Inspection Date
+ Click *Create Re-inspection Report*

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Expected Result:* 
  - New report created with phase "Re-Inspection"
  - All deficiencies copied from original report
  - Deficiency statuses can be updated (Open → Closed)
  - New report number generated with "Re-Inspection" prefix
]

=== Step 9.3: Update Deficiency Statuses

+ Review each copied deficiency
+ Update status for repaired items:
  - Change from "Open" to "Closed / Repaired"
+ Add any new deficiencies found
+ Submit for QC review

#pagebreak()

= Test 10: PDF Generation

== Objective

Verify that completed reports can be exported as PDF.

== Procedure

=== Step 10.1: Access Completed Report

+ Open a report with status "Complete"

=== Step 10.2: Generate PDF

+ Click *Download PDF*
+ Verify PDF opens or downloads

=== Step 10.3: Verify PDF Contents

Check that the PDF includes:
- Report header with all details
- Building photograph
- All deficiencies with:
  - Location descriptions
  - Categories and codes
  - RAC scores
  - Photographs
- Summary and signatures
- Proper formatting and logos

#pagebreak()

= Test Results Summary

Use this checklist to record test results:

#table(
  columns: (auto, auto, auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Test*], [*Pass*], [*Fail*], [*Notes*],
  [1. Report Creation], [ ], [ ], [],
  [2. QC Manager Assignment], [ ], [ ], [],
  [3. QC Approval], [ ], [ ], [],
  [4. QC Rejection/Resubmit], [ ], [ ], [],
  [5. Standalone MRF], [ ], [ ], [],
  [6. Sequential MRF Numbers], [ ], [ ], [],
  [7. Admin Settings], [ ], [ ], [],
  [8. User Management], [ ], [ ], [],
  [9. Re-inspection Workflow], [ ], [ ], [],
  [10. PDF Generation], [ ], [ ], [],
)

#v(1cm)

*Tested By:* #box(width: 200pt, stroke: (bottom: 0.5pt))

*Date:* #box(width: 100pt, stroke: (bottom: 0.5pt))

*System Version:* #box(width: 100pt, stroke: (bottom: 0.5pt))

#pagebreak()

= Document Control

#table(
  columns: (auto, auto, auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Version*], [*Date*], [*Author*], [*Changes*],
  [1.0], [Dec 2025], [TFS Team], [Initial release],
)
