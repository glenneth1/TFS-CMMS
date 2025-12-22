# TFS-CMMS

## Task Force SAFE - Computerized Maintenance Management System

A web-based CMMS built with Common Lisp (SBCL/Hunchentoot) and SQLite, designed for managing electrical inspection reports, deficiency tracking, and operational reporting across multiple international sites.

## Features

### Core Modules

- **Inspection Reports**: Create, submit, and track electrical inspection reports with QC workflow
- **Work Order Management**: Create, track, and close work orders with auto-generated WO numbers
- **Deficiency Tracking**: Master Tracker for all deficiencies across sites
- **Material Request Forms (MRF)**: Request and track materials for repairs

### Daily Operations

- **Daily Activity Reports (DAR)**: Electrician teams document daily activities
- **Immediate Repair Package (IRP)**: Track standard inventory items issued to teams
- **Site Activity Report (SAR)**: Consolidated multi-site PDF for AO Lead/PMO

### Personnel Management

- **R&R Leave Management**: Request, approve, and track Rest & Recuperation leave
- **Role-Based Access Control**: Admin, AO Lead, QC Manager, Inspector, Electrician, and more

### Reporting

- **PDF Generation**: Inspection reports, DARs, and consolidated SARs
- **Weekly Reports**: Per-site status reports matching PWS requirements
- **Audit Trail**: Full change history on all work orders

## WO Number Format

Work orders are automatically numbered:

```text
TFS-<SITE_CODE>-<YEAR>-<SEQUENCE>
Example: TFS-AJ-2025-00001
```

## Requirements

- SBCL (Steel Bank Common Lisp) 2.2.0 or later
- Quicklisp package manager
- SQLite3
- Python 3.10+ with virtual environment (for PDF generation)

### Python Dependencies

```bash
# Create virtual environment (one level up from app directory)
python3 -m venv ../.venv
source ../.venv/bin/activate
pip install -r requirements.txt
```

Key packages: weasyprint, PyMuPDF, openpyxl, pandas, pillow

## Installation

1. Install SBCL:

   ```bash
   sudo apt install sbcl
   ```

2. Install Quicklisp (if not already installed):

   ```bash
   curl -O https://beta.quicklisp.org/quicklisp.lisp
   sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
   ```

3. Link this project to Quicklisp local-projects:

   ```bash
   ln -s /path/to/tfs-cmms ~/quicklisp/local-projects/
   ```

4. Load and start:

   ```lisp
   (ql:quickload :tfs-cmms)
   (tfs-cmms:start-server)
   ```

## Usage

### Starting the Server

```lisp
(ql:quickload :tfs-cmms)
(tfs-cmms:start-server :port 8080)
```

Then open <http://localhost:8080> in your browser.

### API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/work-orders/list` | GET/POST | List work orders |
| `/api/work-orders/get/:id` | GET | Get single work order |
| `/api/work-orders/create` | POST | Create new work order |
| `/api/work-orders/update/:id` | POST | Update work order |
| `/api/sites/list` | GET | List all sites |
| `/api/sites/create` | POST | Create new site |
| `/api/facilities/list` | GET | List facilities |
| `/api/inventory/list` | GET | Get inventory summary |
| `/api/inventory/transaction` | POST | Record inventory transaction |
| `/api/reports/weekly` | POST | Generate weekly report |

### Web Pages

- `/` - Dashboard
- `/inspection-reports` - Inspection report list and management
- `/work-orders` - Work order list and management
- `/sites` - Site management
- `/inventory` - Inventory management
- `/mrf` - Material Request Forms
- `/dar` - Daily Activity Reports
- `/irp` - Immediate Repair Package
- `/sar` - Site Activity Report (Admin/AO Lead only)
- `/rr` - R&R Leave Management
- `/master-tracker` - Deficiency tracking
- `/reports` - Weekly report generation
- `/admin/users` - User management (Admin only)

## Database

SQLite database stored at `data/tfs-cmms.db`. The schema is auto-initialized on first run.

### Key Tables

- `sites` - Base/site locations
- `facilities` - Buildings/facilities per site
- `assets` - Equipment/systems
- `work_orders` - Maintenance actions
- `wo_history` - Audit trail
- `stock_items` - Inventory catalog
- `inventory` - On-hand quantities by site
- `inventory_transactions` - Issue/receipt log
- `job_plans` - Work templates

## PWS Compliance

This system addresses PWS Section 17.0 requirements:

| Requirement | Implementation |
|-------------|----------------|
| Track/manage work performed | Work order system with full CRUD |
| Work planning & scheduling | Target dates, assigned teams |
| Repair work order management | Status workflow, progress tracking |
| Inventory management (burn rates) | Transaction log, auto-calculated burn rates |
| Scheduling | Target start/completion dates |
| CLIN and capacity tracking | CLIN field on WOs, summary reports |
| Work orders by site/facility/equipment | Full asset hierarchy |
| Hierarchical relationships | Site → Facility → System → Asset |
| Specs, warranty, manufacturer data | Asset records with all fields |
| Job plans with materials | Job plan templates with BOM |
| Weekly per-site status report | Automated report generation |

## Documentation

Detailed documentation is available in Typst format:

- **User Guide.typ** - End-user documentation for all roles
- **Testing Procedure.typ** - Test cases and verification procedures
- **Deployment Guide.typ** - Server setup, installation, and maintenance

## License

Proprietary - Task Force SAFE Program
