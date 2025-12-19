# TF SAFE CMMS

**Task Force SAFE - Computerized Maintenance Management System**

A self-hosted CMMS built with Common Lisp and the Radiance web framework, designed to meet PWS Section 17.0 requirements for the Task Force SAFE program.

## Features

- **Work Order Management**: Create, track, and close work orders with auto-generated WO numbers
- **Asset Hierarchy**: Site → Facility → System → Asset → Component structure
- **Inventory Tracking**: Stock items, transactions, burn rate calculations, reorder alerts
- **Weekly Reports**: Per-site status reports matching PWS requirements
- **Audit Trail**: Full change history on all work orders
- **Job Plans**: Reusable templates with steps and default materials

## WO Number Format

Work orders are automatically numbered:
```
TFS-<SITE_CODE>-<YEAR>-<SEQUENCE>
Example: TFS-AJ-2025-00001
```

## Requirements

- SBCL (Steel Bank Common Lisp) or another Common Lisp implementation
- Quicklisp package manager
- SQLite3

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

Then open http://localhost:8080 in your browser.

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

- `/work-orders` - Work order list and management
- `/work-orders/new` - Create new work order
- `/work-orders/:id` - Work order detail/edit
- `/sites` - Site management
- `/inventory` - Inventory management
- `/reports` - Weekly report generation

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

## License

Proprietary - Task Force SAFE Program
