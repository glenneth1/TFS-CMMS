// TFS-CMMS Deployment Guide

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
      TFS-CMMS Deployment Guide
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
  #text(22pt, weight: "bold")[Deployment Guide]
  #v(0.5cm)
  #text(16pt)[Server Requirements & Installation]
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

= Executive Summary

This document provides technical specifications and deployment instructions for the Task Force SAFE Computerized Maintenance Management System (TFS-CMMS). The system is a web-based application designed for managing electrical inspection reports, deficiency tracking, and operational reporting across multiple international sites.

== System Architecture

TFS-CMMS is built on the following technology stack:

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Component*], [*Technology*],
  [Backend], [Common Lisp (SBCL - Steel Bank Common Lisp)],
  [Web Server], [Hunchentoot (embedded HTTP server)],
  [Database], [SQLite 3 (file-based, serverless)],
  [Report Generation], [Python 3.x with matplotlib, pandas],
  [PDF Generation], [WeasyPrint (Python)],
  [Frontend], [HTML5, CSS3, JavaScript (vanilla)],
)

== Key Benefits

- *Lightweight* — No external database server required
- *Portable* — Single directory deployment
- *Low Maintenance* — Minimal system administration
- *Secure* — Role-based access control, session management
- *Offline Capable* — Can operate on isolated networks

#pagebreak()

= Server Requirements

== Minimum Specifications

For a small deployment (up to 20 concurrent users, <10,000 inspection records):

#table(
  columns: (auto, auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Resource*], [*Minimum*], [*Notes*],
  [CPU], [2 cores], [x86-64 architecture required],
  [RAM], [4 GB], [2 GB for OS, 2 GB for application],
  [Storage], [20 GB SSD], [10 GB system, 10 GB data/uploads],
  [Network], [100 Mbps], [Internal network access],
)

== Recommended Specifications

For production deployment (50+ concurrent users, 50,000+ records):

#table(
  columns: (auto, auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Resource*], [*Recommended*], [*Notes*],
  [CPU], [4 cores], [Intel Xeon or AMD EPYC recommended],
  [RAM], [8 GB], [Allows for growth and caching],
  [Storage], [100 GB SSD], [NVMe preferred for database performance],
  [Network], [1 Gbps], [For file uploads and report downloads],
)

#block(
  fill: rgb("#cce5ff"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Storage Calculation:* 
  - Base system: ~500 MB
  - Database: ~1 MB per 1,000 deficiency records
  - Uploads: ~2 MB average per inspection report (images)
  - Reports: ~5 MB per weekly report set
  
  For 10,000 inspections over 2 years: ~25 GB recommended
]

== Operating System

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*OS*], [*Compatibility*],
  [Ubuntu 22.04 LTS], [Recommended - fully tested],
  [Ubuntu 24.04 LTS], [Supported],
  [Debian 11/12], [Supported],
  [RHEL 8/9], [Supported with additional configuration],
  [Windows Server], [Not recommended - requires WSL2],
)

#pagebreak()

= Software Dependencies

== Required Packages

=== Steel Bank Common Lisp (SBCL)

The primary runtime environment for the application.

```bash
# Ubuntu/Debian
sudo apt install sbcl

# Or download from sbcl.org for latest version
```

Version: 2.2.0 or later recommended

=== Quicklisp

Common Lisp package manager for dependencies.

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql:add-to-init-file)' \
     --quit
```

=== Python 3

Required for report generation and PDF export.

```bash
sudo apt install python3 python3-pip python3-venv
```

Version: 3.10 or later

=== Python Packages

The application expects a Python virtual environment at `../.venv` relative to the application directory (i.e., one level up from the tfs-cmms folder).

```bash
# Create virtual environment (from parent directory)
cd ~/tfs-cmms
python3 -m venv ../.venv

# Activate and install dependencies
source ../.venv/bin/activate
pip install -r requirements.txt
```

The `requirements.txt` file includes:
- *weasyprint* — PDF generation for DAR and SAR reports
- *PyMuPDF* — PDF processing
- *openpyxl* — Excel spreadsheet processing
- *pandas* — Data analysis and manipulation
- *pillow* — Image processing
- *matplotlib* — Chart generation for reports

#block(
  fill: rgb("#fff3cd"),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
)[
  *Note:* The application automatically detects the virtual environment at `../.venv/bin/python3`. If not found, it falls back to the system `python3` command.
]

=== System Libraries

```bash
# For WeasyPrint PDF generation
sudo apt install libpango-1.0-0 libpangocairo-1.0-0 \
                 libgdk-pixbuf2.0-0 libffi-dev \
                 libcairo2 libgirepository1.0-dev

# For image processing
sudo apt install libjpeg-dev libpng-dev
```

#pagebreak()

= Installation Procedure

== Step 1: Create Application User

```bash
sudo useradd -m -s /bin/bash tfs-cmms
sudo passwd tfs-cmms
```

== Step 2: Clone Repository

```bash
sudo -u tfs-cmms -i
git clone https://github.com/glenneth1/TFS-CMMS.git ~/tfs-cmms
cd ~/tfs-cmms
```

== Step 3: Install Dependencies

```bash
# Install SBCL and Quicklisp (as tfs-cmms user)
# Follow dependency installation from previous section

# Create Python virtual environment (one level up from app directory)
cd ~/tfs-cmms
python3 -m venv ../.venv
source ../.venv/bin/activate
pip install -r requirements.txt
```

== Step 4: Configure Application

```bash
# Create data and reports directories
mkdir -p data uploads/buildings uploads/deficiencies
mkdir -p reports/dar reports/sar

# Set permissions
chmod 755 data uploads reports
```

== Step 5: Initialize Database

The database is automatically initialized on first run. No manual setup required.

== Step 6: Start Application

```bash
# Development/Testing
sbcl --load ~/quicklisp/setup.lisp \
     --eval '(push #p"~/tfs-cmms/" asdf:*central-registry*)' \
     --eval '(ql:quickload :tfs-cmms)' \
     --eval '(tfs-cmms:start-server :port 8080)'
```

#pagebreak()

= Production Deployment

== Systemd Service

Create a systemd service for automatic startup and management.

=== Service File

Create `/etc/systemd/system/tfs-cmms.service`:

```ini
[Unit]
Description=TFS-CMMS Web Application
After=network.target

[Service]
Type=simple
User=tfs-cmms
Group=tfs-cmms
WorkingDirectory=/home/tfs-cmms/tfs-cmms
ExecStart=/usr/bin/sbcl --noinform \
    --load /home/tfs-cmms/quicklisp/setup.lisp \
    --eval '(push #p"/home/tfs-cmms/tfs-cmms/" asdf:*central-registry*)' \
    --eval '(ql:quickload :tfs-cmms)' \
    --eval '(tfs-cmms:start-server :port 8080)' \
    --eval '(loop (sleep 3600))'
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

=== Enable Service

```bash
sudo systemctl daemon-reload
sudo systemctl enable tfs-cmms
sudo systemctl start tfs-cmms
```

=== Service Management

```bash
# Check status
sudo systemctl status tfs-cmms

# View logs
sudo journalctl -u tfs-cmms -f

# Restart after updates
sudo systemctl restart tfs-cmms
```

== Reverse Proxy (Nginx)

For production, use Nginx as a reverse proxy for SSL termination and static file serving.

=== Nginx Configuration

Create `/etc/nginx/sites-available/tfs-cmms`:

```nginx
server {
    listen 80;
    server_name cmms.example.com;
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name cmms.example.com;

    ssl_certificate /etc/ssl/certs/cmms.crt;
    ssl_certificate_key /etc/ssl/private/cmms.key;

    # Security headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";

    # Static files
    location /static/ {
        alias /home/tfs-cmms/tfs-cmms/static/;
        expires 7d;
    }

    location /uploads/ {
        alias /home/tfs-cmms/tfs-cmms/uploads/;
        expires 1d;
    }

    # Proxy to application
    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Timeouts for large uploads
        proxy_read_timeout 300;
        proxy_connect_timeout 300;
        client_max_body_size 50M;
    }
}
```

=== Enable Site

```bash
sudo ln -s /etc/nginx/sites-available/tfs-cmms \
           /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

#pagebreak()

= Backup and Recovery

== Database Backup

The SQLite database is a single file that can be backed up while the application is running.

=== Manual Backup

```bash
# Create backup directory
mkdir -p /backup/tfs-cmms

# Backup database
sqlite3 /home/tfs-cmms/tfs-cmms/data/tfs-cmms.db \
        ".backup /backup/tfs-cmms/tfs-cmms-$(date +%Y%m%d).db"

# Backup uploads
tar -czf /backup/tfs-cmms/uploads-$(date +%Y%m%d).tar.gz \
         /home/tfs-cmms/tfs-cmms/uploads/
```

=== Automated Backup Script

Create `/home/tfs-cmms/backup.sh`:

```bash
#!/bin/bash
BACKUP_DIR="/backup/tfs-cmms"
DATE=$(date +%Y%m%d_%H%M%S)
APP_DIR="/home/tfs-cmms/tfs-cmms"

# Create backup directory
mkdir -p $BACKUP_DIR

# Backup database
sqlite3 $APP_DIR/data/tfs-cmms.db ".backup $BACKUP_DIR/db_$DATE.db"

# Backup uploads
tar -czf $BACKUP_DIR/uploads_$DATE.tar.gz $APP_DIR/uploads/

# Keep only last 30 days
find $BACKUP_DIR -name "*.db" -mtime +30 -delete
find $BACKUP_DIR -name "*.tar.gz" -mtime +30 -delete
```

=== Cron Schedule

```bash
# Run daily at 2 AM
0 2 * * * /home/tfs-cmms/backup.sh
```

== Recovery Procedure

```bash
# Stop application
sudo systemctl stop tfs-cmms

# Restore database
cp /backup/tfs-cmms/db_YYYYMMDD.db \
   /home/tfs-cmms/tfs-cmms/data/tfs-cmms.db

# Restore uploads
tar -xzf /backup/tfs-cmms/uploads_YYYYMMDD.tar.gz -C /

# Fix permissions
chown -R tfs-cmms:tfs-cmms /home/tfs-cmms/tfs-cmms/

# Start application
sudo systemctl start tfs-cmms
```

#pagebreak()

= Security Considerations

== Network Security

- Deploy behind a firewall
- Use HTTPS with valid SSL certificates
- Restrict access to internal network or VPN
- Consider IP whitelisting for sensitive operations

== Application Security

- Change default admin password immediately after deployment
- Use strong passwords (minimum 12 characters)
- Review user accounts regularly
- Deactivate accounts for departed personnel

== File Permissions

```bash
# Application files (read-only)
chmod -R 755 /home/tfs-cmms/tfs-cmms/src/
chmod -R 755 /home/tfs-cmms/tfs-cmms/static/

# Data directories (read-write)
chmod 755 /home/tfs-cmms/tfs-cmms/data/
chmod 755 /home/tfs-cmms/tfs-cmms/uploads/
chmod 755 /home/tfs-cmms/tfs-cmms/reports/

# Database file
chmod 644 /home/tfs-cmms/tfs-cmms/data/tfs-cmms.db
```

== Audit Logging

The application logs all requests to stdout/stderr. Configure systemd to capture logs:

```bash
# View recent logs
sudo journalctl -u tfs-cmms --since "1 hour ago"

# Export logs for audit
sudo journalctl -u tfs-cmms --since "2025-01-01" > audit_log.txt
```

#pagebreak()

= Performance Tuning

== SBCL Memory Settings

For high-load environments, increase SBCL heap size:

```bash
# In systemd service file, modify ExecStart:
ExecStart=/usr/bin/sbcl --dynamic-space-size 2048 --noinform ...
```

== SQLite Optimization

The application uses SQLite with WAL mode for better concurrent access. For very high loads, consider:

```sql
-- Run once to optimize
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
PRAGMA cache_size = -64000;  -- 64MB cache
PRAGMA temp_store = MEMORY;
```

== Nginx Caching

Enable caching for static assets:

```nginx
location /static/ {
    alias /home/tfs-cmms/tfs-cmms/static/;
    expires 30d;
    add_header Cache-Control "public, immutable";
}
```

#pagebreak()

= Monitoring

== Health Check Endpoint

The application responds to HTTP requests on the configured port. Use this for monitoring:

```bash
# Simple health check
curl -s -o /dev/null -w "%{http_code}" http://localhost:8080/

# Expected: 200 (or 302 redirect to login)
```

== Disk Space Monitoring

Monitor these directories:

- `/home/tfs-cmms/tfs-cmms/data/` — Database growth
- `/home/tfs-cmms/tfs-cmms/uploads/` — Image uploads
- `/home/tfs-cmms/tfs-cmms/reports/` — Generated reports

== Resource Monitoring

```bash
# Check application memory usage
ps aux | grep sbcl

# Check database size
du -h /home/tfs-cmms/tfs-cmms/data/tfs-cmms.db

# Check upload directory size
du -sh /home/tfs-cmms/tfs-cmms/uploads/
```

#pagebreak()

= Upgrade Procedure

== Standard Upgrade

```bash
# Stop application
sudo systemctl stop tfs-cmms

# Backup current version
cp -r /home/tfs-cmms/tfs-cmms /home/tfs-cmms/tfs-cmms.backup

# Pull latest code
cd /home/tfs-cmms/tfs-cmms
git pull origin main

# Clear compiled cache
rm -rf ~/.cache/common-lisp/sbcl-*/

# Start application
sudo systemctl start tfs-cmms

# Verify operation
curl http://localhost:8080/
```

== Rollback Procedure

```bash
# Stop application
sudo systemctl stop tfs-cmms

# Restore backup
rm -rf /home/tfs-cmms/tfs-cmms
mv /home/tfs-cmms/tfs-cmms.backup /home/tfs-cmms/tfs-cmms

# Clear cache and restart
rm -rf ~/.cache/common-lisp/sbcl-*/
sudo systemctl start tfs-cmms
```

#pagebreak()

= Appendix A: Port Configuration

The default port is 8080. To change:

```lisp
;; In start command
(tfs-cmms:start-server :port 8443)
```

Or set environment variable:

```bash
export TFS_CMMS_PORT=8443
```

= Appendix B: Troubleshooting

== Application Won't Start

+ Check SBCL is installed: `sbcl --version`
+ Check Quicklisp is installed: `ls ~/quicklisp/`
+ Check for syntax errors in logs: `journalctl -u tfs-cmms`
+ Verify port is available: `netstat -tlnp | grep 8080`

== Database Errors

+ Check file permissions on `data/` directory
+ Verify SQLite is installed: `sqlite3 --version`
+ Check disk space: `df -h`

== Upload Failures

+ Check `uploads/` directory permissions
+ Verify disk space available
+ Check Nginx `client_max_body_size` setting

== PDF Generation Fails

+ Verify Python virtual environment is activated
+ Check WeasyPrint dependencies are installed
+ Test manually: `python3 scripts/generate_pdf.py --test`

#pagebreak()

= Document Control

#table(
  columns: (auto, auto, auto, 1fr),
  stroke: 0.5pt,
  inset: 8pt,
  [*Version*], [*Date*], [*Author*], [*Changes*],
  [1.0], [Dec 2025], [TFS Team], [Initial release],
)
