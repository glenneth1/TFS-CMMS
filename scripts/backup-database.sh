#!/bin/bash
# TFS CMMS Database Backup Script
# Performs safe SQLite backup with rotation policy

set -e

# Configuration
DB_PATH="/opt/tfs-cmms/data/tfs-cmms.db"
BACKUP_DIR="/opt/tfs-cmms/backups"
DATE=$(date +%Y-%m-%d)
DAY_OF_WEEK=$(date +%u)  # 1=Monday, 7=Sunday
DAY_OF_MONTH=$(date +%d)
TIMESTAMP=$(date +%Y-%m-%d_%H%M%S)

# Retention settings
DAILY_KEEP=7
WEEKLY_KEEP=4
MONTHLY_KEEP=3

# Create backup directories
mkdir -p "$BACKUP_DIR/daily"
mkdir -p "$BACKUP_DIR/weekly"
mkdir -p "$BACKUP_DIR/monthly"

# Perform safe backup using sqlite3 .backup command
# This is safe even while the database is in use
DAILY_BACKUP="$BACKUP_DIR/daily/tfs-cmms-$DATE.db"
echo "[$(date)] Starting daily backup..."
sqlite3 "$DB_PATH" ".backup '$DAILY_BACKUP'"
gzip -f "$DAILY_BACKUP"
echo "[$(date)] Daily backup complete: ${DAILY_BACKUP}.gz ($(du -h "${DAILY_BACKUP}.gz" | cut -f1))"

# Weekly backup (on Sundays)
if [ "$DAY_OF_WEEK" -eq 7 ]; then
    WEEKLY_BACKUP="$BACKUP_DIR/weekly/tfs-cmms-week-$DATE.db.gz"
    cp "${DAILY_BACKUP}.gz" "$WEEKLY_BACKUP"
    echo "[$(date)] Weekly backup created: $WEEKLY_BACKUP"
fi

# Monthly backup (on 1st of month)
if [ "$DAY_OF_MONTH" -eq "01" ]; then
    MONTHLY_BACKUP="$BACKUP_DIR/monthly/tfs-cmms-month-$DATE.db.gz"
    cp "${DAILY_BACKUP}.gz" "$MONTHLY_BACKUP"
    echo "[$(date)] Monthly backup created: $MONTHLY_BACKUP"
fi

# Cleanup old backups
echo "[$(date)] Cleaning up old backups..."

# Remove daily backups older than DAILY_KEEP days
find "$BACKUP_DIR/daily" -name "*.gz" -mtime +$DAILY_KEEP -delete 2>/dev/null || true

# Remove weekly backups older than WEEKLY_KEEP weeks
find "$BACKUP_DIR/weekly" -name "*.gz" -mtime +$((WEEKLY_KEEP * 7)) -delete 2>/dev/null || true

# Remove monthly backups older than MONTHLY_KEEP months (approx 90 days)
find "$BACKUP_DIR/monthly" -name "*.gz" -mtime +$((MONTHLY_KEEP * 30)) -delete 2>/dev/null || true

# Show current backup status
echo ""
echo "=== Backup Status ==="
echo "Daily backups:   $(ls -1 $BACKUP_DIR/daily/*.gz 2>/dev/null | wc -l) files"
echo "Weekly backups:  $(ls -1 $BACKUP_DIR/weekly/*.gz 2>/dev/null | wc -l) files"
echo "Monthly backups: $(ls -1 $BACKUP_DIR/monthly/*.gz 2>/dev/null | wc -l) files"
echo "Total size:      $(du -sh $BACKUP_DIR | cut -f1)"
echo ""
echo "[$(date)] Backup complete!"
