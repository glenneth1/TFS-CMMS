#!/bin/bash
# TFS CMMS Database Restore Script
# Restores database from a backup file with safety checks

set -e

# Configuration
DB_PATH="/opt/tfs-cmms/data/tfs-cmms.db"
BACKUP_DIR="/opt/tfs-cmms/backups"
SERVICE_NAME="tfs-cmms"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo ""
echo "=========================================="
echo "  TFS CMMS Database Restore Utility"
echo "=========================================="
echo ""

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo -e "${RED}Error: This script must be run as root (use sudo)${NC}"
    exit 1
fi

# Function to list available backups
list_backups() {
    echo "Available backups:"
    echo ""
    echo "=== Daily Backups ==="
    ls -lh "$BACKUP_DIR/daily/"*.gz 2>/dev/null | awk '{print NR". "$9" ("$5")"}' || echo "  (none)"
    echo ""
    echo "=== Weekly Backups ==="
    ls -lh "$BACKUP_DIR/weekly/"*.gz 2>/dev/null | awk '{print NR". "$9" ("$5")"}' || echo "  (none)"
    echo ""
    echo "=== Monthly Backups ==="
    ls -lh "$BACKUP_DIR/monthly/"*.gz 2>/dev/null | awk '{print NR". "$9" ("$5")"}' || echo "  (none)"
    echo ""
}

# Check for backup file argument
if [ -z "$1" ]; then
    echo -e "${YELLOW}Usage: $0 <backup-file.db.gz>${NC}"
    echo ""
    list_backups
    echo "Example:"
    echo "  sudo $0 $BACKUP_DIR/daily/tfs-cmms-2025-12-25.db.gz"
    echo ""
    exit 1
fi

BACKUP_FILE="$1"

# Verify backup file exists
if [ ! -f "$BACKUP_FILE" ]; then
    echo -e "${RED}Error: Backup file not found: $BACKUP_FILE${NC}"
    exit 1
fi

# Verify it's a gzipped file
if [[ ! "$BACKUP_FILE" == *.gz ]]; then
    echo -e "${RED}Error: Backup file must be a .gz file${NC}"
    exit 1
fi

echo -e "${YELLOW}WARNING: This will replace the current database!${NC}"
echo ""
echo "Backup file: $BACKUP_FILE"
echo "Database:    $DB_PATH"
echo ""

# Show backup file info
echo "Backup details:"
ls -lh "$BACKUP_FILE"
echo ""

# Confirm restore
read -p "Are you sure you want to restore this backup? (yes/no): " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
    echo "Restore cancelled."
    exit 0
fi

echo ""
echo "[$(date)] Starting database restore..."

# Step 1: Stop the service
echo "[$(date)] Stopping $SERVICE_NAME service..."
systemctl stop "$SERVICE_NAME"
sleep 2

# Step 2: Create a safety backup of current database
SAFETY_BACKUP="$DB_PATH.pre-restore-$(date +%Y%m%d_%H%M%S)"
echo "[$(date)] Creating safety backup of current database..."
cp "$DB_PATH" "$SAFETY_BACKUP"
echo "  Safety backup: $SAFETY_BACKUP"

# Step 3: Decompress and restore
echo "[$(date)] Decompressing backup..."
TEMP_DB="/tmp/tfs-cmms-restore-$$.db"
gunzip -c "$BACKUP_FILE" > "$TEMP_DB"

# Step 4: Verify the backup is a valid SQLite database
echo "[$(date)] Verifying backup integrity..."
if ! sqlite3 "$TEMP_DB" "PRAGMA integrity_check;" | grep -q "ok"; then
    echo -e "${RED}Error: Backup file failed integrity check!${NC}"
    rm -f "$TEMP_DB"
    systemctl start "$SERVICE_NAME"
    exit 1
fi

# Step 5: Replace the database
echo "[$(date)] Replacing database..."
mv "$TEMP_DB" "$DB_PATH"
chown 1000:1000 "$DB_PATH"
chmod 644 "$DB_PATH"

# Step 6: Restart the service
echo "[$(date)] Starting $SERVICE_NAME service..."
systemctl start "$SERVICE_NAME"
sleep 3

# Step 7: Verify service is running
if systemctl is-active --quiet "$SERVICE_NAME"; then
    echo -e "${GREEN}[$(date)] Restore complete! Service is running.${NC}"
    echo ""
    echo "Safety backup saved at: $SAFETY_BACKUP"
    echo "You can delete it once you've verified the restore was successful."
else
    echo -e "${RED}[$(date)] Warning: Service may not have started correctly.${NC}"
    echo "Check status with: systemctl status $SERVICE_NAME"
    echo ""
    echo "To rollback, run:"
    echo "  sudo systemctl stop $SERVICE_NAME"
    echo "  sudo cp $SAFETY_BACKUP $DB_PATH"
    echo "  sudo systemctl start $SERVICE_NAME"
fi

echo ""
echo "[$(date)] Done!"
