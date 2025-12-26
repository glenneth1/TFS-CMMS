#!/bin/bash
# Pull backups from production server to local machine
# Run this periodically to maintain off-site backups

set -e

LOCAL_BACKUP_DIR="/home/glenn/Notes/org/TFS/CMMS/tfs-cmms/backups"
REMOTE_HOST="tfs.is"
REMOTE_BACKUP_DIR="/opt/tfs-cmms/backups"

echo "[$(date)] Pulling backups from production..."

# Sync all backups from production
rsync -avz --progress "$REMOTE_HOST:$REMOTE_BACKUP_DIR/" "$LOCAL_BACKUP_DIR/"

echo ""
echo "=== Local Backup Status ==="
echo "Daily backups:   $(ls -1 $LOCAL_BACKUP_DIR/daily/*.gz 2>/dev/null | wc -l) files"
echo "Weekly backups:  $(ls -1 $LOCAL_BACKUP_DIR/weekly/*.gz 2>/dev/null | wc -l) files"
echo "Monthly backups: $(ls -1 $LOCAL_BACKUP_DIR/monthly/*.gz 2>/dev/null | wc -l) files"
echo "Total size:      $(du -sh $LOCAL_BACKUP_DIR | cut -f1)"
echo ""
echo "[$(date)] Backup sync complete!"
