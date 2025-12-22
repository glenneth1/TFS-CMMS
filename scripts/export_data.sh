#!/bin/bash
# Export key data tables from TFS-CMMS database
# Usage: ./scripts/export_data.sh [output_dir]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
DB_PATH="$PROJECT_DIR/data/tfs-cmms.db"
OUTPUT_DIR="${1:-$PROJECT_DIR/data/exports}"
DATE=$(date +%Y%m%d_%H%M%S)

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo "Exporting data from $DB_PATH..."
echo "Output directory: $OUTPUT_DIR"

# Export tables as CSV
sqlite3 -header -csv "$DB_PATH" "SELECT * FROM master_deficiencies;" > "$OUTPUT_DIR/master_deficiencies_$DATE.csv"
sqlite3 -header -csv "$DB_PATH" "SELECT * FROM inventory_items;" > "$OUTPUT_DIR/inventory_items_$DATE.csv"
sqlite3 -header -csv "$DB_PATH" "SELECT * FROM inventory_locations;" > "$OUTPUT_DIR/inventory_locations_$DATE.csv"
sqlite3 -header -csv "$DB_PATH" "SELECT * FROM users;" > "$OUTPUT_DIR/users_$DATE.csv"
sqlite3 -header -csv "$DB_PATH" "SELECT * FROM sites;" > "$OUTPUT_DIR/sites_$DATE.csv"
sqlite3 -header -csv "$DB_PATH" "SELECT * FROM camps;" > "$OUTPUT_DIR/camps_$DATE.csv"
sqlite3 -header -csv "$DB_PATH" "SELECT * FROM inspection_reports;" > "$OUTPUT_DIR/inspection_reports_$DATE.csv"
sqlite3 -header -csv "$DB_PATH" "SELECT * FROM rr_requests;" > "$OUTPUT_DIR/rr_requests_$DATE.csv"

# Also create a full database backup
cp "$DB_PATH" "$OUTPUT_DIR/tfs-cmms_$DATE.db"

echo ""
echo "Export complete!"
echo "Files created:"
ls -lh "$OUTPUT_DIR"/*_$DATE.*

echo ""
echo "To import on a new deployment:"
echo "  1. Copy the .db file to data/tfs-cmms.db"
echo "  OR"
echo "  2. Use the CSV files with: sqlite3 data/tfs-cmms.db '.import file.csv table_name'"
