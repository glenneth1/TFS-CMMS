#!/usr/bin/env python3
"""
Import inventory data from Inventory.xlsx into the TFS-CMMS database.

This script imports the EBOM (Electrical Bill of Materials) inventory data,
creating location records and inventory items with their quantities and costs.
"""

import sqlite3
import pandas as pd
import os
import sys
from datetime import datetime

# Database path
DB_PATH = os.path.join(os.path.dirname(__file__), '..', 'data', 'tfs-cmms.db')
EXCEL_PATH = os.path.join(os.path.dirname(__file__), '..', 'Inventory.xlsx')


def get_connection():
    """Get database connection."""
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn


def get_or_create_location(conn, location_code):
    """Get or create an inventory location by code."""
    cursor = conn.cursor()
    
    # Check if location exists
    cursor.execute("SELECT id FROM inventory_locations WHERE location_code = ?", (location_code,))
    row = cursor.fetchone()
    
    if row:
        return row[0]
    
    # Determine location type based on code
    location_type = 'Container'
    if 'CRATE' in location_code.upper():
        location_type = 'Crate'
    elif 'YARD' in location_code.upper():
        location_type = 'Yard'
    elif location_code.upper() in ['NLZ', 'ERBIL', 'SHARURAH']:
        location_type = 'Site'
    
    # Create location name from code
    location_name = location_code.replace('CON ', 'Container ').replace('CON', 'Container ')
    
    cursor.execute("""
        INSERT INTO inventory_locations (location_code, location_name, location_type)
        VALUES (?, ?, ?)
    """, (location_code, location_name, location_type))
    
    return cursor.lastrowid


def import_inventory(conn, df):
    """Import inventory items from DataFrame."""
    cursor = conn.cursor()
    
    # Track statistics
    stats = {
        'locations_created': 0,
        'items_imported': 0,
        'items_updated': 0,
        'errors': 0
    }
    
    # Get existing locations count
    cursor.execute("SELECT COUNT(*) FROM inventory_locations")
    locations_before = cursor.fetchone()[0]
    
    for idx, row in df.iterrows():
        try:
            # Skip rows with no item number
            if pd.isna(row['#']):
                continue
            
            item_number = int(row['#'])
            description = str(row['ITEM DESCRIPTION']).strip() if pd.notna(row['ITEM DESCRIPTION']) else ''
            make = str(row['MAKE']).strip() if pd.notna(row['MAKE']) else ''
            part_number = str(row['PART NUMBER']).strip() if pd.notna(row['PART NUMBER']) else ''
            uom = str(row['UOM']).strip() if pd.notna(row['UOM']) else 'EA'
            quantity = float(row['QTY']) if pd.notna(row['QTY']) else 0
            unit_cost = float(row['UNIT COST']) if pd.notna(row['UNIT COST']) else 0
            total_cost = float(row['TOTAL COST']) if pd.notna(row['TOTAL COST']) else 0
            container = str(row['CONTAINER #']).strip() if pd.notna(row['CONTAINER #']) else 'UNKNOWN'
            property_type = str(row['PROPERTY TYPE']).strip() if pd.notna(row['PROPERTY TYPE']) else 'Material'
            property_usage = str(row['PROPERTY USAGE']).strip() if pd.notna(row['PROPERTY USAGE']) else 'Consume'
            
            # Get or create location
            location_id = get_or_create_location(conn, container)
            
            # Check if item already exists (by item_number)
            cursor.execute("SELECT id FROM inventory_items WHERE item_number = ?", (item_number,))
            existing = cursor.fetchone()
            
            if existing:
                # Update existing item
                cursor.execute("""
                    UPDATE inventory_items 
                    SET description = ?, make = ?, part_number = ?, uom = ?,
                        quantity = ?, unit_cost = ?, total_cost = ?,
                        location_id = ?, property_type = ?, property_usage = ?,
                        updated_at = datetime('now')
                    WHERE id = ?
                """, (description, make, part_number, uom, quantity, unit_cost, 
                      total_cost, location_id, property_type, property_usage, existing[0]))
                stats['items_updated'] += 1
            else:
                # Insert new item
                cursor.execute("""
                    INSERT INTO inventory_items 
                    (item_number, description, make, part_number, uom, quantity,
                     unit_cost, total_cost, location_id, property_type, property_usage)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (item_number, description, make, part_number, uom, quantity,
                      unit_cost, total_cost, location_id, property_type, property_usage))
                stats['items_imported'] += 1
                
        except Exception as e:
            print(f"Error importing row {idx}: {e}")
            stats['errors'] += 1
            continue
    
    # Calculate locations created
    cursor.execute("SELECT COUNT(*) FROM inventory_locations")
    locations_after = cursor.fetchone()[0]
    stats['locations_created'] = locations_after - locations_before
    
    conn.commit()
    return stats


def print_summary(conn):
    """Print summary of imported data."""
    cursor = conn.cursor()
    
    print("\n" + "="*60)
    print("INVENTORY IMPORT SUMMARY")
    print("="*60)
    
    # Total items
    cursor.execute("SELECT COUNT(*) FROM inventory_items")
    total_items = cursor.fetchone()[0]
    print(f"\nTotal inventory items: {total_items}")
    
    # Total value
    cursor.execute("SELECT SUM(total_cost) FROM inventory_items")
    total_value = cursor.fetchone()[0] or 0
    print(f"Total inventory value: ${total_value:,.2f}")
    
    # By property type
    print("\nBy Property Type:")
    cursor.execute("""
        SELECT property_type, COUNT(*) as count, SUM(total_cost) as value
        FROM inventory_items
        GROUP BY property_type
    """)
    for row in cursor.fetchall():
        print(f"  {row[0]}: {row[1]} items, ${row[2]:,.2f}")
    
    # By property usage
    print("\nBy Property Usage:")
    cursor.execute("""
        SELECT property_usage, COUNT(*) as count, SUM(total_cost) as value
        FROM inventory_items
        GROUP BY property_usage
    """)
    for row in cursor.fetchall():
        print(f"  {row[0]}: {row[1]} items, ${row[2]:,.2f}")
    
    # Locations
    print("\nInventory Locations:")
    cursor.execute("""
        SELECT l.location_code, l.location_type, COUNT(i.id) as item_count,
               COALESCE(SUM(i.total_cost), 0) as total_value
        FROM inventory_locations l
        LEFT JOIN inventory_items i ON i.location_id = l.id
        GROUP BY l.id
        ORDER BY item_count DESC
        LIMIT 15
    """)
    for row in cursor.fetchall():
        print(f"  {row[0]} ({row[1]}): {row[2]} items, ${row[3]:,.2f}")
    
    # UOM distribution
    print("\nUnits of Measure:")
    cursor.execute("""
        SELECT uom, COUNT(*) as count
        FROM inventory_items
        GROUP BY uom
        ORDER BY count DESC
    """)
    for row in cursor.fetchall():
        print(f"  {row[0]}: {row[1]} items")
    
    print("\n" + "="*60)


def main():
    """Main import function."""
    print("TFS-CMMS Inventory Import")
    print("="*60)
    
    # Check if Excel file exists
    if not os.path.exists(EXCEL_PATH):
        print(f"Error: Excel file not found at {EXCEL_PATH}")
        sys.exit(1)
    
    print(f"Reading from: {EXCEL_PATH}")
    
    # Read Excel file
    try:
        df = pd.read_excel(EXCEL_PATH, sheet_name='TF SAFE EBOM - ALL')
        print(f"Found {len(df)} rows in spreadsheet")
    except Exception as e:
        print(f"Error reading Excel file: {e}")
        sys.exit(1)
    
    # Connect to database
    conn = get_connection()
    
    # Ensure tables exist (run schema creation)
    print("\nEnsuring database tables exist...")
    
    # Create tables if they don't exist
    conn.execute("""
        CREATE TABLE IF NOT EXISTS inventory_locations (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            location_code TEXT NOT NULL UNIQUE,
            location_name TEXT,
            location_type TEXT DEFAULT 'Container',
            site_id INTEGER,
            notes TEXT,
            created_at TEXT DEFAULT (datetime('now')),
            updated_at TEXT DEFAULT (datetime('now'))
        )
    """)
    
    conn.execute("""
        CREATE TABLE IF NOT EXISTS inventory_items (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            item_number INTEGER NOT NULL,
            description TEXT NOT NULL,
            make TEXT,
            part_number TEXT,
            uom TEXT NOT NULL,
            quantity REAL DEFAULT 0,
            unit_cost REAL DEFAULT 0,
            total_cost REAL DEFAULT 0,
            location_id INTEGER,
            property_type TEXT DEFAULT 'Material',
            property_usage TEXT DEFAULT 'Consume',
            min_stock_level REAL DEFAULT 0,
            reorder_point REAL DEFAULT 0,
            last_audit_date TEXT,
            last_audit_quantity REAL,
            notes TEXT,
            active INTEGER DEFAULT 1,
            created_at TEXT DEFAULT (datetime('now')),
            updated_at TEXT DEFAULT (datetime('now')),
            FOREIGN KEY (location_id) REFERENCES inventory_locations(id)
        )
    """)
    
    conn.commit()
    
    # Import data
    print("\nImporting inventory data...")
    stats = import_inventory(conn, df)
    
    print(f"\nImport complete:")
    print(f"  Locations created: {stats['locations_created']}")
    print(f"  Items imported: {stats['items_imported']}")
    print(f"  Items updated: {stats['items_updated']}")
    print(f"  Errors: {stats['errors']}")
    
    # Print summary
    print_summary(conn)
    
    conn.close()
    print("\nInventory import completed successfully!")


if __name__ == '__main__':
    main()
