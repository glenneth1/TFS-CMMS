#!/usr/bin/env python3
"""
Import QA_Master.xlsx data into the TFS CMMS database.
"""

import sqlite3
import openpyxl
from datetime import datetime
import os

# Paths
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.dirname(SCRIPT_DIR)
DB_PATH = os.path.join(PROJECT_DIR, 'data', 'tfs-cmms.db')
EXCEL_PATH = os.path.join(PROJECT_DIR, 'QA_Master.xlsx')

# Camp to Country mapping (based on the spreadsheet structure)
# Map camp names to countries (use lowercase keys for case-insensitive lookup)
CAMP_COUNTRY_MAP = {
    # Iraq
    'al asad': 'Iraq',
    'erbil': 'Iraq',
    'sulay': 'Iraq',
    'ka2ab': 'Iraq',
    'camp pongo': 'Iraq',
    
    # Jordan
    'h5': 'Jordan',
    'kasotc': 'Jordan',
    'tower 22': 'Jordan',
    'badger': 'Jordan',
    'jtc': 'Jordan',
    'titin': 'Jordan',
    'kfab': 'Jordan',  # KFAB is in Jordan, not Kuwait
    
    # Syria
    'green village': 'Syria',
    'moore team house': 'Syria',
    'conoco': 'Syria',
    'rumalyn lz': 'Syria',
    'northern lz': 'Syria',
    'shaddadi': 'Syria',
    'atg': 'Syria',
    'camp monschke': 'Syria',
    'camp narvik': 'Syria',
    
    # Kuwait
    'arifjan': 'Kuwait',
    'buehring': 'Kuwait',
    
    # Qatar
    'cas air base': 'Qatar',
    
    # Saudi Arabia
    'sharura': 'Saudi Arabia',
    
    # Yemen
    'ar riyan': 'Yemen',
}

def get_country_for_camp(camp_name):
    """Get the country for a camp, defaulting to 'Unknown' if not mapped."""
    if not camp_name:
        return 'Unknown'
    normalized = camp_name.strip().lower()
    return CAMP_COUNTRY_MAP.get(normalized, 'Unknown')

def format_date(date_val):
    """Convert Excel date to ISO format string."""
    if date_val is None:
        return None
    if isinstance(date_val, datetime):
        return date_val.strftime('%Y-%m-%d')
    if isinstance(date_val, str):
        return date_val
    return str(date_val)

def normalize_status(status):
    """Normalize deficiency status to consistent case."""
    if not status:
        return None
    status = str(status).strip()
    status_lower = status.lower()
    
    # Map common variations to standard values
    if status_lower == 'open':
        return 'Open'
    elif status_lower == 'closed':
        return 'Closed'
    elif 'closed' in status_lower and 'repaired' in status_lower:
        return 'Closed / Repaired'
    elif 'closed' in status_lower and 'previous' in status_lower:
        return 'Closed Previously'
    elif 'tfs' in status_lower and 'closed' in status_lower:
        return 'Closed / Repaired'
    elif status_lower == 'reinsp-open' or status_lower == 'reinsp open':
        return 'ReInsp-Open'
    elif status_lower == 'mitigated':
        return 'Mitigated'
    elif status_lower == 'n/a':
        return 'N/A'
    else:
        # Title case for anything else
        return status.title()

def import_data():
    """Import data from QA_Master.xlsx into the database."""
    print(f"Opening database: {DB_PATH}")
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    
    print(f"Opening Excel file: {EXCEL_PATH}")
    wb = openpyxl.load_workbook(EXCEL_PATH, read_only=True, data_only=True)
    
    # Create countries
    print("\nCreating countries...")
    countries = ['Iraq', 'Yemen', 'Jordan', 'Syria', 'Kuwait', 'Qatar', 'Saudi Arabia', 'Unknown']
    for country in countries:
        cursor.execute(
            "INSERT OR IGNORE INTO countries (name) VALUES (?)",
            (country,)
        )
    conn.commit()
    
    # Get country IDs
    cursor.execute("SELECT id, name FROM countries")
    country_ids = {row[1]: row[0] for row in cursor.fetchall()}
    print(f"Countries: {country_ids}")
    
    # Process Data sheet
    ws = wb['Data']
    print("\nProcessing Data sheet...")
    
    # Track camps as we encounter them
    camp_ids = {}
    
    # Skip header row
    row_count = 0
    batch = []
    batch_size = 1000
    
    for row in ws.iter_rows(min_row=2, values_only=True):
        camp_name = row[0]
        if not camp_name:
            continue
            
        # Normalize camp name - title case for consistency
        camp_name = str(camp_name).strip().title()
        
        # Get or create camp
        if camp_name not in camp_ids:
            country_name = get_country_for_camp(camp_name)
            country_id = country_ids.get(country_name, country_ids['Unknown'])
            
            # Check if camp already exists (case-insensitive)
            cursor.execute(
                "SELECT id FROM camps WHERE LOWER(name) = LOWER(?) AND country_id = ?",
                (camp_name, country_id)
            )
            existing = cursor.fetchone()
            if existing:
                camp_ids[camp_name] = existing[0]
            else:
                cursor.execute(
                    "INSERT INTO camps (country_id, name) VALUES (?, ?)",
                    (country_id, camp_name)
                )
                camp_ids[camp_name] = cursor.lastrowid
        
        camp_id = camp_ids.get(camp_name)
        if not camp_id:
            print(f"Warning: Could not get camp_id for {camp_name}")
            continue
        
        # Extract row data
        building_number = str(row[1]).strip() if row[1] else None
        deficiency_number = str(row[2]).strip() if row[2] else None
        def_category = str(row[3]).strip() if row[3] else None
        equipment = str(row[4]).strip() if row[4] else None
        inspection_phase = str(row[5]).strip() if row[5] else None
        repair_by = str(row[6]).strip() if row[6] else None
        # Handle non-numeric occurrences values
        try:
            occurrences = int(row[7]) if row[7] and str(row[7]).strip() else 0
        except (ValueError, TypeError):
            occurrences = 0
        lhs_imminent = str(row[8]).strip() if row[8] else 'No'
        om_sor_number = str(row[9]).strip() if row[9] else None
        repair_team_number = str(row[10]).strip() if row[10] else None
        parts_ordered = str(row[11]).strip() if row[11] else None
        date_ordered = format_date(row[12])
        deficiency_status = normalize_status(row[13])
        inspection_date = format_date(row[14])
        # Handle non-numeric RAC values
        try:
            rac = int(row[15]) if row[15] and str(row[15]).strip() else None
        except (ValueError, TypeError):
            rac = None
        
        batch.append((
            camp_id, building_number, deficiency_number, def_category,
            equipment, inspection_phase, repair_by, occurrences,
            lhs_imminent, om_sor_number, repair_team_number, parts_ordered,
            date_ordered, deficiency_status, inspection_date, rac
        ))
        
        row_count += 1
        
        # Insert in batches
        if len(batch) >= batch_size:
            cursor.executemany("""
                INSERT INTO master_deficiencies 
                (camp_id, building_number, deficiency_number, def_category,
                 equipment, inspection_phase, repair_by, occurrences,
                 lhs_imminent, om_sor_number, repair_team_number, parts_ordered,
                 date_ordered, deficiency_status, inspection_date, rac)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, batch)
            conn.commit()
            print(f"  Imported {row_count} rows...")
            batch = []
    
    # Insert remaining batch
    if batch:
        cursor.executemany("""
            INSERT INTO master_deficiencies 
            (camp_id, building_number, deficiency_number, def_category,
             equipment, inspection_phase, repair_by, occurrences,
             lhs_imminent, om_sor_number, repair_team_number, parts_ordered,
             date_ordered, deficiency_status, inspection_date, rac)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, batch)
        conn.commit()
    
    print(f"\nImport complete!")
    print(f"  Total rows imported: {row_count}")
    
    # Print summary
    cursor.execute("SELECT COUNT(*) FROM master_deficiencies")
    total = cursor.fetchone()[0]
    print(f"  Total records in database: {total}")
    
    cursor.execute("""
        SELECT c.name as country, COUNT(*) as count 
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        JOIN countries c ON ca.country_id = c.id
        GROUP BY c.name
        ORDER BY count DESC
    """)
    print("\n  Records by country:")
    for row in cursor.fetchall():
        print(f"    {row[0]}: {row[1]}")
    
    cursor.execute("""
        SELECT ca.name as camp, COUNT(*) as count 
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        GROUP BY ca.name
        ORDER BY count DESC
        LIMIT 10
    """)
    print("\n  Top 10 camps by record count:")
    for row in cursor.fetchall():
        print(f"    {row[0]}: {row[1]}")
    
    conn.close()
    wb.close()
    print("\nDone!")

if __name__ == '__main__':
    import_data()
