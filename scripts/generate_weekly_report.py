#!/usr/bin/env python3
"""
Generate weekly report slides for client updates.
Uses matplotlib and seaborn for charts.
Outputs PNG images ready for PowerPoint.
"""

import sqlite3
import os
import argparse
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns
import pandas as pd

def format_date_display(date_str):
    """Convert YYYY-MM-DD to DD-MMM-YYYY format (e.g., 22-DEC-2025)."""
    if not date_str:
        return date_str
    try:
        dt = datetime.strptime(date_str, '%Y-%m-%d')
        return dt.strftime('%d-%b-%Y').upper()
    except:
        return date_str

# Paths
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.dirname(SCRIPT_DIR)
DB_PATH = os.path.join(PROJECT_DIR, 'data', 'tfs-cmms.db')
OUTPUT_DIR = os.path.join(PROJECT_DIR, 'reports')

# Ensure output directory exists
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Style settings
sns.set_theme(style="whitegrid")
NAVY_BLUE = '#000080'
COLORS = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2']


def get_db_connection():
    """Get database connection."""
    return sqlite3.connect(DB_PATH)


def get_last_monday():
    """Get the Monday of the previous week."""
    today = datetime.now()
    days_since_monday = today.weekday()
    last_monday = today - timedelta(days=days_since_monday + 7)
    return last_monday.strftime('%Y-%m-%d')


def get_last_saturday():
    """Get the Saturday of the previous week."""
    today = datetime.now()
    days_since_monday = today.weekday()
    last_saturday = today - timedelta(days=days_since_monday + 2)
    return last_saturday.strftime('%Y-%m-%d')


def get_weekly_stats(conn, start_date, end_date):
    """Get weekly statistics by country from master_deficiencies."""
    query = """
        SELECT 
            co.name as country,
            COUNT(DISTINCT md.building_number) as facilities_inspected,
            COUNT(DISTINCT md.building_number || '_' || md.inspection_date || '_' || md.inspection_phase) as total_reports,
            COUNT(DISTINCT CASE WHEN md.inspection_phase NOT LIKE '%Re%' 
                  THEN md.building_number || '_' || md.inspection_date || '_' || md.inspection_phase END) as initial_inspections,
            COUNT(DISTINCT CASE WHEN md.inspection_phase LIKE '%Re%' 
                  THEN md.building_number || '_' || md.inspection_date || '_' || md.inspection_phase END) as reinspections,
            COUNT(*) as total_deficiencies,
            SUM(CASE WHEN md.deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_deficiencies,
            SUM(CASE WHEN md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_deficiencies
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        JOIN countries co ON ca.country_id = co.id
        WHERE md.inspection_date >= ? AND md.inspection_date <= ?
        GROUP BY co.name
        ORDER BY total_reports DESC
    """
    df = pd.read_sql_query(query, conn, params=[start_date, end_date])
    return df


def get_deficiency_types(conn, start_date, end_date):
    """Get deficiency types breakdown for the week."""
    query = """
        SELECT 
            md.def_category as category,
            COUNT(*) as count
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        JOIN countries co ON ca.country_id = co.id
        WHERE md.inspection_date >= ? AND md.inspection_date <= ?
            AND md.def_category IS NOT NULL AND md.def_category != ''
        GROUP BY md.def_category
        ORDER BY count DESC
        LIMIT 10
    """
    df = pd.read_sql_query(query, conn, params=[start_date, end_date])
    return df


def get_deficiency_types_by_country(conn, start_date, end_date):
    """Get deficiency types breakdown with top country for each type."""
    query = """
        SELECT 
            md.def_category as category,
            co.name as country,
            COUNT(*) as count
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        JOIN countries co ON ca.country_id = co.id
        WHERE md.inspection_date >= ? AND md.inspection_date <= ?
            AND md.def_category IS NOT NULL AND md.def_category != ''
        GROUP BY md.def_category, co.name
        ORDER BY md.def_category, count DESC
    """
    df = pd.read_sql_query(query, conn, params=[start_date, end_date])
    return df


def get_cumulative_stats(conn):
    """Get cumulative (all-time) statistics."""
    query = """
        SELECT 
            COUNT(DISTINCT building_number) as total_facilities,
            COUNT(*) as total_deficiencies,
            SUM(CASE WHEN deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
            SUM(CASE WHEN deficiency_status LIKE '%Closed%' OR deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_count
        FROM master_deficiencies
    """
    cursor = conn.cursor()
    cursor.execute(query)
    row = cursor.fetchone()
    return {
        'total_facilities': row[0],
        'total_deficiencies': row[1],
        'open_count': row[2],
        'closed_count': row[3]
    }


def get_cumulative_by_country(conn):
    """Get cumulative statistics by country."""
    query = """
        SELECT 
            co.name as country,
            COUNT(DISTINCT md.building_number) as facilities,
            COUNT(*) as total_deficiencies,
            SUM(CASE WHEN md.deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
            SUM(CASE WHEN md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_count
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        JOIN countries co ON ca.country_id = co.id
        GROUP BY co.name
        ORDER BY total_deficiencies DESC
    """
    df = pd.read_sql_query(query, conn)
    return df


def get_90_day_stats(conn):
    """Get statistics for the last 90 days."""
    end_date = datetime.now().strftime('%Y-%m-%d')
    start_date = (datetime.now() - timedelta(days=90)).strftime('%Y-%m-%d')
    
    query = """
        SELECT 
            co.name as country,
            COUNT(DISTINCT md.building_number) as facilities,
            COUNT(*) as total_deficiencies,
            SUM(CASE WHEN md.deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        JOIN countries co ON ca.country_id = co.id
        WHERE md.inspection_date >= ? AND md.inspection_date <= ?
        GROUP BY co.name
        ORDER BY total_deficiencies DESC
    """
    df = pd.read_sql_query(query, conn, params=[start_date, end_date])
    return df


def get_repair_responsibility_stats(conn, start_date=None, end_date=None):
    """Get deficiency breakdown by repair responsibility (TF SAFE, O&M, Military)."""
    date_filter = ""
    params = []
    if start_date and end_date:
        date_filter = "WHERE md.inspection_date >= ? AND md.inspection_date <= ?"
        params = [start_date, end_date]
    
    query = f"""
        SELECT 
            COALESCE(md.repair_by, 'Unassigned') as repair_by,
            md.inspection_phase,
            SUM(CASE WHEN md.deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
            SUM(CASE WHEN md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_count,
            SUM(CASE WHEN md.deficiency_status LIKE '%Mitigated%' THEN 1 ELSE 0 END) as mitigated_count,
            COUNT(*) as total
        FROM master_deficiencies md
        {date_filter}
        GROUP BY md.repair_by, md.inspection_phase
        ORDER BY repair_by, inspection_phase
    """
    df = pd.read_sql_query(query, conn, params=params)
    return df


def get_combined_period_stats(conn, weekly_start, weekly_end):
    """Get combined stats for all three periods: Cumulative, Weekly, 90-day."""
    # 90-day range
    day90_end = datetime.now().strftime('%Y-%m-%d')
    day90_start = (datetime.now() - timedelta(days=90)).strftime('%Y-%m-%d')
    
    query = """
        SELECT 
            'Cumulative' as period,
            COUNT(*) as total_deficiencies,
            SUM(CASE WHEN deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
            SUM(CASE WHEN deficiency_status LIKE '%Closed%' OR deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_count
        FROM master_deficiencies
        UNION ALL
        SELECT 
            'Last 90 Days' as period,
            COUNT(*) as total_deficiencies,
            SUM(CASE WHEN deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
            SUM(CASE WHEN deficiency_status LIKE '%Closed%' OR deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_count
        FROM master_deficiencies
        WHERE inspection_date >= ? AND inspection_date <= ?
        UNION ALL
        SELECT 
            'This Week' as period,
            COUNT(*) as total_deficiencies,
            SUM(CASE WHEN deficiency_status = 'Open' THEN 1 ELSE 0 END) as open_count,
            SUM(CASE WHEN deficiency_status LIKE '%Closed%' OR deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_count
        FROM master_deficiencies
        WHERE inspection_date >= ? AND inspection_date <= ?
    """
    df = pd.read_sql_query(query, conn, params=[day90_start, day90_end, weekly_start, weekly_end])
    return df


def generate_operational_updates_slide(weekly_stats, start_date, end_date, output_path):
    """Generate the Operational Updates slide (REDi format)."""
    fig, ax = plt.subplots(figsize=(12, 8))
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    ax.axis('off')
    
    # Background
    fig.patch.set_facecolor('white')
    
    # Title
    ax.text(0.5, 9.5, '■ Operational Updates', fontsize=24, fontweight='bold', 
            color=NAVY_BLUE, ha='left', va='top')
    
    # Week info
    ax.text(1, 8.5, f'REDi – Week ({format_date_display(start_date)} – {format_date_display(end_date)})', 
            fontsize=18, fontweight='bold', color=NAVY_BLUE, ha='left', va='top')
    
    # Calculate totals
    total_facilities = weekly_stats['facilities_inspected'].sum()
    total_reports = weekly_stats['total_reports'].sum()
    total_reinsp = weekly_stats['reinspections'].sum()
    initial_insp = weekly_stats['initial_inspections'].sum()
    
    # Electrical section
    y_pos = 7.5
    ax.text(1.5, y_pos, '• Electrical', fontsize=16, fontweight='bold', 
            color=NAVY_BLUE, ha='left', va='top')
    
    y_pos -= 0.8
    ax.text(2.5, y_pos, f'➤ Number of Facilities Inspected – {total_facilities}', 
            fontsize=14, color=NAVY_BLUE, ha='left', va='top')
    
    y_pos -= 0.6
    ax.text(3, y_pos, f'✓ Uploaded {total_reports} reports ({initial_insp} Inspections – {total_reinsp} Re-Inspections)', 
            fontsize=14, color=NAVY_BLUE, ha='left', va='top')
    
    # UFC section (placeholder)
    y_pos -= 1.2
    ax.text(1.5, y_pos, '• UFC', fontsize=16, fontweight='bold', 
            color=NAVY_BLUE, ha='left', va='top')
    
    y_pos -= 0.8
    ax.text(2.5, y_pos, '➤ Number of Facilities Inspected – 0', 
            fontsize=14, color=NAVY_BLUE, ha='left', va='top')
    
    y_pos -= 0.6
    ax.text(3, y_pos, '✓ Uploaded 0 reports', 
            fontsize=14, color=NAVY_BLUE, ha='left', va='top')
    
    # Footer
    y_pos -= 1.5
    ax.text(0.5, y_pos, 'All appropriate stakeholders were notified', 
            fontsize=14, color=NAVY_BLUE, ha='left', va='top')
    
    # REDi link
    rect = mpatches.FancyBboxPatch((0.3, 0.5), 9.4, 0.8, 
                                    boxstyle="round,pad=0.05", 
                                    facecolor='yellow', edgecolor='none')
    ax.add_patch(rect)
    ax.text(5, 0.9, 'For access to all reports, please use the following link: https://uroc-redi.usace.army.mil/sites/uroc/TADARCENT',
            fontsize=10, color=NAVY_BLUE, ha='center', va='center')
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight', facecolor='white')
    plt.close()
    print(f"  Generated: {output_path}")


def generate_weekly_by_country_chart(weekly_stats, start_date, end_date, output_path):
    """Generate bar chart of weekly stats by country."""
    if weekly_stats.empty:
        print("  No weekly data to chart")
        return
    
    fig, ax = plt.subplots(figsize=(12, 6))
    
    x = range(len(weekly_stats))
    width = 0.35
    
    bars1 = ax.bar([i - width/2 for i in x], weekly_stats['facilities_inspected'], 
                   width, label='Facilities Inspected', color='#1f77b4')
    bars2 = ax.bar([i + width/2 for i in x], weekly_stats['total_deficiencies'], 
                   width, label='Deficiencies Found', color='#ff7f0e')
    
    ax.set_xlabel('Country', fontsize=12)
    ax.set_ylabel('Count', fontsize=12)
    ax.set_title(f'Weekly Inspection Summary ({format_date_display(start_date)} to {format_date_display(end_date)})', fontsize=14, fontweight='bold')
    ax.set_xticks(x)
    ax.set_xticklabels(weekly_stats['country'], rotation=45, ha='right')
    ax.legend()
    
    # Add value labels on bars
    for bar in bars1:
        height = bar.get_height()
        ax.annotate(f'{int(height)}', xy=(bar.get_x() + bar.get_width()/2, height),
                   xytext=(0, 3), textcoords="offset points", ha='center', va='bottom', fontsize=9)
    for bar in bars2:
        height = bar.get_height()
        ax.annotate(f'{int(height)}', xy=(bar.get_x() + bar.get_width()/2, height),
                   xytext=(0, 3), textcoords="offset points", ha='center', va='bottom', fontsize=9)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Generated: {output_path}")


def generate_deficiency_types_chart(def_types, start_date, end_date, output_path, def_types_by_country=None):
    """Generate horizontal bar chart of deficiency types with country indicator."""
    if def_types.empty:
        print("  No deficiency type data to chart")
        return
    
    fig, ax = plt.subplots(figsize=(14, 8))
    fig.patch.set_facecolor('white')
    ax.set_facecolor('white')
    
    # Get top country for each deficiency type
    top_countries = {}
    if def_types_by_country is not None and not def_types_by_country.empty:
        for category in def_types['category'].unique():
            cat_data = def_types_by_country[def_types_by_country['category'] == category]
            if not cat_data.empty:
                top_country = cat_data.iloc[0]['country']
                top_count = cat_data.iloc[0]['count']
                top_countries[category] = (top_country, top_count)
    
    # Truncate long category names and add country info
    def_types = def_types.copy()
    def_types['label'] = def_types.apply(
        lambda row: f"{row['category'][:30]}{'...' if len(str(row['category'])) > 30 else ''}" + 
                    (f" ({top_countries.get(row['category'], ('', 0))[0]})" 
                     if row['category'] in top_countries else ""),
        axis=1
    )
    
    # Reverse order so largest is at top
    def_types = def_types.iloc[::-1]
    
    # Create horizontal bar chart
    colors = sns.color_palette('husl', len(def_types))
    bars = ax.barh(range(len(def_types)), def_types['count'], color=colors[::-1])
    
    ax.set_yticks(range(len(def_types)))
    ax.set_yticklabels(def_types['label'], fontsize=10)
    ax.set_xlabel('Number of Deficiencies', fontsize=12, fontweight='bold')
    ax.set_title(f'Top Deficiency Types by Category\n({format_date_display(start_date)} to {format_date_display(end_date)})', 
                 fontsize=14, fontweight='bold', color=NAVY_BLUE, pad=15)
    
    # Add value labels on bars
    for bar, count in zip(bars, def_types['count']):
        width = bar.get_width()
        ax.annotate(f'{int(count):,}', xy=(width, bar.get_y() + bar.get_height()/2),
                   xytext=(5, 0), textcoords="offset points", 
                   ha='left', va='center', fontsize=10, fontweight='bold')
    
    # Add grid
    ax.xaxis.grid(True, linestyle='--', alpha=0.7)
    ax.set_axisbelow(True)
    
    # Remove top and right spines
    for spine in ['top', 'right']:
        ax.spines[spine].set_visible(False)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight', facecolor='white')
    plt.close()
    print(f"  Generated: {output_path}")


def generate_cumulative_stats_slide(cumulative_stats, cumulative_by_country, output_path):
    """Generate cumulative statistics slide."""
    fig, axes = plt.subplots(1, 2, figsize=(14, 6))
    
    # Left: Summary stats
    ax1 = axes[0]
    ax1.axis('off')
    
    stats_text = f"""
    CUMULATIVE STATISTICS (All Time)
    
    Total Facilities Inspected: {cumulative_stats['total_facilities']:,}
    
    Total Deficiencies Found: {cumulative_stats['total_deficiencies']:,}
    
    Open Deficiencies: {cumulative_stats['open_count']:,}
    
    Closed/Repaired: {cumulative_stats['closed_count']:,}
    """
    
    ax1.text(0.1, 0.9, stats_text, fontsize=14, fontweight='bold',
             color=NAVY_BLUE, ha='left', va='top', transform=ax1.transAxes,
             family='monospace')
    ax1.set_title('Summary', fontsize=16, fontweight='bold', color=NAVY_BLUE)
    
    # Right: By country bar chart
    ax2 = axes[1]
    if not cumulative_by_country.empty:
        colors = ['#d62728' if row['open_count'] > 0 else '#2ca02c' 
                  for _, row in cumulative_by_country.iterrows()]
        
        bars = ax2.barh(cumulative_by_country['country'], 
                        cumulative_by_country['total_deficiencies'],
                        color='#1f77b4')
        
        ax2.set_xlabel('Total Deficiencies', fontsize=12)
        ax2.set_title('Deficiencies by Country', fontsize=14, fontweight='bold')
        
        # Add value labels
        for bar, val in zip(bars, cumulative_by_country['total_deficiencies']):
            ax2.text(val + 100, bar.get_y() + bar.get_height()/2, 
                    f'{int(val):,}', va='center', fontsize=10)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Generated: {output_path}")


def generate_90_day_chart(stats_90_day, output_path):
    """Generate 90-day statistics chart."""
    if stats_90_day.empty:
        print("  No 90-day data to chart")
        return
    
    fig, ax = plt.subplots(figsize=(12, 6))
    
    x = range(len(stats_90_day))
    width = 0.25
    
    bars1 = ax.bar([i - width for i in x], stats_90_day['facilities'], 
                   width, label='Facilities', color='#1f77b4')
    bars2 = ax.bar(x, stats_90_day['total_deficiencies'], 
                   width, label='Total Deficiencies', color='#ff7f0e')
    bars3 = ax.bar([i + width for i in x], stats_90_day['open_count'], 
                   width, label='Open', color='#d62728')
    
    ax.set_xlabel('Country', fontsize=12)
    ax.set_ylabel('Count', fontsize=12)
    ax.set_title('Last 90 Days - Inspection Data', fontsize=14, fontweight='bold')
    ax.set_xticks(x)
    ax.set_xticklabels(stats_90_day['country'], rotation=45, ha='right')
    ax.legend()
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Generated: {output_path}")


def generate_combined_deficiency_chart(combined_stats, start_date, end_date, output_path):
    """Generate professional combined chart showing Cumulative, 90-Day, and Weekly data together."""
    if combined_stats.empty:
        print("  No combined data to chart")
        return
    
    fig, ax = plt.subplots(figsize=(14, 8))
    
    # Set up the data
    periods = combined_stats['period'].tolist()
    x = range(len(periods))
    width = 0.25
    
    # Create grouped bars for Open, Closed, Total
    bars_total = ax.bar([i - width for i in x], combined_stats['total_deficiencies'], 
                        width, label='Total Deficiencies', color='#3498db', edgecolor='white')
    bars_closed = ax.bar(x, combined_stats['closed_count'], 
                         width, label='Closed/Repaired', color='#27ae60', edgecolor='white')
    bars_open = ax.bar([i + width for i in x], combined_stats['open_count'], 
                       width, label='Open', color='#e74c3c', edgecolor='white')
    
    ax.set_xlabel('Time Period', fontsize=14, fontweight='bold')
    ax.set_ylabel('Number of Deficiencies', fontsize=14, fontweight='bold')
    ax.set_title(f'Deficiency Status Comparison\n(Week: {format_date_display(start_date)} to {format_date_display(end_date)})', 
                 fontsize=16, fontweight='bold', color=NAVY_BLUE)
    ax.set_xticks(x)
    ax.set_xticklabels(periods, fontsize=12, fontweight='bold')
    ax.legend(loc='upper right', fontsize=11, framealpha=0.9)
    
    # Add value labels on bars
    def add_labels(bars):
        for bar in bars:
            height = bar.get_height()
            if height > 0:
                ax.annotate(f'{int(height):,}', 
                           xy=(bar.get_x() + bar.get_width()/2, height),
                           xytext=(0, 3), textcoords="offset points", 
                           ha='center', va='bottom', fontsize=10, fontweight='bold')
    
    add_labels(bars_total)
    add_labels(bars_closed)
    add_labels(bars_open)
    
    # Add grid for readability
    ax.yaxis.grid(True, linestyle='--', alpha=0.7)
    ax.set_axisbelow(True)
    
    # Style the spines
    for spine in ['top', 'right']:
        ax.spines[spine].set_visible(False)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight', facecolor='white')
    plt.close()
    print(f"  Generated: {output_path}")


def get_facilities_inspected_by_site(conn, start_date, end_date):
    """Get count of unique facilities inspected by country and site.
    
    Args:
        conn: Database connection
        start_date: Start date (None for all-time/cumulative)
        end_date: End date
    
    Returns count of unique building_number values per country/camp.
    """
    if start_date:
        date_filter = "WHERE md.inspection_date >= ? AND md.inspection_date <= ?"
        params = [start_date, end_date]
    else:
        date_filter = "WHERE md.inspection_date <= ?"
        params = [end_date]
    
    query = f"""
        SELECT 
            co.name as country,
            ca.name as camp,
            COUNT(DISTINCT md.building_number) as facilities_inspected
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        JOIN countries co ON ca.country_id = co.id
        {date_filter}
        GROUP BY co.name, ca.name
        ORDER BY co.name, ca.name
    """
    df = pd.read_sql_query(query, conn, params=params)
    return df


def generate_facilities_inspected_table(conn, start_date, end_date, output_path):
    """Generate Theater Weekly Totals - Facilities Inspected table.
    
    Shows count of unique facilities inspected by Country/Camp for:
    - Weekly (selected date range)
    - 90 Days (90 days back from end date)
    - Cumulative (all time)
    """
    # Calculate date ranges
    end_date_obj = datetime.strptime(end_date, '%Y-%m-%d')
    day90_start = (end_date_obj - timedelta(days=90)).strftime('%Y-%m-%d')
    
    # Get data for all three periods
    df_weekly = get_facilities_inspected_by_site(conn, start_date, end_date)
    df_90day = get_facilities_inspected_by_site(conn, day90_start, end_date)
    df_cumulative = get_facilities_inspected_by_site(conn, None, end_date)
    
    if df_cumulative.empty:
        print("  No facilities data to generate table")
        return
    
    # Merge dataframes on country and camp
    df = df_cumulative[['country', 'camp']].copy()
    df = df.merge(df_weekly[['country', 'camp', 'facilities_inspected']], 
                  on=['country', 'camp'], how='left', suffixes=('', '_weekly'))
    df = df.rename(columns={'facilities_inspected': 'weekly'})
    df = df.merge(df_90day[['country', 'camp', 'facilities_inspected']], 
                  on=['country', 'camp'], how='left')
    df = df.rename(columns={'facilities_inspected': 'day90'})
    df = df.merge(df_cumulative[['country', 'camp', 'facilities_inspected']], 
                  on=['country', 'camp'], how='left')
    df = df.rename(columns={'facilities_inspected': 'cumulative'})
    
    # Fill NaN with 0
    df = df.fillna(0)
    
    # Calculate figure height based on number of rows
    num_rows = len(df) + 2  # data rows + header + totals
    fig_height = max(8, num_rows * 0.35 + 2)
    
    # Create figure
    fig, ax = plt.subplots(figsize=(14, fig_height))
    ax.axis('off')
    fig.patch.set_facecolor('white')
    
    # Title
    title = "Theater Weekly Totals, Electrical Reports"
    subtitle = "Number of Facilities Inspected (Power Distribution / Gen Sets)"
    fig.suptitle(title, fontsize=18, fontweight='bold', color='#000080', y=0.98)
    ax.set_title(subtitle, fontsize=14, color='#000080', pad=10)
    
    # Format date ranges for column headers
    weekly_header = f"Weekly\n{format_date_display(start_date)} - {format_date_display(end_date)}"
    day90_header = f"90 Days\n{format_date_display(day90_start)} to {format_date_display(end_date)}"
    cumulative_header = "Cumulative\nAll Time"
    
    col_headers = ['Country', 'Camp', weekly_header, day90_header, cumulative_header]
    
    # Build table data with country grouping
    table_data = []
    current_country = None
    
    for _, row in df.iterrows():
        country_display = row['country'] if row['country'] != current_country else ''
        current_country = row['country']
        
        table_data.append([
            country_display,
            row['camp'],
            int(row['weekly']),
            int(row['day90']),
            int(row['cumulative'])
        ])
    
    # Add totals row
    totals = ['TOTALS', '', 
              int(df['weekly'].sum()), 
              int(df['day90'].sum()), 
              int(df['cumulative'].sum())]
    table_data.append(totals)
    
    # Create table
    table = ax.table(
        cellText=table_data,
        colLabels=col_headers,
        loc='center',
        cellLoc='center'
    )
    
    # Style the table
    table.auto_set_font_size(False)
    table.set_fontsize(9)
    table.scale(1.2, 1.5)
    
    # Color the headers
    header_colors = {
        0: '#C0C0C0',  # Country - gray
        1: '#C0C0C0',  # Camp - gray
        2: '#FFFF00',  # Weekly - yellow
        3: '#87CEEB',  # 90 Days - light blue
        4: '#90EE90',  # Cumulative - light green
    }
    
    for col, color in header_colors.items():
        cell = table[(0, col)]
        cell.set_facecolor(color)
        cell.set_text_props(fontweight='bold')
    
    # Style data cells
    for i in range(1, len(table_data) + 1):
        for j in range(len(col_headers)):
            cell = table[(i, j)]
            # Remove gridlines
            cell.set_edgecolor('white')
            cell.set_linewidth(0.5)
            
            # Color columns
            if j == 2:  # Weekly
                cell.set_facecolor('#FFFFCC')  # Light yellow
            elif j == 3:  # 90 Days
                cell.set_facecolor('#E0F0FF')  # Light blue
            elif j == 4:  # Cumulative
                cell.set_facecolor('#E0FFE0')  # Light green
            
            # Bold totals row
            if i == len(table_data):
                cell.set_text_props(fontweight='bold')
                cell.set_facecolor('#DDDDDD')
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight', facecolor='white')
    plt.close()
    print(f"  Generated: {output_path}")


def get_top5_hazards(conn, start_date, end_date):
    """Get top 5 deficiency categories (hazards) with counts.
    
    Args:
        conn: Database connection
        start_date: Start date (None for all-time/cumulative)
        end_date: End date
    """
    # The top 5 hazard categories
    top5_categories = [
        'Unlisted Equipment',
        'Poor Workmanship', 
        'Grounding and Bonding',
        'Improper Terminations',
        'Improper Use / Damaged'
    ]
    
    if start_date:
        date_filter = "WHERE inspection_date >= ? AND inspection_date <= ?"
        params = [start_date, end_date]
    else:
        date_filter = "WHERE inspection_date <= ?"
        params = [end_date]
    
    # Get counts for each category
    results = []
    for category in top5_categories:
        query = f"""
            SELECT COUNT(*) as cnt 
            FROM master_deficiencies 
            {date_filter} AND def_category = ?
        """
        df = pd.read_sql_query(query, conn, params=params + [category])
        count = df['cnt'].iloc[0] if not df.empty else 0
        results.append({'category': category, 'count': count})
    
    return pd.DataFrame(results)


def generate_top5_hazards_chart(conn, start_date, end_date, output_path):
    """Generate Top 5 Hazards chart with 3 horizontal bar charts (Weekly, 90-Day, Cumulative).
    
    Shows the top 5 deficiency categories with counts and percentages.
    """
    # Calculate date ranges
    end_date_obj = datetime.strptime(end_date, '%Y-%m-%d')
    day90_start = (end_date_obj - timedelta(days=90)).strftime('%Y-%m-%d')
    
    # Get data for all three periods
    df_weekly = get_top5_hazards(conn, start_date, end_date)
    df_90day = get_top5_hazards(conn, day90_start, end_date)
    df_cumulative = get_top5_hazards(conn, None, end_date)
    
    # Calculate percentages
    for df in [df_weekly, df_90day, df_cumulative]:
        total = df['count'].sum()
        df['percentage'] = (df['count'] / total * 100) if total > 0 else 0
    
    # Create figure with 3 subplots (vertical stack)
    fig, axes = plt.subplots(3, 1, figsize=(14, 16))
    fig.patch.set_facecolor('white')
    
    # Main title
    fig.suptitle('Type of Deficiencies Found (Top 5 Hazards)', 
                 fontsize=20, fontweight='bold', color='#800080', y=0.98)
    
    # Color palette (purple theme to match TF SAFE branding)
    colors = ['#8B008B', '#9932CC', '#BA55D3', '#DA70D6', '#EE82EE']
    
    # Data and titles for each chart
    datasets = [
        (df_weekly, f"Weekly ({format_date_display(start_date)} to {format_date_display(end_date)})", '#FFFF00'),
        (df_90day, f"90-Day ({format_date_display(day90_start)} to {format_date_display(end_date)})", '#87CEEB'),
        (df_cumulative, f"Cumulative (All Time through {format_date_display(end_date)})", '#90EE90')
    ]
    
    for ax, (df, title, title_color) in zip(axes, datasets):
        # Sort by count descending for display
        df_sorted = df.sort_values('count', ascending=True)  # ascending for horizontal bar
        
        # Create horizontal bar chart
        y_pos = range(len(df_sorted))
        bars = ax.barh(y_pos, df_sorted['count'], color=colors[::-1], edgecolor='white', height=0.6)
        
        # Set labels
        ax.set_yticks(y_pos)
        ax.set_yticklabels([cat.upper() for cat in df_sorted['category']], fontsize=10, fontweight='bold')
        ax.set_xlabel('Count', fontsize=11)
        ax.set_title(title, fontsize=14, fontweight='bold', color='#333333', pad=10)
        
        # Add count and percentage labels on bars
        for i, (bar, row) in enumerate(zip(bars, df_sorted.itertuples())):
            width = bar.get_width()
            label = f"{row.category}, {int(row.count):,}"
            pct_label = f"({row.percentage:.1f}%)"
            
            # Position label inside or outside bar based on width
            if width > df_sorted['count'].max() * 0.3:
                ax.text(width * 0.5, bar.get_y() + bar.get_height()/2,
                       f"{row.category}, {int(row.count):,}",
                       ha='center', va='center', fontsize=9, color='white', fontweight='bold')
            else:
                ax.text(width + df_sorted['count'].max() * 0.02, bar.get_y() + bar.get_height()/2,
                       f"{int(row.count):,} ({row.percentage:.1f}%)",
                       ha='left', va='center', fontsize=9, color='#333333', fontweight='bold')
        
        # Style
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.grid(axis='x', alpha=0.3, linestyle='--')
    
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    plt.savefig(output_path, dpi=150, bbox_inches='tight', facecolor='white')
    plt.close()
    print(f"  Generated: {output_path}")


def get_inspection_summary_by_site(conn, start_date, end_date):
    """Get inspection summary grouped by country and site with repair responsibility breakdown.
    
    C1 = Imminent Danger (LHS = Yes) - Life, Health, Safety imminent
    C2 = Non-Imminent (LHS = No) - Not imminent danger
    
    Args:
        conn: Database connection
        start_date: Start date (None for all-time/cumulative)
        end_date: End date
    """
    # Build WHERE clause based on whether start_date is provided
    if start_date:
        date_filter = "WHERE md.inspection_date >= ? AND md.inspection_date <= ?"
        params = [start_date, end_date]
    else:
        date_filter = "WHERE md.inspection_date <= ?"
        params = [end_date]
    
    query = f"""
        SELECT 
            co.name as country,
            ca.name as camp,
            -- Count unique inspections (building + date combinations)
            COUNT(DISTINCT md.building_number || '_' || md.inspection_date) as num_inspections,
            -- Total deficiencies
            COUNT(*) as num_deficiencies,
            -- TF SAFE C1 (Imminent LHS = Yes)
            SUM(CASE WHEN (md.repair_by LIKE '%Task Force SAFE%' OR md.repair_by LIKE '%Task Force Safe%') 
                     AND UPPER(md.lhs_imminent) = 'YES'
                     AND (md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%') THEN 1 ELSE 0 END) as tfs_c1_repaired,
            SUM(CASE WHEN (md.repair_by LIKE '%Task Force SAFE%' OR md.repair_by LIKE '%Task Force Safe%') 
                     AND UPPER(md.lhs_imminent) = 'YES'
                     AND md.deficiency_status LIKE '%Mitigated%' THEN 1 ELSE 0 END) as tfs_c1_mitigated,
            -- TF SAFE C2 (Non-Imminent LHS = No)
            SUM(CASE WHEN (md.repair_by LIKE '%Task Force SAFE%' OR md.repair_by LIKE '%Task Force Safe%') 
                     AND (UPPER(md.lhs_imminent) = 'NO' OR md.lhs_imminent IS NULL OR TRIM(md.lhs_imminent) = '')
                     AND (md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%') THEN 1 ELSE 0 END) as tfs_c2_repaired,
            SUM(CASE WHEN (md.repair_by LIKE '%Task Force SAFE%' OR md.repair_by LIKE '%Task Force Safe%') 
                     AND (UPPER(md.lhs_imminent) = 'NO' OR md.lhs_imminent IS NULL OR TRIM(md.lhs_imminent) = '')
                     AND (md.deficiency_status = 'Open' OR md.deficiency_status LIKE '%Open%') THEN 1 ELSE 0 END) as tfs_c2_open,
            -- O&M C1 (Imminent LHS = Yes)
            SUM(CASE WHEN md.repair_by LIKE '%O&M%' 
                     AND UPPER(md.lhs_imminent) = 'YES'
                     AND (md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%') THEN 1 ELSE 0 END) as om_c1_repaired,
            SUM(CASE WHEN md.repair_by LIKE '%O&M%' 
                     AND UPPER(md.lhs_imminent) = 'YES'
                     AND md.deficiency_status LIKE '%Mitigated%' THEN 1 ELSE 0 END) as om_c1_mitigated,
            -- O&M C2 (Non-Imminent LHS = No)
            SUM(CASE WHEN md.repair_by LIKE '%O&M%' 
                     AND (UPPER(md.lhs_imminent) = 'NO' OR md.lhs_imminent IS NULL OR TRIM(md.lhs_imminent) = '')
                     AND (md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%') THEN 1 ELSE 0 END) as om_c2_repaired,
            SUM(CASE WHEN md.repair_by LIKE '%O&M%' 
                     AND (UPPER(md.lhs_imminent) = 'NO' OR md.lhs_imminent IS NULL OR TRIM(md.lhs_imminent) = '')
                     AND (md.deficiency_status = 'Open' OR md.deficiency_status LIKE '%Open%') THEN 1 ELSE 0 END) as om_c2_open,
            -- Military C1 (Imminent LHS = Yes)
            SUM(CASE WHEN md.repair_by LIKE '%Military%' 
                     AND UPPER(md.lhs_imminent) = 'YES'
                     AND (md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%') THEN 1 ELSE 0 END) as mil_c1_repaired,
            SUM(CASE WHEN md.repair_by LIKE '%Military%' 
                     AND UPPER(md.lhs_imminent) = 'YES'
                     AND md.deficiency_status LIKE '%Mitigated%' THEN 1 ELSE 0 END) as mil_c1_mitigated,
            -- Military C2 (Non-Imminent LHS = No)
            SUM(CASE WHEN md.repair_by LIKE '%Military%' 
                     AND (UPPER(md.lhs_imminent) = 'NO' OR md.lhs_imminent IS NULL OR TRIM(md.lhs_imminent) = '')
                     AND (md.deficiency_status LIKE '%Closed%' OR md.deficiency_status LIKE '%Repaired%') THEN 1 ELSE 0 END) as mil_c2_repaired,
            SUM(CASE WHEN md.repair_by LIKE '%Military%' 
                     AND (UPPER(md.lhs_imminent) = 'NO' OR md.lhs_imminent IS NULL OR TRIM(md.lhs_imminent) = '')
                     AND (md.deficiency_status = 'Open' OR md.deficiency_status LIKE '%Open%') THEN 1 ELSE 0 END) as mil_c2_open
        FROM master_deficiencies md
        JOIN camps ca ON md.camp_id = ca.id
        JOIN countries co ON ca.country_id = co.id
        {date_filter}
        GROUP BY co.name, ca.name
        ORDER BY co.name, ca.name
    """
    df = pd.read_sql_query(query, conn, params=params)
    return df


def generate_inspection_data_table(conn, start_date, end_date, output_path, period_label="Weekly"):
    """Generate an Inspection Data table as an image.
    
    Args:
        conn: Database connection
        start_date: Start date for data range (None for cumulative/all-time)
        end_date: End date for data range
        output_path: Path to save the image
        period_label: Label for the period (e.g., "Weekly", "90-Day", "Cumulative")
    """
    df = get_inspection_summary_by_site(conn, start_date, end_date)
    
    if df.empty:
        print(f"  No {period_label.lower()} inspection data to generate table")
        return
    
    # Calculate figure height based on number of rows
    num_rows = len(df) + 2  # data rows + header + totals
    fig_height = max(6, num_rows * 0.35 + 2)
    
    # Create figure with appropriate size
    fig, ax = plt.subplots(figsize=(18, fig_height))
    ax.axis('off')
    fig.patch.set_facecolor('white')
    
    # Title
    title = f"Task Force SAFE - Inspection Data ({period_label})"
    if start_date:
        subtitle = f"{format_date_display(start_date)} to {format_date_display(end_date)}"
    else:
        subtitle = f"All Data through {format_date_display(end_date)}"
    fig.suptitle(title, fontsize=18, fontweight='bold', color='#800080', y=0.98)
    ax.set_title(subtitle, fontsize=14, color='#800080', pad=5)
    
    # Prepare table data
    # Column headers (multi-level)
    col_headers = [
        'Country', 'Camp', 'No. of\nInspections', 'No. of\nDeficiencies',
        'Repaired', 'Mitigated',  # TF SAFE C1
        'Repaired', 'Mitigated',  # O&M C1
        'Repaired', 'Mitigated',  # Military C1
        'Repaired', 'Open',       # TF SAFE C2
        'Repaired', 'Open',       # O&M C2
        'Repaired', 'Open'        # Military C2
    ]
    
    # Build table data with country grouping
    table_data = []
    current_country = None
    
    for _, row in df.iterrows():
        country_display = row['country'] if row['country'] != current_country else ''
        current_country = row['country']
        
        table_data.append([
            country_display,
            row['camp'],
            int(row['num_inspections']),
            int(row['num_deficiencies']),
            int(row['tfs_c1_repaired']),
            int(row['tfs_c1_mitigated']),
            int(row['om_c1_repaired']),
            int(row['om_c1_mitigated']),
            int(row['mil_c1_repaired']),
            int(row['mil_c1_mitigated']),
            int(row['tfs_c2_repaired']),
            int(row['tfs_c2_open']),
            int(row['om_c2_repaired']),
            int(row['om_c2_open']),
            int(row['mil_c2_repaired']),
            int(row['mil_c2_open'])
        ])
    
    # Add totals row
    totals = ['TOTALS', '']
    totals.append(int(df['num_inspections'].sum()))
    totals.append(int(df['num_deficiencies'].sum()))
    totals.append(int(df['tfs_c1_repaired'].sum()))
    totals.append(int(df['tfs_c1_mitigated'].sum()))
    totals.append(int(df['om_c1_repaired'].sum()))
    totals.append(int(df['om_c1_mitigated'].sum()))
    totals.append(int(df['mil_c1_repaired'].sum()))
    totals.append(int(df['mil_c1_mitigated'].sum()))
    totals.append(int(df['tfs_c2_repaired'].sum()))
    totals.append(int(df['tfs_c2_open'].sum()))
    totals.append(int(df['om_c2_repaired'].sum()))
    totals.append(int(df['om_c2_open'].sum()))
    totals.append(int(df['mil_c2_repaired'].sum()))
    totals.append(int(df['mil_c2_open'].sum()))
    table_data.append(totals)
    
    # Create table
    table = ax.table(
        cellText=table_data,
        colLabels=col_headers,
        loc='center',
        cellLoc='center'
    )
    
    # Style the table
    table.auto_set_font_size(False)
    table.set_fontsize(8)
    table.scale(1.2, 1.5)
    
    # Color coding
    header_colors = {
        (0, 0): '#DDD9C4',  # Country
        (0, 1): '#DDD9C4',  # Camp
        (0, 2): '#FFFF00',  # Inspections (yellow)
        (0, 3): '#FF0000',  # Deficiencies (red)
    }
    
    # TF SAFE headers (purple)
    for col in [4, 5, 10, 11]:
        header_colors[(0, col)] = '#800080'
    
    # O&M headers (green)
    for col in [6, 7, 12, 13]:
        header_colors[(0, col)] = '#00FF00'
    
    # Military headers (red)
    for col in [8, 9, 14, 15]:
        header_colors[(0, col)] = '#FF0000'
    
    # Apply header colors
    for (row, col), color in header_colors.items():
        cell = table[(row, col)]
        cell.set_facecolor(color)
        if color in ['#800080', '#FF0000']:
            cell.set_text_props(color='white', fontweight='bold')
        else:
            cell.set_text_props(fontweight='bold')
    
    # Style data cells - alternate row colors and highlight deficiency column
    for i in range(1, len(table_data) + 1):
        for j in range(len(col_headers)):
            cell = table[(i, j)]
            # Remove cell borders (no gridlines)
            cell.set_edgecolor('white')
            cell.set_linewidth(0.5)
            
            # Yellow background for inspections column
            if j == 2:
                cell.set_facecolor('#FFFF99')
            # Red background for deficiencies column  
            elif j == 3:
                cell.set_facecolor('#FF9999')
            # Light purple for TF SAFE columns
            elif j in [4, 5, 10, 11]:
                cell.set_facecolor('#E6E0F8')
            # Light green for O&M columns
            elif j in [6, 7, 12, 13]:
                cell.set_facecolor('#E0FFE0')
            # Light red for Military columns
            elif j in [8, 9, 14, 15]:
                cell.set_facecolor('#FFE0E0')
            
            # Bold totals row
            if i == len(table_data):
                cell.set_text_props(fontweight='bold')
                cell.set_facecolor('#DDDDDD')
    
    # Add group headers above the main headers - positioned relative to table
    # Adjust header_y based on number of rows (more rows = lower position needed)
    header_y = 0.92 if num_rows < 15 else 0.95  # Position higher above the table
    ax.text(0.32, header_y, 'TF SAFE - C1', fontsize=9, fontweight='bold', 
            color='white', ha='center', transform=ax.transAxes,
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#800080', edgecolor='none'))
    ax.text(0.44, header_y, 'O&M - C1', fontsize=9, fontweight='bold',
            color='black', ha='center', transform=ax.transAxes,
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#00FF00', edgecolor='none'))
    ax.text(0.56, header_y, 'Military - C1', fontsize=9, fontweight='bold',
            color='white', ha='center', transform=ax.transAxes,
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#FF0000', edgecolor='none'))
    ax.text(0.68, header_y, 'TF SAFE - C2', fontsize=9, fontweight='bold',
            color='white', ha='center', transform=ax.transAxes,
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#800080', edgecolor='none'))
    ax.text(0.80, header_y, 'O&M - C2', fontsize=9, fontweight='bold',
            color='black', ha='center', transform=ax.transAxes,
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#00FF00', edgecolor='none'))
    ax.text(0.92, header_y, 'Military - C2', fontsize=9, fontweight='bold',
            color='white', ha='center', transform=ax.transAxes,
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#FF0000', edgecolor='none'))
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight', facecolor='white')
    plt.close()
    print(f"  Generated: {output_path}")


def generate_repair_responsibility_chart(conn, start_date, end_date, output_path):
    """Generate chart showing deficiencies by repair responsibility with status breakdown."""
    # Get cumulative, 90-day, and weekly data by repair responsibility
    day90_end = datetime.now().strftime('%Y-%m-%d')
    day90_start = (datetime.now() - timedelta(days=90)).strftime('%Y-%m-%d')
    
    query = """
        SELECT 
            CASE 
                WHEN repair_by LIKE '%Task Force SAFE%' OR repair_by LIKE '%Task Force Safe%' THEN 'TF SAFE'
                WHEN repair_by LIKE '%O&M%' OR repair_by LIKE '%O & M%' THEN 'O&M'
                WHEN repair_by LIKE '%Military%' THEN 'Military'
                ELSE 'Other'
            END as responsibility,
            COUNT(*) as total,
            SUM(CASE WHEN deficiency_status = 'Open' OR deficiency_status LIKE '%ReInsp-Open%' OR deficiency_status LIKE '%Reinsp-Open%' THEN 1 ELSE 0 END) as open_count,
            SUM(CASE WHEN deficiency_status LIKE '%Closed%' OR deficiency_status LIKE '%Repaired%' THEN 1 ELSE 0 END) as closed_count
        FROM master_deficiencies
        GROUP BY responsibility
        HAVING responsibility != 'Other'
        ORDER BY total DESC
    """
    cumulative_df = pd.read_sql_query(query, conn)
    
    if cumulative_df.empty:
        print("  No repair responsibility data to chart")
        return
    
    fig, axes = plt.subplots(1, 2, figsize=(16, 7))
    fig.patch.set_facecolor('white')
    
    # Left chart: Stacked bar by responsibility (Cumulative)
    ax1 = axes[0]
    ax1.set_facecolor('white')
    responsibilities = cumulative_df['responsibility'].tolist()
    x = range(len(responsibilities))
    
    # Use total for bar height, with closed/open as stacked segments
    # Calculate the "other" status count (total - open - closed)
    cumulative_df['other_count'] = cumulative_df['total'] - cumulative_df['open_count'] - cumulative_df['closed_count']
    
    bars_closed = ax1.bar(x, cumulative_df['closed_count'], label='Closed/Repaired', color='#27ae60')
    bars_open = ax1.bar(x, cumulative_df['open_count'], bottom=cumulative_df['closed_count'], 
                        label='Open', color='#e74c3c')
    # Add "other" statuses on top if any
    bars_other = ax1.bar(x, cumulative_df['other_count'], 
                         bottom=cumulative_df['closed_count'] + cumulative_df['open_count'],
                         label='Other Status', color='#95a5a6')
    
    ax1.set_xlabel('Repair Responsibility', fontsize=12, fontweight='bold')
    ax1.set_ylabel('Number of Deficiencies', fontsize=12, fontweight='bold')
    ax1.set_title('Deficiencies by Repair Responsibility\n(Cumulative)', fontsize=14, fontweight='bold', 
                  color=NAVY_BLUE, backgroundcolor='white', pad=20)
    ax1.set_xticks(x)
    ax1.set_xticklabels(responsibilities, fontsize=11, fontweight='bold')
    ax1.legend(loc='upper right', fontsize=10)
    ax1.yaxis.grid(True, linestyle='--', alpha=0.7)
    ax1.set_axisbelow(True)
    
    # Add total labels on top of bars
    for i, (idx, row) in enumerate(cumulative_df.iterrows()):
        total = row['total']
        ax1.annotate(f'{int(total):,}', xy=(i, total), xytext=(0, 5),
                    textcoords="offset points", ha='center', va='bottom', 
                    fontsize=11, fontweight='bold')
    
    # Right chart: Pie chart showing distribution
    ax2 = axes[1]
    ax2.set_facecolor('white')
    colors = ['#3498db', '#e67e22', '#9b59b6']
    
    # Pre-calculate percentages and use actual counts to avoid rounding errors
    totals = cumulative_df['total'].tolist()
    grand_total = sum(totals)
    
    def make_autopct(values):
        def autopct(pct):
            idx = int(round(pct/100 * len(values)))
            # Find the actual value based on the percentage
            for i, v in enumerate(values):
                if abs(v/grand_total*100 - pct) < 0.1:
                    return f'{pct:.1f}%\n({int(v):,})'
            return f'{pct:.1f}%'
        return autopct
    
    wedges, texts, autotexts = ax2.pie(totals, 
                                        labels=responsibilities,
                                        autopct=make_autopct(totals),
                                        colors=colors[:len(responsibilities)],
                                        explode=[0.02] * len(responsibilities),
                                        shadow=False,
                                        startangle=90)
    
    ax2.set_title('Distribution by Responsibility\n(Cumulative)', fontsize=14, fontweight='bold', 
                  color=NAVY_BLUE, backgroundcolor='white', pad=20)
    
    # Style the pie chart text
    for text in texts:
        text.set_fontsize(12)
        text.set_fontweight('bold')
    for autotext in autotexts:
        autotext.set_fontsize(10)
        autotext.set_fontweight('bold')
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight', facecolor='white')
    plt.close()
    print(f"  Generated: {output_path}")


def generate_weekly_report(start_date=None, end_date=None):
    """Generate all weekly report slides."""
    # Default to last week
    if not start_date:
        start_date = get_last_monday()
    if not end_date:
        end_date = get_last_saturday()
    
    print(f"\n{'='*60}")
    print(f"Generating Weekly Report")
    print(f"Period: {start_date} to {end_date}")
    print(f"{'='*60}\n")
    
    conn = get_db_connection()
    
    # Create output subdirectory for this report
    report_dir = os.path.join(OUTPUT_DIR, f"week_{start_date}_to_{end_date}")
    os.makedirs(report_dir, exist_ok=True)
    
    # Get data
    print("Fetching data...")
    weekly_stats = get_weekly_stats(conn, start_date, end_date)
    def_types = get_deficiency_types(conn, start_date, end_date)
    def_types_by_country = get_deficiency_types_by_country(conn, start_date, end_date)
    cumulative_stats = get_cumulative_stats(conn)
    cumulative_by_country = get_cumulative_by_country(conn)
    stats_90_day = get_90_day_stats(conn)
    combined_stats = get_combined_period_stats(conn, start_date, end_date)
    
    print(f"  Weekly records: {weekly_stats['total_deficiencies'].sum() if not weekly_stats.empty else 0}")
    print(f"  Countries with data: {len(weekly_stats)}")
    
    # Generate slides
    print("\nGenerating slides...")
    
    # Calculate 90-day date range (90 days back from end_date)
    end_date_obj = datetime.strptime(end_date, '%Y-%m-%d')
    day90_start = (end_date_obj - timedelta(days=90)).strftime('%Y-%m-%d')
    
    # 1. Operational Updates slide
    generate_operational_updates_slide(
        weekly_stats, start_date, end_date,
        os.path.join(report_dir, '01_operational_updates.png')
    )
    
    # 2. Theater Weekly Totals - Facilities Inspected table
    generate_facilities_inspected_table(
        conn, start_date, end_date,
        os.path.join(report_dir, '02_Facilities_Inspected.png')
    )
    
    # 3. Top 5 Hazards chart (Weekly, 90-Day, Cumulative bar charts)
    generate_top5_hazards_chart(
        conn, start_date, end_date,
        os.path.join(report_dir, '03_Top5_Hazards.png')
    )
    
    # 4. Weekly Inspection Data table by country/site
    generate_inspection_data_table(
        conn, start_date, end_date,
        os.path.join(report_dir, '04_Inspection_Data_Weekly.png'),
        period_label="Weekly"
    )
    
    # 5. 90-Day Inspection Data table by country/site
    generate_inspection_data_table(
        conn, day90_start, end_date,
        os.path.join(report_dir, '05_Inspection_Data_90Day.png'),
        period_label="90-Day"
    )
    
    # 6. Cumulative Inspection Data table by country/site (all time)
    generate_inspection_data_table(
        conn, None, end_date,
        os.path.join(report_dir, '06_Inspection_Data_Cumulative.png'),
        period_label="Cumulative"
    )
    
    # 7. Repair responsibility breakdown chart
    generate_repair_responsibility_chart(
        conn, start_date, end_date,
        os.path.join(report_dir, '07_repair_responsibility.png')
    )
    
    conn.close()
    
    print(f"\n{'='*60}")
    print(f"Report generated successfully!")
    print(f"Output directory: {report_dir}")
    print(f"{'='*60}\n")
    
    return report_dir


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generate weekly report slides')
    parser.add_argument('--start', '-s', help='Start date (YYYY-MM-DD)', default=None)
    parser.add_argument('--end', '-e', help='End date (YYYY-MM-DD)', default=None)
    
    args = parser.parse_args()
    
    generate_weekly_report(args.start, args.end)
