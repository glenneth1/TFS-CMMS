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
    ax.text(1, 8.5, f'REDi – Week ({start_date} – {end_date})', 
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
    ax.set_title(f'Weekly Inspection Summary ({start_date} to {end_date})', fontsize=14, fontweight='bold')
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


def generate_deficiency_types_chart(def_types, start_date, end_date, output_path):
    """Generate pie chart of deficiency types."""
    if def_types.empty:
        print("  No deficiency type data to chart")
        return
    
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Truncate long category names
    def_types['category_short'] = def_types['category'].apply(
        lambda x: x[:25] + '...' if len(str(x)) > 25 else x
    )
    
    colors = sns.color_palette('husl', len(def_types))
    wedges, texts, autotexts = ax.pie(def_types['count'], 
                                       labels=def_types['category_short'],
                                       autopct='%1.1f%%',
                                       colors=colors,
                                       pctdistance=0.75)
    
    ax.set_title(f'Deficiency Types - Weekly ({start_date} to {end_date})', 
                 fontsize=14, fontweight='bold')
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
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
    ax.set_title(f'Deficiency Status Comparison\n(Week: {start_date} to {end_date})', 
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
    wedges, texts, autotexts = ax2.pie(cumulative_df['total'], 
                                        labels=responsibilities,
                                        autopct=lambda pct: f'{pct:.1f}%\n({int(pct/100*cumulative_df["total"].sum()):,})',
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
    cumulative_stats = get_cumulative_stats(conn)
    cumulative_by_country = get_cumulative_by_country(conn)
    stats_90_day = get_90_day_stats(conn)
    combined_stats = get_combined_period_stats(conn, start_date, end_date)
    
    print(f"  Weekly records: {weekly_stats['total_deficiencies'].sum() if not weekly_stats.empty else 0}")
    print(f"  Countries with data: {len(weekly_stats)}")
    
    # Generate slides
    print("\nGenerating slides...")
    
    # 1. Operational Updates slide
    generate_operational_updates_slide(
        weekly_stats, start_date, end_date,
        os.path.join(report_dir, '01_operational_updates.png')
    )
    
    # 2. Weekly by country chart
    generate_weekly_by_country_chart(
        weekly_stats, start_date, end_date,
        os.path.join(report_dir, '02_weekly_by_country.png')
    )
    
    # 3. Deficiency types pie chart
    generate_deficiency_types_chart(
        def_types, start_date, end_date,
        os.path.join(report_dir, '03_deficiency_types.png')
    )
    
    # 4. Cumulative stats
    generate_cumulative_stats_slide(
        cumulative_stats, cumulative_by_country,
        os.path.join(report_dir, '04_cumulative_stats.png')
    )
    
    # 5. 90-day chart
    generate_90_day_chart(
        stats_90_day,
        os.path.join(report_dir, '05_90_day_stats.png')
    )
    
    # 6. NEW: Combined deficiency chart (Cumulative, 90-Day, Weekly on one chart)
    generate_combined_deficiency_chart(
        combined_stats, start_date, end_date,
        os.path.join(report_dir, '06_combined_deficiency_comparison.png')
    )
    
    # 7. NEW: Repair responsibility breakdown chart
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
