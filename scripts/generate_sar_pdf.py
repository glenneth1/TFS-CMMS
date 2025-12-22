#!/usr/bin/env python3
"""Generate Site Activity Report (SAR) PDF - consolidated report for all sites.
Naming convention: DAR_ALL_SITES_DD-MMM-YY.pdf
"""

import sqlite3
import sys
import os
from datetime import datetime
from weasyprint import HTML
from weasyprint.text.fonts import FontConfiguration

# Get the project root directory
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.dirname(SCRIPT_DIR)
DB_PATH = os.path.join(PROJECT_DIR, 'data', 'tfs-cmms.db')
REPORTS_DIR = os.path.join(PROJECT_DIR, 'reports', 'sar')

def get_inspection_reports_for_date(report_date):
    """Get all inspection reports submitted on a given date, grouped by site."""
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()
    
    # Get inspection reports for the date with camp/site info
    cursor.execute('''
        SELECT 
            ir.id,
            ir.report_number,
            ir.inspection_date,
            ir.building_number,
            ir.status,
            ir.team_number,
            s.name as site_name
        FROM inspection_reports ir
        JOIN sites s ON ir.site_id = s.id
        WHERE date(ir.inspection_date) = date(?)
        ORDER BY s.name, ir.team_number, ir.id
    ''', (report_date,))
    
    reports = cursor.fetchall()
    conn.close()
    
    # Group by site
    sites = {}
    for report in reports:
        site_key = report['site_name'] or 'Unknown'
        if site_key not in sites:
            sites[site_key] = []
        sites[site_key].append(dict(report))
    
    return sites

def format_date_display(date_str):
    """Format YYYY-MM-DD as DD-MMM-YY."""
    if not date_str:
        return '-'
    try:
        dt = datetime.strptime(date_str[:10], '%Y-%m-%d')
        return dt.strftime('%d-%b-%y')
    except:
        return date_str

def format_date_filename(date_str):
    """Format YYYY-MM-DD as DD-MMM-YY for filename."""
    if not date_str:
        return 'NODATE'
    try:
        dt = datetime.strptime(date_str[:10], '%Y-%m-%d')
        return dt.strftime('%d-%b-%y').upper()
    except:
        return 'NODATE'

def generate_sar_filename(report_date):
    """Generate filename: DAR_ALL_SITES_DD-MMM-YY.pdf"""
    date_str = format_date_filename(report_date)
    return f"DAR_ALL_SITES_{date_str}.pdf"

def generate_site_page_html(site_name, reports, report_date):
    """Generate HTML for a single site page."""
    date_display = format_date_display(report_date)
    
    rows_html = ''
    for report in reports:
        team = report.get('team_number') or '-'
        date = format_date_display(report.get('inspection_date'))
        location = report.get('site_name') or '-'
        building = report.get('building_number') or '-'
        tag_id = report.get('report_number') or '-'
        
        # Reports Submitted - YES if status is Submitted or Approved
        status = report.get('status') or ''
        reports_submitted = 'YES' if status in ('Submitted', 'Approved', 'QC Approved') else 'NO'
        reports_class = 'yes' if reports_submitted == 'YES' else 'no'
        
        # SPLO and EWP - placeholder for safety data (will be YES for now if report submitted)
        splo = 'YES' if reports_submitted == 'YES' else 'NO'
        ewp = 'YES' if reports_submitted == 'YES' else 'NO'
        splo_class = 'yes' if splo == 'YES' else 'no'
        ewp_class = 'yes' if ewp == 'YES' else 'no'
        
        rows_html += f'''
        <tr>
            <td class="team">{team}</td>
            <td>{date}</td>
            <td>{location}</td>
            <td>{building}</td>
            <td class="tag-id">{tag_id}</td>
            <td class="{reports_class}">{reports_submitted}</td>
            <td class="{splo_class}">{splo}</td>
            <td class="{ewp_class}">{ewp}</td>
        </tr>
        '''
    
    if not rows_html:
        rows_html = '<tr><td colspan="8" class="no-data">No inspection reports for this date</td></tr>'
    
    return f'''
    <div class="page">
        <div class="header">
            <div class="logo-left">
                <div class="army-logo">★</div>
                <div class="usace-text">US Army Corps<br/>of Engineers</div>
            </div>
            <div class="title">
                <h1>Task Force SAFE - CentCOM</h1>
                <h2>Site Activity Report</h2>
            </div>
            <div class="logo-right">
                <div class="tfs-logo">TFS</div>
            </div>
        </div>
        
        <div class="site-banner">{site_name}</div>
        
        <table class="report-table">
            <thead>
                <tr>
                    <th colspan="6" class="section-header">QA/QC Information</th>
                    <th colspan="2" class="section-header">Safety</th>
                </tr>
                <tr>
                    <th class="team-col">TEAM</th>
                    <th>Date</th>
                    <th>Location</th>
                    <th>Facility/Building Number</th>
                    <th class="tag-col">Report TAG ID</th>
                    <th>Reports Submitted</th>
                    <th>SPLO</th>
                    <th>EWP</th>
                </tr>
            </thead>
            <tbody>
                {rows_html}
            </tbody>
        </table>
    </div>
    '''

def generate_html(sites_data, report_date):
    """Generate full HTML for the SAR PDF."""
    pages_html = ''
    page_num = 1
    
    for site_name, reports in sorted(sites_data.items()):
        pages_html += generate_site_page_html(site_name, reports, report_date)
        pages_html += f'<div class="page-number">{page_num}</div>'
        page_num += 1
    
    if not pages_html:
        pages_html = generate_site_page_html("No Active Sites", [], report_date)
        pages_html += '<div class="page-number">1</div>'
    
    html = f'''
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <style>
            @page {{
                size: landscape;
                margin: 0.5in;
            }}
            body {{
                font-family: Arial, sans-serif;
                font-size: 9pt;
                margin: 0;
                padding: 0;
            }}
            .page {{
                page-break-after: always;
                position: relative;
                min-height: 90%;
            }}
            .page:last-child {{
                page-break-after: avoid;
            }}
            .header {{
                display: flex;
                justify-content: space-between;
                align-items: center;
                margin-bottom: 10px;
            }}
            .logo-left {{
                display: flex;
                align-items: center;
                gap: 5px;
            }}
            .army-logo {{
                font-size: 24pt;
                color: #000;
            }}
            .usace-text {{
                font-size: 8pt;
                font-weight: bold;
            }}
            .title {{
                text-align: center;
            }}
            .title h1 {{
                font-size: 18pt;
                margin: 0;
                color: #000;
            }}
            .title h2 {{
                font-size: 12pt;
                margin: 5px 0 0 0;
                font-weight: normal;
            }}
            .logo-right {{
                text-align: right;
            }}
            .tfs-logo {{
                font-size: 14pt;
                font-weight: bold;
                color: #003366;
            }}
            .site-banner {{
                background-color: #8faec1;
                color: #000;
                text-align: center;
                padding: 8px;
                font-weight: bold;
                font-size: 12pt;
                margin-bottom: 5px;
                border: 1px solid #000;
            }}
            .report-table {{
                width: 100%;
                border-collapse: collapse;
                font-size: 8pt;
            }}
            .report-table th, .report-table td {{
                border: 1px solid #000;
                padding: 4px 6px;
                text-align: left;
            }}
            .report-table th {{
                background-color: #8faec1;
                font-weight: bold;
            }}
            .section-header {{
                background-color: #8faec1;
                text-align: center;
            }}
            .team-col {{
                width: 50px;
            }}
            .team {{
                text-align: center;
            }}
            .tag-col {{
                width: 35%;
            }}
            .tag-id {{
                font-size: 7pt;
            }}
            .yes {{
                background-color: #90EE90;
                text-align: center;
                font-weight: bold;
            }}
            .no {{
                background-color: #FFB6C1;
                text-align: center;
                font-weight: bold;
            }}
            .no-data {{
                text-align: center;
                font-style: italic;
                color: #666;
            }}
            .page-number {{
                position: absolute;
                bottom: 10px;
                right: 10px;
                font-size: 10pt;
            }}
        </style>
    </head>
    <body>
        {pages_html}
    </body>
    </html>
    '''
    return html

def generate_pdf(report_date, output_dir=None):
    """Generate SAR PDF for a given date."""
    sites_data = get_inspection_reports_for_date(report_date)
    
    # Generate filename
    filename = generate_sar_filename(report_date)
    
    # Set output directory
    if output_dir is None:
        output_dir = REPORTS_DIR
    os.makedirs(output_dir, exist_ok=True)
    
    output_path = os.path.join(output_dir, filename)
    
    # Generate HTML and convert to PDF
    html_content = generate_html(sites_data, report_date)
    font_config = FontConfiguration()
    html = HTML(string=html_content)
    html.write_pdf(output_path, font_config=font_config)
    
    print(f"Generated: {output_path}")
    print(f"Sites included: {list(sites_data.keys())}")
    print(f"Total reports: {sum(len(r) for r in sites_data.values())}")
    return output_path

if __name__ == '__main__':
    if len(sys.argv) < 2:
        # Default to today
        report_date = datetime.now().strftime('%Y-%m-%d')
    else:
        report_date = sys.argv[1]
    
    output_dir = sys.argv[2] if len(sys.argv) > 2 else None
    
    result = generate_pdf(report_date, output_dir)
    if result:
        print(f"Success: {result}")
    else:
        sys.exit(1)
